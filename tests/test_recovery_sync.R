#!/usr/bin/env Rscript
# Offline tests; no test framework, auth or network required.
source("R/recovery_sync_helpers.R")
checks <- 0L
check <- function(value, message) { checks <<- checks + 1L; if (!isTRUE(value)) stop(message, call. = FALSE) }
fails <- function(expr, pattern) {
  message <- tryCatch({ force(expr); "NO ERROR" }, error = function(e) conditionMessage(e))
  check(grepl(pattern, message), paste("Expected failure", pattern, "but received:", message))
}
headers <- c("record_index", "rec_number", "title", "journal_name", "year", "abstract", "doi", "pdf_found", "pdf_link", "pdf_link", "screening_status", "Assigned", "OJ_include", "OJ_comments")
row <- function(id, title, status = "PDF missing") c(id, paste0(id, "0"), title, "Journal", "2001", "Abstract", "", "", "", "", status, "Reviewer", "Exclude", "Keep this comment = exactly")
sheet <- list(headers = headers, rows = list(row("1", "First title"), row("99", "Unrelated title", "Completed")), sheet_id = 123L, grid_row_count = 3L, grid_column_count = length(headers), tab = "studies")
manifest <- data.frame(record_index = c("1", "2"), title = c("First title", "Second title"), pdf_found = TRUE,
  pdf_filename = c("1.pdf", "2.pdf"), source_file_id = c("source1", "source2"), source_md5 = c(strrep("a", 32), strrep("b", 32)),
  source_size = c("100", "200"), pdf_verified = TRUE, source_folder_id = "source", source_filename = c("1.pdf", "2.pdf"), stringsAsFactors = FALSE)
sources <- data.frame(id = manifest$source_file_id, name = manifest$source_filename, mime_type = "application/pdf", md5_checksum = manifest$source_md5, size = manifest$source_size, folder_id = "source", stringsAsFactors = FALSE)
empty_files <- recovery_files(NULL)
destinations <- sources; destinations$id <- c("dest1", "dest2"); destinations$folder_id <- "destination"
plan <- recovery_sync_plan(manifest, sheet, sources, empty_files, "no_doi")
check(plan$summary$copies == 2L && plan$summary$existing_records_updated == 1L && plan$summary$absent_exceptions == 1L, "No-DOI plan count mismatch")
check(length(plan$append_rows) == 0 && identical(plan$exceptions$record_index, "2"), "Absent no-DOI record was appended")
check(identical(plan$cells$column, c(8L, 9L, 10L, 11L)), "Duplicate link columns not addressed physically")
fails(recovery_sheet_requests(plan, sheet), "unresolved Drive copies")
resolved <- recovery_sync_plan(manifest, sheet, sources, destinations, "no_doi")
expected <- recovery_expected_sheet(sheet, resolved)
check(identical(expected$rows[[1]][12:14], unname(sheet$rows[[1]][12:14])) && identical(expected$rows[[2]], unname(sheet$rows[[2]])), "Protected screening fields changed")
check(expected$rows[[1]][[10]] == "=I2", "Second pdf_link is not a same-row mirror")
again <- recovery_sync_plan(manifest, expected, sources, destinations, "no_doi")
check(nrow(again$cells) == 0 && all(again$files$action == "reuse"), "Rerun is not idempotent")
custom <- sheet; custom$rows[[1]][[11]] <- "In full-text review"
check(!11L %in% recovery_sync_plan(manifest, custom, sources, destinations, "no_doi")$cells$column, "Custom screening status overwritten")
array <- sheet; array$rows[[1]][[10]] <- "=ARRAYFORMULA(I2)"
check(!10L %in% recovery_sync_plan(manifest, array, sources, destinations, "no_doi")$cells$column, "Correct existing ARRAYFORMULA mirror changed")
for (which in c("id", "title", "link", "found_formula")) {
  bad <- sheet
  if (which == "id") bad$rows[[2]][[1]] <- "1"
  if (which == "title") bad$rows[[1]][[3]] <- "Wrong paper"
  if (which == "link") bad$rows[[1]][[9]] <- "https://drive.google.com/file/d/wrong/view"
  if (which == "found_formula") bad$rows[[1]][[8]] <- "=TRUE"
  fails(recovery_sync_plan(manifest, bad, sources, destinations, "no_doi"), switch(which, id = "duplicate record IDs", title = "title mismatch", link = "Conflicting existing PDF link", found_formula = "pdf_found formula"))
}
bad <- destinations; bad$md5_checksum[1] <- strrep("c", 32)
fails(recovery_sync_plan(manifest, sheet, sources, bad, "no_doi"), "checksum differs")
bad <- sources; bad$mime_type[1] <- "text/html"
fails(recovery_sync_plan(manifest, sheet, bad, empty_files, "no_doi"), "not a PDF MIME")
bad <- sources; bad$id[2] <- "extra"; bad$name[2] <- "1.pdf"
fails(recovery_sync_plan(manifest, sheet, bad, empty_files, "no_doi"), "Source filename missing or duplicated")
bad <- destinations; bad$id <- c("d1", "d2"); bad$name <- "1.pdf"
fails(recovery_sync_plan(manifest, sheet, sources, bad, "no_doi"), "Duplicate destination filename")
bad <- manifest; bad$pdf_verified[1] <- FALSE
fails(recovery_sync_plan(bad, sheet, sources, destinations, "no_doi"), "explicit identity/format verification")
bad <- manifest; bad$source_md5[1] <- "wrong"
fails(recovery_sync_plan(bad, sheet, sources, destinations, "no_doi"), "valid source MD5")
shared <- manifest; shared[2, c("source_file_id", "source_filename", "source_md5", "source_size")] <- shared[1, c("source_file_id", "source_filename", "source_md5", "source_size")]
check(recovery_sync_plan(shared, sheet, sources[1, ], empty_files, "no_doi")$summary$copies == 2, "Curated shared abstract book rejected")
renamed <- manifest[1, ]; renamed$source_filename <- "old_record.pdf"; renamed_sources <- sources[1, ]; renamed_sources$name <- "old_record.pdf"
check(recovery_sync_plan(renamed, sheet, renamed_sources, empty_files, "country")$files$pdf_filename == "1.pdf", "Explicit source-to-destination rename lost")
missing <- manifest[1, ]; missing$record_index <- "3"; missing$title <- "Third title"; missing$pdf_found <- FALSE; missing$pdf_verified <- FALSE
missing[, c("pdf_filename", "source_file_id", "source_md5", "source_size", "source_filename")] <- ""
country_manifest <- rbind(manifest, missing)
country <- recovery_sync_plan(country_manifest, sheet, sources, destinations, "country")
check(length(country$append_rows) == 2, "Country missing records not appended")
check(all(country$append_rows[[1]]$values[12:14] == ""), "Country append copied review decisions/comments")
check(country$append_rows[[2]]$values[[8]] == "FALSE" && country$append_rows[[2]]$values[[11]] == "PDF missing" && all(country$append_rows[[2]]$values[9:10] == ""), "Missing PDF country row wrongly marked found")
requests <- recovery_sheet_requests(country, sheet)
check(requests[[1]]$appendDimension$length == 2, "Grid extension incorrect")
check(identical(vapply(requests[2:3], function(x) x$copyPaste$pasteType, character(1)), c("PASTE_FORMAT", "PASTE_DATA_VALIDATION")), "New-row formatting and validation not copied safely")
check(requests[[4]]$repeatCell$fields == "userEnteredFormat.textFormat.link", "Exemplar hyperlink not cleared")
json <- jsonlite::toJSON(requests, auto_unbox = TRUE, null = "null")
check(grepl('"stringValue":""', json, fixed = TRUE), "Blank cells serialized as malformed arrays")
check(!grepl('PASTE_NORMAL', json, fixed = TRUE) && !grepl('"fields":"*"', json, fixed = TRUE), "Requests include unsafe broad field copies")
country_expected <- recovery_expected_sheet(sheet, country)
country_again <- recovery_sync_plan(country_manifest, country_expected, sources, destinations, "country")
check(length(country_again$append_rows) == 0 && nrow(country_again$cells) == 0, "Country rerun is not idempotent")
# Real Sheets readback adds exact automatic URL metadata even for an update
# restricted to userEnteredValue. This is allowed only at planned PDF cells.
metadata_before <- sheet
metadata_before$protected_metadata <- list(`2:9` = list(note = "Keep this note",
  userEnteredFormat = list(textFormat = list(bold = TRUE)), dataValidation = list(strict = TRUE)))
metadata_plan <- recovery_sync_plan(manifest, metadata_before, sources, destinations, "no_doi")
metadata_after <- recovery_expected_sheet(metadata_before, metadata_plan)
metadata_after$protected_metadata[["2:9"]]$userEnteredFormat$textFormat$link <- list(uri = recovery_link("dest1"))
check(recovery_readback_matches(metadata_before, metadata_after, metadata_plan), "Exact automatic PDF hyperlink was rejected")
check(!recovery_same_sheet(metadata_before, metadata_after), "Plan-aware readback weakened strict preflight comparison")
metadata_after$protected_metadata[["2:10"]] <- list(userEnteredFormat = list(textFormat = list(link = list(uri = recovery_link("dest1")))))
check(recovery_readback_matches(metadata_before, metadata_after, metadata_plan), "Correct automatic mirror hyperlink was rejected")
bad_metadata <- metadata_after; bad_metadata$protected_metadata[["2:9"]]$userEnteredFormat$textFormat$link$uri <- recovery_link("wrong")
check(!recovery_readback_matches(metadata_before, bad_metadata, metadata_plan), "Wrong automatically added PDF hyperlink was accepted")
bad_metadata <- metadata_after; bad_metadata$protected_metadata[["2:9"]]$note <- "Changed note"
check(!recovery_readback_matches(metadata_before, bad_metadata, metadata_plan), "Automatic hyperlink exception concealed a changed note")
bad_metadata <- metadata_after; bad_metadata$protected_metadata[["2:9"]]$userEnteredFormat$textFormat$bold <- FALSE
check(!recovery_readback_matches(metadata_before, bad_metadata, metadata_plan), "Automatic hyperlink exception concealed a formatting change")
bad_metadata <- metadata_after; bad_metadata$protected_metadata[["2:12"]] <- list(userEnteredFormat = list(textFormat = list(link = list(uri = recovery_link("dest1")))))
check(!recovery_readback_matches(metadata_before, bad_metadata, metadata_plan), "An extra hyperlink outside planned PDF cells was accepted")
append_metadata <- country_expected
append_metadata$protected_metadata <- list(`4:9` = list(userEnteredFormat = list(textFormat = list(link = list(uri = recovery_link("dest2"))))),
  `4:10` = list(userEnteredFormat = list(textFormat = list(link = list(uri = recovery_link("dest2"))))))
check(recovery_readback_matches(sheet, append_metadata, country), "Correct automatic I/J links on a new country row were rejected")
bad_metadata <- append_metadata; bad_metadata$protected_metadata[["4:10"]]$userEnteredFormat$textFormat$link$uri <- recovery_link("old-exemplar")
check(!recovery_readback_matches(sheet, bad_metadata, country), "Inherited wrong country mirror link was accepted")
bad_metadata <- append_metadata; bad_metadata$protected_metadata[["5:9"]] <- append_metadata$protected_metadata[["4:9"]]
check(!recovery_readback_matches(sheet, bad_metadata, country), "Missing-PDF country row acquired an unwanted hyperlink")
bad_metadata <- append_metadata; bad_metadata$protected_metadata[["4:14"]] <- list(note = "Unexpected inherited note")
check(!recovery_readback_matches(sheet, bad_metadata, country), "New country row inherited an unexpected note")
# Freshly reread plain values must compare equal to planned rows (no value-derived names).
check(recovery_same_sheet(country_expected, recovery_sheet(country_expected)), "Readback normalization changes row names")

make_adapter <- function(change_sheet_on_read = Inf, write_timeout = FALSE) {
  state <- new.env(parent = emptyenv()); state$sheet <- sheet; state$destinations <- empty_files
  state$copies <- 0L; state$writes <- 0L; state$reads <- 0L
  adapter <- list(
    read_sheet = function() {
      state$reads <- state$reads + 1L
      if (state$reads == change_sheet_on_read) state$sheet$rows[[1]][[14]] <- "Concurrent user edit"
      state$sheet
    },
    list_files = function(folder_id, name = NULL) {
      files <- if (folder_id == "source") sources else state$destinations
      if (!is.null(name)) files <- files[files$name == name, , drop = FALSE]
      files
    },
    copy_file = function(source_id, destination_folder_id, filename) {
      file <- sources[sources$id == source_id, , drop = FALSE]; file$id <- paste0("destination_", source_id); file$name <- filename; file$folder_id <- destination_folder_id
      state$destinations <- rbind(state$destinations, file); state$copies <- state$copies + 1L
    },
    write_plan = function(plan, before) {
      state$writes <- state$writes + 1L; state$sheet <- recovery_expected_sheet(state$sheet, plan)
      if (write_timeout) stop("Simulated timeout after server commit")
    })
  list(adapter = adapter, state = state)
}
receipt_dir <- tempfile("recovery-tests-"); dir.create(receipt_dir)
on.exit <- NULL
config <- list(workflow = "no_doi", sheet_id = "test_sheet", sheet_tab = "studies", source_folder_id = "source", destination_folder_id = "destination", receipt_dir = receipt_dir)
fake <- make_adapter(); dry <- recovery_sync_run(manifest, config, fake$adapter)
check(fake$state$copies == 0 && fake$state$writes == 0 && dry$receipt$status == "dry_run_complete", "Dry run mutated remote state")
fake <- make_adapter(); applied <- recovery_sync_run(manifest, config, fake$adapter, apply = TRUE)
check(fake$state$copies == 2 && fake$state$writes == 1 && applied$receipt$status == "applied_and_verified", "Apply failed to copy all approved PDFs then exact-write")
rerun <- recovery_sync_run(manifest, config, fake$adapter, apply = TRUE)
check(fake$state$copies == 2 && fake$state$writes == 1, "Apply rerun duplicated copies or writes")
check(file.exists(file.path(applied$receipt_dir, "receipt.json")) && file.exists(file.path(applied$receipt_dir, "cells.csv")), "Receipt files not persisted")
fake <- make_adapter(change_sheet_on_read = 2)
fails(recovery_sync_run(manifest, config, fake$adapter, apply = TRUE), "Sheet changed during preflight")
check(fake$state$copies == 0 && fake$state$writes == 0, "Preflight drift did not prevent mutations")
fake <- make_adapter(change_sheet_on_read = 3)
fails(recovery_sync_run(manifest, config, fake$adapter, apply = TRUE), "Sheet changed while copying")
check(fake$state$copies == 2 && fake$state$writes == 0, "Late drift did not preserve sheet while retaining safe copies")
fake <- make_adapter(write_timeout = TRUE)
recovered <- recovery_sync_run(manifest, config, fake$adapter, apply = TRUE)
check(recovered$receipt$status == "applied_and_verified" && nzchar(recovered$receipt$write_warning), "Ambiguous write success was not resolved by readback")
fake <- make_adapter()
original_write <- fake$adapter$write_plan
fake$adapter$write_plan <- function(plan, before) {
  original_write(plan, before)
  fake$state$sheet$protected_metadata <- list(`2:9` = list(userEnteredFormat = list(textFormat = list(link = list(uri = recovery_link("destination_source1"))))))
}
autolink_run <- recovery_sync_run(manifest, config, fake$adapter, apply = TRUE)
check(autolink_run$receipt$status == "applied_and_verified", "Apply rejected the exact real-shaped automatic URL metadata")
unlink(receipt_dir, recursive = TRUE)
cat(sprintf("Passed %d recovery synchronization assertions (offline).\n", checks))
