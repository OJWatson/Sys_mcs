# Exact-record Drive recovery synchronization. Sourcing this file never connects
# to Google. Dry runs read remote state but cannot write it; apply is explicit.

recovery_empty <- function(x) is.null(x) || !length(x) || is.na(x[[1]]) || !nzchar(trimws(as.character(x[[1]])))
recovery_text <- function(x) if (recovery_empty(x)) "" else as.character(x[[1]])
recovery_true <- function(x) tolower(recovery_text(x)) %in% c("true", "1", "yes")
recovery_title <- function(x) {
  x <- gsub("<[^>]+>", " ", x)
  x <- gsub("&amp;", " and ", x, fixed = TRUE)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT", sub = "")
  trimws(gsub("[^a-z0-9]+", " ", tolower(x)))
}
recovery_require <- function(ok, message) if (!isTRUE(ok)) stop(message, call. = FALSE)
recovery_columns <- function(headers) {
  one <- function(name) {
    i <- which(headers == name)
    recovery_require(length(i) == 1L, paste0("Expected exactly one physical column named ", name, "."))
    i
  }
  links <- which(headers == "pdf_link")
  recovery_require(length(links) %in% 1:2, "Expected one or two physical pdf_link columns; repaired names are unsupported.")
  list(id = one("record_index"), title = one("title"), found = one("pdf_found"),
       links = links, status = one("screening_status"))
}
recovery_col_letter <- function(n) {
  result <- ""
  while (n > 0) { result <- paste0(LETTERS[(n - 1L) %% 26L + 1L], result); n <- (n - 1L) %/% 26L }
  result
}
recovery_sheet <- function(sheet) {
  sheet$headers <- as.character(unlist(sheet$headers, use.names = FALSE))
  recovery_require(length(sheet$headers) > 0, "Sheet has no header.")
  w <- length(sheet$headers)
  sheet$rows <- lapply(sheet$rows, function(row) {
    recovery_require(length(row) <= w, "Sheet data extends beyond its header; resolve the schema first.")
    c(unname(vapply(row, recovery_text, character(1))), rep("", w - length(row)))
  })
  sheet
}
recovery_files <- function(files) {
  required <- c("id", "name", "mime_type", "md5_checksum", "size", "folder_id")
  if (is.null(files) || !nrow(files)) return(as.data.frame(setNames(rep(list(character()), length(required)), required)))
  recovery_require(all(required %in% names(files)), paste("File inventory requires", paste(required, collapse = ", ")))
  for (name in required) files[[name]] <- as.character(files[[name]])
  recovery_require(!anyDuplicated(files$id), "Duplicate Drive IDs in folder inventory.")
  files
}
recovery_manifest <- function(manifest, default_source_folder = NULL) {
  required <- c("record_index", "title", "pdf_found", "pdf_filename", "source_file_id", "source_md5", "source_size", "pdf_verified")
  recovery_require(all(required %in% names(manifest)), paste("Manifest requires", paste(required, collapse = ", ")))
  recovery_require(nrow(manifest) > 0, "Manifest is empty.")
  for (name in names(manifest)) manifest[[name]] <- vapply(as.list(manifest[[name]]), recovery_text, character(1))
  if (!"source_folder_id" %in% names(manifest)) manifest$source_folder_id <- ""
  if (!"source_filename" %in% names(manifest)) manifest$source_filename <- manifest$pdf_filename
  manifest$source_filename[manifest$source_filename == ""] <- manifest$pdf_filename[manifest$source_filename == ""]
  if (!is.null(default_source_folder)) manifest$source_folder_id[manifest$source_folder_id == ""] <- default_source_folder
  recovery_require(all(grepl("^[1-9][0-9]*$", manifest$record_index)), "Manifest record IDs must be nonzero decimal integers without whitespace.")
  recovery_require(!anyDuplicated(manifest$record_index), "Manifest has duplicate record IDs.")
  recovery_require(all(nzchar(recovery_title(manifest$title))), "Manifest title missing.")
  recovery_require(all(tolower(manifest$pdf_found) %in% c("true", "false", "1", "0", "yes", "no")), "Manifest pdf_found must be explicit TRUE or FALSE.")
  manifest$pdf_found <- vapply(as.list(manifest$pdf_found), recovery_true, logical(1))
  manifest$pdf_verified <- vapply(as.list(manifest$pdf_verified), recovery_true, logical(1))
  found <- manifest$pdf_found
  recovery_require(all(manifest$pdf_verified[found]), "Every found PDF needs explicit identity/format verification bound to its checksum.")
  recovery_require(all(manifest$pdf_filename[found] == paste0(manifest$record_index[found], ".pdf")), "Destination filenames must equal record_index.pdf.")
  recovery_require(!anyDuplicated(manifest$pdf_filename[found]), "Duplicate destination filenames in manifest.")
  recovery_require(all(grepl("^[0-9a-fA-F]{32}$", manifest$source_md5[found])), "Found PDFs require valid source MD5 fingerprints.")
  recovery_require(all(!is.na(as.numeric(manifest$source_size[found])) & as.numeric(manifest$source_size[found]) > 0), "Found PDFs require positive source sizes.")
  recovery_require(all(nzchar(manifest$source_file_id[found]) & nzchar(manifest$source_folder_id[found]) & nzchar(manifest$source_filename[found])), "Found PDFs require source file ID, folder ID and filename.")
  manifest$source_md5 <- tolower(manifest$source_md5)
  manifest
}
recovery_assert_pdf <- function(file, expected_md5, expected_size, context) {
  recovery_require(nrow(file) == 1L, paste(context, "must identify exactly one file."))
  recovery_require(identical(file$mime_type[[1]], "application/pdf"), paste(context, "is not a PDF MIME type."))
  recovery_require(identical(tolower(file$md5_checksum[[1]]), tolower(expected_md5)), paste(context, "checksum differs from the verified source."))
  recovery_require(!is.na(as.numeric(file$size[[1]])) && as.numeric(file$size[[1]]) == as.numeric(expected_size), paste(context, "size differs from the verified source."))
}
recovery_link <- function(id) paste0("https://drive.google.com/file/d/", id, "/view")
recovery_canonical <- function(x) {
  if (!is.list(x)) return(x)
  if (!length(x)) return(list())
  if (!is.null(names(x))) x <- x[sort(names(x))]
  lapply(x, recovery_canonical)
}
# Optional sparse metadata map keyed by physical "row:column", including header.
# Preflight compares stored properties strictly; final readback also accounts
# for Google's automatic hyperlink on an exact newly written PDF URL.
recovery_protected_equal <- function(before, after, maximum_row = Inf) {
  if (is.null(before$protected_metadata) && is.null(after$protected_metadata)) return(TRUE)
  subset <- function(sheet) {
    metadata <- sheet$protected_metadata
    if (is.null(metadata)) metadata <- list()
    if (length(metadata)) metadata <- metadata[as.integer(sub(":.*$", "", names(metadata))) <= maximum_row]
    recovery_canonical(metadata)
  }
  identical(subset(before), subset(after))
}
# Sheets can add textFormat.link automatically when a URL or a URL-returning
# formula is written with fields=userEnteredValue. Permit only that exact new
# URI in the PDF cells changed by this plan; all prior metadata remains strict.
recovery_readback_matches <- function(before, after, plan) {
  before <- recovery_sheet(before); after <- recovery_sheet(after)
  expected <- recovery_expected_sheet(before, plan)
  expected_values <- expected; actual_values <- after
  expected_values$protected_metadata <- NULL; actual_values$protected_metadata <- NULL
  if (!recovery_same_sheet(expected_values, actual_values)) return(FALSE)
  cols <- recovery_columns(before$headers)
  normalized_after <- after
  pdf_changes <- plan$cells[plan$cells$column %in% cols$links, , drop = FALSE]
  for (i in seq_len(nrow(pdf_changes))) {
    row <- pdf_changes$row[[i]]; column <- pdf_changes$column[[i]]
    key <- paste(row, column, sep = ":")
    original <- before$protected_metadata[[key]]
    observed <- after$protected_metadata[[key]]
    if (identical(recovery_canonical(original), recovery_canonical(observed))) next
    target_url <- expected$rows[[row - 1L]][[cols$links[[1]]]]
    if (!grepl("^https://drive[.]google[.]com/file/d/[^/]+/view$", target_url) ||
        !recovery_empty(original$userEnteredFormat$textFormat$link)) return(FALSE)
    allowed <- if (is.null(original)) list() else original
    allowed$userEnteredFormat$textFormat$link <- list(uri = target_url)
    if (!identical(recovery_canonical(allowed), recovery_canonical(observed))) return(FALSE)
    # Remove only the allowed automatic addition for the full old-cell comparison.
    normalized_after$protected_metadata[[key]] <- original
  }
  if (!recovery_protected_equal(before, normalized_after, length(before$rows) + 1L)) return(FALSE)
  for (append in plan$append_rows) {
    target_url <- append$values[[cols$links[[1]]]]
    for (column in seq_along(before$headers)) {
      metadata <- after$protected_metadata[[paste(append$row, column, sep = ":")]]
      if (!recovery_empty(metadata$note) || length(metadata$textFormatRuns) || length(metadata$chipRuns)) return(FALSE)
      link <- metadata$userEnteredFormat$textFormat$link
      if (!recovery_empty(link)) {
        # Google may add the same correct hyperlink to the mirror formula too.
        if (!column %in% cols$links || !nzchar(target_url) ||
            !identical(recovery_canonical(link), list(uri = target_url))) return(FALSE)
      }
    }
  }
  TRUE
}

recovery_guard_target_metadata <- function(sheet, row, column, id) {
  key <- paste(row, column, sep = ":")
  metadata <- sheet$protected_metadata[[key]]
  if (is.null(metadata)) return(invisible(NULL))
  recovery_require(!length(metadata$textFormatRuns) && !length(metadata$chipRuns), paste("Target cell has rich text or smart chips; explicit review required for", id, "at", key))
  recovery_require(recovery_empty(metadata$userEnteredFormat$textFormat$link$uri), paste("Target cell has a formatting hyperlink; explicit review required for", id, "at", key))
  invisible(NULL)
}

# Pure planner. Missing no-DOI IDs are explicit exceptions, never appended.
# A validated shared abstract book may deliberately serve several record IDs.
recovery_sync_plan <- function(manifest, sheet, source_files, destination_files, workflow = c("no_doi", "country")) {
  workflow <- match.arg(workflow)
  manifest <- recovery_manifest(manifest)
  sheet <- recovery_sheet(sheet)
  cols <- recovery_columns(sheet$headers)
  sources <- recovery_files(source_files)
  destinations <- recovery_files(destination_files)
  ids <- vapply(sheet$rows, function(row) row[[cols$id]], character(1))
  nonblank <- vapply(sheet$rows, function(row) any(nzchar(row)), logical(1))
  recovery_require(all(nzchar(ids[nonblank])), "A nonempty production row lacks record_index; resolve it before synchronizing.")
  recovery_require(all(grepl("^[1-9][0-9]*$", ids[nzchar(ids)])), "Production record IDs must be nonzero decimal integers.")
  recovery_require(!anyDuplicated(ids[nzchar(ids)]), "Production sheet contains duplicate record IDs.")
  if (workflow == "no_doi") recovery_require(all(manifest$pdf_found), "The no-DOI sync manifest must contain only verified recovered PDFs.")
  files <- list(); cells <- list(); appends <- list(); exceptions <- list()
  add_cell <- function(row, column, id, before, after, type = "string") {
    if (!identical(before, after)) {
      recovery_guard_target_metadata(sheet, row, column, id)
      cells[[length(cells) + 1L]] <<- data.frame(row = row, column = column, record_index = id, before = before, after = after, type = type, stringsAsFactors = FALSE)
    }
  }
  for (i in seq_len(nrow(manifest))) {
    item <- manifest[i, , drop = FALSE]; id <- item$record_index[[1]]
    destination_id <- ""; destination_link <- ""; source_link <- ""
    if (item$pdf_found) {
      source <- sources[sources$id == item$source_file_id & sources$folder_id == item$source_folder_id, , drop = FALSE]
      named_source <- sources[sources$name == item$source_filename & sources$folder_id == item$source_folder_id, , drop = FALSE]
      recovery_require(nrow(named_source) == 1L, paste("Source filename missing or duplicated:", item$source_filename, "in", item$source_folder_id))
      recovery_require(nrow(source) == 1L && source$name[[1]] == item$source_filename, paste("Source ID/name/folder mismatch for", id))
      recovery_assert_pdf(source, item$source_md5, item$source_size, paste("Source PDF", id))
      destination <- destinations[destinations$name == item$pdf_filename, , drop = FALSE]
      recovery_require(nrow(destination) <= 1L, paste("Duplicate destination filename:", item$pdf_filename))
      action <- "copy"
      if (nrow(destination)) {
        recovery_assert_pdf(destination, item$source_md5, item$source_size, paste("Existing destination PDF", id))
        destination_id <- destination$id[[1]]; action <- "reuse"
      }
      destination_link <- if (nzchar(destination_id)) recovery_link(destination_id) else paste0("PENDING_COPY:", item$pdf_filename)
      source_link <- recovery_link(item$source_file_id)
      files[[length(files) + 1L]] <- data.frame(record_index = id, source_file_id = item$source_file_id, source_folder_id = item$source_folder_id,
        source_filename = item$source_filename, pdf_filename = item$pdf_filename, source_md5 = item$source_md5,
        source_size = item$source_size, destination_id = destination_id, action = action, stringsAsFactors = FALSE)
    }
    matched <- which(ids == id)
    if (!length(matched) && workflow == "no_doi") {
      exceptions[[length(exceptions) + 1L]] <- data.frame(record_index = id, title = item$title, reason = "record_absent_from_production_not_appended", stringsAsFactors = FALSE)
      next
    }
    if (length(matched)) {
      physical_row <- matched + 1L; row <- sheet$rows[[matched]]
      recovery_require(identical(recovery_title(row[[cols$title]]), recovery_title(item$title)), paste("Exact record title mismatch for", id, "at row", physical_row))
      if (!item$pdf_found) next # Never downgrade an existing record's PDF status.
      allowed_links <- c("", destination_link, source_link)
      for (j in seq_along(cols$links)) {
        current <- row[[cols$links[[j]]]]
        mirror <- paste0("=", recovery_col_letter(cols$links[[1]]), physical_row)
        array_mirror <- paste0("=ARRAYFORMULA(", recovery_col_letter(cols$links[[1]]), physical_row, ")")
        recovery_require(current %in% c(allowed_links, if (j == 2L) c(mirror, array_mirror)), paste("Conflicting existing PDF link for", id, "at", paste0(recovery_col_letter(cols$links[[j]]), physical_row)))
      }
      recovery_require(!startsWith(row[[cols$found]], "="), paste("Refusing to overwrite a pdf_found formula for", id))
      add_cell(physical_row, cols$found, id, row[[cols$found]], "TRUE", "boolean")
      add_cell(physical_row, cols$links[[1]], id, row[[cols$links[[1]]]], destination_link)
      if (length(cols$links) == 2L && !row[[cols$links[[2]]]] %in% c(paste0("=", recovery_col_letter(cols$links[[1]]), physical_row), paste0("=ARRAYFORMULA(", recovery_col_letter(cols$links[[1]]), physical_row, ")"))) add_cell(physical_row, cols$links[[2]], id, row[[cols$links[[2]]]], paste0("=", recovery_col_letter(cols$links[[1]]), physical_row), "formula")
      if (row[[cols$status]] %in% c("", "PDF missing")) add_cell(physical_row, cols$status, id, row[[cols$status]], "PDF uploaded")
    } else {
      physical_row <- length(sheet$rows) + length(appends) + 2L
      for (column in seq_along(sheet$headers)) {
        metadata <- sheet$protected_metadata[[paste(physical_row, column, sep = ":")]]
        recovery_require(recovery_empty(metadata$note), paste("Append destination contains an existing note at", paste(physical_row, column, sep = ":")))
        recovery_guard_target_metadata(sheet, physical_row, column, id)
      }
      row <- rep("", length(sheet$headers))
      bibliography <- c("record_index", "rec_number", "title", "journal_name", "year", "abstract", "doi")
      for (name in intersect(bibliography, names(item))) {
        positions <- which(sheet$headers == name)
        recovery_require(length(positions) <= 1L, paste("Ambiguous bibliography column", name))
        if (length(positions)) row[positions] <- item[[name]][[1]]
      }
      row[[cols$found]] <- if (item$pdf_found) "TRUE" else "FALSE"
      row[[cols$status]] <- if (item$pdf_found) "PDF uploaded" else "PDF missing"
      if (item$pdf_found) {
        row[[cols$links[[1]]]] <- destination_link
        if (length(cols$links) == 2L) row[[cols$links[[2]]]] <- paste0("=", recovery_col_letter(cols$links[[1]]), physical_row)
      }
      appends[[length(appends) + 1L]] <- list(row = physical_row, record_index = id, values = row)
    }
  }
  bind <- function(x, prototype) if (length(x)) do.call(rbind, x) else prototype
  files <- bind(files, data.frame(record_index = character(), source_file_id = character(), source_folder_id = character(), source_filename = character(), pdf_filename = character(), source_md5 = character(), source_size = character(), destination_id = character(), action = character()))
  cells <- bind(cells, data.frame(row = integer(), column = integer(), record_index = character(), before = character(), after = character(), type = character()))
  exceptions <- bind(exceptions, data.frame(record_index = character(), title = character(), reason = character()))
  list(workflow = workflow, files = files, cells = cells, append_rows = appends, exceptions = exceptions,
       summary = list(records = nrow(manifest), copies = sum(files$action == "copy"), reused = sum(files$action == "reuse"),
                      existing_records_updated = length(unique(cells$record_index)), cells_updated = nrow(cells),
                      rows_appended = length(appends), absent_exceptions = nrow(exceptions)))
}

recovery_cell_value <- function(value, type = "string") {
  if (type == "formula") list(userEnteredValue = list(formulaValue = value)) else
    if (type == "boolean") list(userEnteredValue = list(boolValue = recovery_true(value))) else
      list(userEnteredValue = list(stringValue = value))
}
# Existing-row requests target physical coordinates and userEnteredValue only.
# Google may add the matching URL's hyperlink formatting. Validation, notes,
# reviewer values and assignments in existing rows are untouched.
recovery_sheet_requests <- function(plan, sheet, allow_pending = FALSE) {
  sheet <- recovery_sheet(sheet); cols <- recovery_columns(sheet$headers)
  recovery_require(!is.null(sheet$sheet_id), "A numeric sheet tab ID is needed to build write requests.")
  recovery_require(allow_pending || !any(grepl("^PENDING_COPY:", c(plan$cells$after, unlist(lapply(plan$append_rows, `[[`, "values"))))), "Cannot write a plan with unresolved Drive copies.")
  sheet_id <- as.integer(sheet$sheet_id); requests <- list()
  add <- function(value) requests[[length(requests) + 1L]] <<- value
  if (length(plan$append_rows)) {
    first <- plan$append_rows[[1]]$row; last <- tail(plan$append_rows, 1)[[1]]$row
    grid_rows <- as.integer(sheet$grid_row_count)
    recovery_require(length(grid_rows) == 1L && !is.na(grid_rows), "Sheet grid row count is required for appends.")
    if (last > grid_rows) add(list(appendDimension = list(sheetId = sheet_id, dimension = "ROWS", length = last - grid_rows)))
    recovery_require(length(sheet$rows) > 0, "Country append requires an existing data row as formatting exemplar.")
    # Copy only formatting/validation, never contents or comments. Select the
    # last existing record rather than a trailing empty row.
    ids <- vapply(sheet$rows, function(row) row[[cols$id]], character(1))
    exemplar <- tail(which(nzchar(ids)), 1) + 1L
    source <- list(sheetId = sheet_id, startRowIndex = exemplar - 1L, endRowIndex = exemplar, startColumnIndex = 0L, endColumnIndex = length(sheet$headers))
    destination <- list(sheetId = sheet_id, startRowIndex = first - 1L, endRowIndex = last, startColumnIndex = 0L, endColumnIndex = length(sheet$headers))
    for (paste_type in c("PASTE_FORMAT", "PASTE_DATA_VALIDATION")) add(list(copyPaste = list(source = source, destination = destination, pasteType = paste_type, pasteOrientation = "NORMAL")))
    # PASTE_FORMAT can carry an old PDF URL in textFormat.link. Clear only that
    # property on new rows so the copied exemplar cannot attach another PDF.
    add(list(repeatCell = list(range = destination, cell = list(userEnteredFormat = list(textFormat = list(link = NULL))), fields = "userEnteredFormat.textFormat.link")))
    for (append in plan$append_rows) {
      values <- lapply(seq_along(append$values), function(j) recovery_cell_value(append$values[[j]],
        if (j == cols$found) "boolean" else if (j %in% cols$links[-1L] && startsWith(append$values[[j]], "=")) "formula" else "string"))
      add(list(updateCells = list(start = list(sheetId = sheet_id, rowIndex = append$row - 1L, columnIndex = 0L), rows = list(list(values = values)), fields = "userEnteredValue")))
    }
  }
  for (i in seq_len(nrow(plan$cells))) {
    cell <- plan$cells[i, , drop = FALSE]
    add(list(updateCells = list(start = list(sheetId = sheet_id, rowIndex = cell$row - 1L, columnIndex = cell$column - 1L),
      rows = list(list(values = list(recovery_cell_value(cell$after, cell$type)))), fields = "userEnteredValue")))
  }
  requests
}

recovery_same_sheet <- function(x, y) {
  x <- recovery_sheet(x); y <- recovery_sheet(y)
  identical(x$headers, y$headers) && identical(x$rows, y$rows) &&
    identical(as.integer(x$sheet_id), as.integer(y$sheet_id)) && identical(recovery_text(x$tab), recovery_text(y$tab)) &&
    recovery_protected_equal(x, y, min(length(x$rows), length(y$rows)) + 1L)
}
recovery_expected_sheet <- function(sheet, plan) {
  sheet <- recovery_sheet(sheet)
  for (i in seq_len(nrow(plan$cells))) sheet$rows[[plan$cells$row[[i]] - 1L]][[plan$cells$column[[i]]]] <- plan$cells$after[[i]]
  for (append in plan$append_rows) sheet$rows[[append$row - 1L]] <- append$values
  sheet
}

recovery_write_receipt <- function(receipt, plan, directory) {
  recovery_require(requireNamespace("jsonlite", quietly = TRUE), "Package jsonlite is required to persist synchronization receipts.")
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(receipt, file.path(directory, "receipt.json"), pretty = TRUE, auto_unbox = TRUE, null = "null", na = "null")
  if (!is.null(plan)) {
    jsonlite::write_json(plan, file.path(directory, "plan.json"), pretty = TRUE, auto_unbox = TRUE, null = "null", na = "null")
    utils::write.csv(plan$files, file.path(directory, "files.csv"), row.names = FALSE, na = "")
    utils::write.csv(plan$cells, file.path(directory, "cells.csv"), row.names = FALSE, na = "")
    utils::write.csv(plan$exceptions, file.path(directory, "exceptions.csv"), row.names = FALSE, na = "")
  }
}

# Adapter methods: read_sheet(), list_files(folder_id, name=NULL),
# copy_file(source_id, destination_folder_id, filename), write_plan(plan,sheet).
# Every apply refreshes the full sheet and both folder inventories before writes.
recovery_sync_run <- function(manifest, config, adapter = NULL, apply = FALSE) {
  required <- c("workflow", "sheet_id", "sheet_tab", "source_folder_id", "destination_folder_id", "receipt_dir")
  recovery_require(all(required %in% names(config)) && all(vapply(config[required], function(x) !recovery_empty(x), logical(1))), "Explicit workflow, sheet, tab, source/destination folders and receipt directory are required.")
  recovery_require(!identical(config$source_folder_id, config$destination_folder_id), "Source and destination folders must differ.")
  recovery_require(is.logical(apply) && length(apply) == 1L && !is.na(apply), "apply must be an explicit logical value.")
  manifest <- recovery_manifest(manifest, config$source_folder_id)
  recovery_require(!any(manifest$source_folder_id[manifest$pdf_found] == config$destination_folder_id), "A source PDF folder equals the destination folder.")
  if (is.null(adapter)) adapter <- recovery_google_adapter(config)
  run_id <- paste0(format(Sys.time(), "%Y%m%dT%H%M%S", tz = "UTC"), "Z-", Sys.getpid(), "-", sprintf("%06d", sample.int(999999, 1)))
  directory <- file.path(config$receipt_dir, run_id)
  receipt <- list(run_id = run_id, started_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    mode = if (apply) "apply" else "dry_run", status = "started", configuration = config, copies = list(), copy_attempts = list(), sheet_write_attempted = FALSE)
  plan <- NULL
  save <- function() recovery_write_receipt(receipt, plan, directory)
  save()
  read_sources <- function() {
    folders <- unique(manifest$source_folder_id[manifest$pdf_found])
    if (!length(folders)) return(recovery_files(NULL))
    inventory <- do.call(rbind, lapply(folders, function(folder) adapter$list_files(folder)))
    # A file in multiple explicitly selected folders is represented once per
    # parent. Rejecting duplicate file IDs is conservative and prevents ambiguity.
    recovery_files(inventory)
  }
  result <- tryCatch({
    sheet <- recovery_sheet(adapter$read_sheet())
    sources <- read_sources(); destinations <- recovery_files(adapter$list_files(config$destination_folder_id))
    plan <- recovery_sync_plan(manifest, sheet, sources, destinations, config$workflow)
    receipt$summary <- plan$summary; receipt$status <- "planned"; save()
    if (!apply) {
      receipt$status <- "dry_run_complete"; save()
      list(plan = plan, receipt = receipt, receipt_dir = directory)
    } else {
      # Fresh preflight after persisted reviewable plan, before the first mutation.
      fresh_sheet <- recovery_sheet(adapter$read_sheet())
      recovery_require(recovery_same_sheet(sheet, fresh_sheet), "Sheet changed during preflight. No sheet writes made; rerun to review current state.")
      fresh_sources <- read_sources(); fresh_destinations <- recovery_files(adapter$list_files(config$destination_folder_id))
      plan <- recovery_sync_plan(manifest, fresh_sheet, fresh_sources, fresh_destinations, config$workflow); save()
      for (i in seq_len(nrow(plan$files))) {
        file <- plan$files[i, , drop = FALSE]
        # Check this exact name again immediately before a potential copy. This
        # closes ordinary rerun/timeout races; operators must not run concurrently.
        existing <- recovery_files(adapter$list_files(config$destination_folder_id, file$pdf_filename))
        existing <- existing[existing$name == file$pdf_filename, , drop = FALSE]
        recovery_require(nrow(existing) <= 1L, paste("Duplicate destination filename during apply:", file$pdf_filename))
        if (nrow(existing)) {
          recovery_assert_pdf(existing, file$source_md5, file$source_size, paste("Destination", file$record_index))
          copied_id <- existing$id[[1]]; action <- "reused"
        } else {
          source_now <- recovery_files(adapter$list_files(file$source_folder_id, file$source_filename))
          source_now <- source_now[source_now$name == file$source_filename, , drop = FALSE]
          recovery_assert_pdf(source_now, file$source_md5, file$source_size, paste("Source before copy", file$record_index))
          recovery_require(source_now$id[[1]] == file$source_file_id, "Source identity changed immediately before copy.")
          receipt$copy_attempts[[length(receipt$copy_attempts) + 1L]] <- list(record_index = file$record_index, source_file_id = file$source_file_id, destination_folder_id = config$destination_folder_id, pdf_filename = file$pdf_filename)
          receipt$status <- "copy_started"; save()
          adapter$copy_file(file$source_file_id, config$destination_folder_id, file$pdf_filename)
          after <- recovery_files(adapter$list_files(config$destination_folder_id, file$pdf_filename))
          after <- after[after$name == file$pdf_filename, , drop = FALSE]
          recovery_assert_pdf(after, file$source_md5, file$source_size, paste("Copied PDF readback", file$record_index))
          copied_id <- after$id[[1]]; action <- "copied"
        }
        receipt$copies[[length(receipt$copies) + 1L]] <- list(record_index = file$record_index, source_file_id = file$source_file_id, destination_file_id = copied_id, action = action, md5 = file$source_md5)
        receipt$status <- "copying"; save()
      }
      before_write <- recovery_sheet(adapter$read_sheet())
      recovery_require(recovery_same_sheet(fresh_sheet, before_write), "Sheet changed while copying PDFs. Copied files retained; no sheet writes made. Rerun safely.")
      plan <- recovery_sync_plan(manifest, before_write, read_sources(), recovery_files(adapter$list_files(config$destination_folder_id)), config$workflow)
      recovery_require(!any(plan$files$action == "copy"), "A destination PDF is missing at final preflight.")
      receipt$summary <- plan$summary; receipt$status <- "ready_to_write"; save()
      if (nrow(plan$cells) || length(plan$append_rows)) {
        receipt$sheet_write_attempted <- TRUE; receipt$status <- "sheet_write_started"; save()
        # The adapter sends all exact-cell updates/appends in one atomic Sheets
        # batchUpdate request. Never automatically retry this mutation.
        write_error <- tryCatch({ adapter$write_plan(plan, before_write); NULL }, error = function(e) conditionMessage(e))
        after_write <- recovery_sheet(adapter$read_sheet())
        recovery_require(recovery_readback_matches(before_write, after_write, plan), paste("Sheet readback did not match the exact planned change. Inspect receipt before retrying.", if (!is.null(write_error)) write_error else ""))
        if (!is.null(write_error)) receipt$write_warning <- paste("Write response failed but complete exact readback confirmed success:", write_error)
      }
      receipt$status <- "applied_and_verified"; save()
      list(plan = plan, receipt = receipt, receipt_dir = directory)
    }
  }, error = function(e) {
    receipt$status <<- if (length(receipt$copy_attempts) || length(receipt$copies) || receipt$sheet_write_attempted) "partial_or_uncertain_failure" else "failed_before_confirmed_changes"
    receipt$error <<- conditionMessage(e); save()
    stop(paste0(conditionMessage(e), "\nReceipt: ", file.path(directory, "receipt.json")), call. = FALSE)
  })
  result
}

# Runtime-only Google adapter. No permissions/sharing calls are made. drive_ls
# paginates with n_max=Inf; no first-page/1000-file assumption is used.
recovery_google_adapter <- function(config) {
  for (package in c("googledrive", "googlesheets4", "httr", "jsonlite")) recovery_require(requireNamespace(package, quietly = TRUE), paste("Install runtime package", package, "before using the Google adapter."))
  googledrive::drive_auth(); googlesheets4::gs4_auth()
  request <- function(method, url, body = NULL, query = NULL) {
    args <- list(verb = method, url = url, config = googlesheets4::gs4_token())
    if (!is.null(body)) { args$body <- body; args$encode <- "json" }
    if (!is.null(query)) args$query <- query
    response <- do.call(httr::VERB, args)
    httr::stop_for_status(response)
    jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
  }
  root <- paste0("https://sheets.googleapis.com/v4/spreadsheets/", utils::URLencode(config$sheet_id, reserved = TRUE))
  list(
    read_sheet = function() {
      range <- paste0("'", gsub("'", "''", config$sheet_tab, fixed = TRUE), "'")
      metadata <- request("GET", root, query = list(ranges = range, includeGridData = TRUE,
        fields = "sheets(properties,data(startRow,startColumn,rowData(values(note,dataValidation,userEnteredFormat,textFormatRuns,chipRuns))))"))
      tabs <- Filter(function(tab) identical(tab$properties$title, config$sheet_tab), metadata$sheets)
      recovery_require(length(tabs) == 1L, "Configured sheet tab does not exist uniquely.")
      tab <- tabs[[1]]$properties
      range <- paste0("'", gsub("'", "''", config$sheet_tab, fixed = TRUE), "'")
      data <- request("GET", paste0(root, "/values/", utils::URLencode(range, reserved = TRUE)), query = list(valueRenderOption = "FORMULA", dateTimeRenderOption = "SERIAL_NUMBER"))
      recovery_require(length(data$values) > 0, "Configured sheet tab is empty.")
      # A production sheet can have tens of thousands of formatted cells.
      # Numeric preallocation avoids quadratic named-list insertion on every read.
      count <- sum(vapply(tabs[[1]]$data, function(block) sum(vapply(block$rowData,
        function(row) length(row$values), integer(1))), integer(1)))
      protected <- vector("list", count); keys <- character(count); used <- 0L
      for (block in tabs[[1]]$data) {
        start_row <- if (is.null(block$startRow)) 0L else as.integer(block$startRow)
        start_col <- if (is.null(block$startColumn)) 0L else as.integer(block$startColumn)
        for (i in seq_along(block$rowData)) for (j in seq_along(block$rowData[[i]]$values)) {
          cell <- block$rowData[[i]]$values[[j]]
          if (length(cell)) {
            used <- used + 1L; protected[[used]] <- cell
            keys[[used]] <- paste(start_row + i, start_col + j, sep = ":")
          }
        }
      }
      protected <- setNames(protected[seq_len(used)], keys[seq_len(used)])
      recovery_sheet(list(headers = data$values[[1]], rows = data$values[-1], sheet_id = tab$sheetId,
        grid_row_count = tab$gridProperties$rowCount, grid_column_count = tab$gridProperties$columnCount, tab = config$sheet_tab,
        protected_metadata = protected))
    },
    list_files = function(folder_id, name = NULL) {
      args <- list(path = googledrive::as_id(folder_id), n_max = Inf, fields = "nextPageToken,files(id,name,mimeType,md5Checksum,size,parents,trashed)")
      if (!is.null(name)) {
        escaped <- gsub("\\", "\\\\", name, fixed = TRUE); escaped <- gsub("'", "\\'", escaped, fixed = TRUE)
        args$q <- paste0("name = '", escaped, "'")
      }
      files <- do.call(googledrive::drive_ls, args)
      if (!nrow(files)) return(recovery_files(NULL))
      rows <- lapply(seq_len(nrow(files)), function(i) {
        resource <- files$drive_resource[[i]]
        data.frame(id = as.character(files$id[[i]]), name = files$name[[i]], mime_type = recovery_text(resource$mimeType),
          md5_checksum = recovery_text(resource$md5Checksum), size = recovery_text(resource$size), folder_id = folder_id, stringsAsFactors = FALSE)
      })
      recovery_files(do.call(rbind, rows))
    },
    copy_file = function(source_id, destination_folder_id, filename) {
      googledrive::drive_cp(file = googledrive::as_id(source_id), path = googledrive::as_id(destination_folder_id), name = filename, overwrite = FALSE)
    },
    write_plan = function(plan, sheet) {
      requests <- recovery_sheet_requests(plan, sheet)
      if (length(requests)) request("POST", paste0(root, ":batchUpdate"), body = list(requests = requests, includeSpreadsheetInResponse = FALSE))
      invisible(NULL)
    }
  )
}
