#!/usr/bin/env Rscript
# Offline public-API contract test. Runtime packages are optional for this test;
# all Google/auth/HTTP entry points are mocked, and no network request is made.
source("R/recovery_sync_helpers.R")
packages <- c("googledrive", "googlesheets4", "httr", "jsonlite")
if (!all(vapply(packages, requireNamespace, logical(1), quietly = TRUE))) {
  cat("SKIP Google adapter contract test: install runtime packages or set R_LIBS.\n")
  quit(status = 0L)
}
checks <- 0L
check <- function(ok, message) { checks <<- checks + 1L; if (!isTRUE(ok)) stop(message, call. = FALSE) }
originals <- list()
mock <- function(package, name, value) {
  key <- paste(package, name, sep = "::")
  originals[[key]] <<- list(package = package, name = name, value = get(name, asNamespace(package)))
  assignInNamespace(name, value, ns = package)
}
restore <- function() for (entry in originals) assignInNamespace(entry$name, entry$value, ns = entry$package)
tryCatch({
  state <- new.env(); state$requests <- list(); state$copies <- list(); state$list_calls <- list()
  mock("googledrive", "drive_auth", function(...) invisible(NULL))
  mock("googlesheets4", "gs4_auth", function(...) invisible(NULL))
  mock("googlesheets4", "gs4_token", function() httr::config(timeout = 17))
  headers <- c("record_index", "title", "pdf_found", "pdf_link", "pdf_link", "screening_status", "comments")
  mock("httr", "VERB", function(verb, url, config, body = NULL, encode = NULL, query = NULL, ...) {
    check(identical(config$options$timeout, 17), "gs4_token request configuration was double-wrapped")
    state$requests[[length(state$requests) + 1L]] <- list(verb = verb, url = url, body = body, query = query)
    payload <- if (verb == "POST") list(spreadsheetId = "test-sheet", replies = list()) else if (grepl("/values/", url, fixed = TRUE)) {
      list(values = list(as.list(headers), list("1", "One title", FALSE, "", "", "PDF missing", "Protected comment")))
    } else list(sheets = list(list(properties = list(sheetId = 42L, title = "studies", gridProperties = list(rowCount = 10L, columnCount = 7L)),
      data = list(list(startRow = 1L, startColumn = 6L, rowData = list(list(values = list(list(note = "Protected note", dataValidation = list(strict = TRUE))))))))))
    structure(list(status_code = 200L, headers = list(`content-type` = "application/json"), url = url,
      content = charToRaw(jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null"))), class = "response")
  })
  mock("googledrive", "drive_ls", function(path = NULL, ..., recursive = FALSE) {
    args <- list(...); state$list_calls[[length(state$list_calls) + 1L]] <- args
    check(identical(args$n_max, Inf), "Folder listing is capped")
    check(grepl("md5Checksum", args$fields, fixed = TRUE) && grepl("nextPageToken", args$fields, fixed = TRUE), "Drive listing lacks fingerprints or pagination metadata")
    result <- data.frame(name = "1.pdf", id = "source-id")
    result$drive_resource <- list(list(mimeType = "application/pdf", md5Checksum = strrep("a", 32), size = "100", parents = list("source")))
    result
  })
  mock("googledrive", "drive_cp", function(file, path = NULL, name = NULL, ..., overwrite = NA) {
    check(identical(overwrite, FALSE), "Drive copy can overwrite another file")
    state$copies[[length(state$copies) + 1L]] <- list(file = as.character(file), path = as.character(path), name = name)
    invisible(NULL)
  })
  config <- list(sheet_id = "test-sheet", sheet_tab = "studies")
  adapter <- recovery_google_adapter(config)
  sheet <- adapter$read_sheet()
  check(identical(sheet$headers, headers) && sheet$rows[[1]][[3]] == "FALSE", "Google read lost duplicate columns or Boolean values")
  check(sheet$protected_metadata[["2:7"]]$note == "Protected note", "Sparse Google note position was not preserved")
  files <- adapter$list_files("source", "1.pdf")
  check(files$id == "source-id" && files$md5_checksum == strrep("a", 32) && files$folder_id == "source", "Drive metadata adapter lost file identity")
  check(identical(state$list_calls[[1]]$q, "name = '1.pdf'"), "Exact filename query not passed to drive_ls")
  adapter$copy_file("source-id", "destination", "2.pdf")
  check(state$copies[[1]]$name == "2.pdf" && state$copies[[1]]$path == "destination", "Source rename or destination parent not passed to Drive copy")
  plan <- list(cells = data.frame(row = 2L, column = 3L, record_index = "1", before = "FALSE", after = "TRUE", type = "boolean"), append_rows = list())
  adapter$write_plan(plan, sheet)
  write <- tail(state$requests, 1)[[1]]
  check(write$verb == "POST" && grepl(":batchUpdate$", write$url), "Adapter did not use atomic Sheets batchUpdate")
  check(identical(write$body$requests[[1]]$updateCells$fields, "userEnteredValue"), "Adapter writes fields beyond exact values")
  check(isTRUE(write$body$requests[[1]]$updateCells$rows[[1]]$values[[1]]$userEnteredValue$boolValue), "Boolean cell is not typed correctly")
  cat(sprintf("Passed %d Google adapter API contract assertions (mocked; no network).\n", checks))
}, finally = restore())
