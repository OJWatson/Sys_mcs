# Step 10: HRP-country correction review
#
# Counterfactual: treat Djibouti and Gambia as HRP countries. A record becomes
# an additional inclusion only when I1 is its sole failed rule: decision is
# Exclude, I1 is fail, and I2/E1/E2/E3/E4 are all pass.
#
# The script always writes local review files. With SHEET_MODE=copy it copies
# the live review sheet, appends only new candidates to the copy, adds a full
# audit tab, and never writes to the source spreadsheet.
#
# Example:
# SCREENING_CSV=/path/to/first_149064_completed_screening_records.csv \
# ENDNOTE_ENL_PATH=/path/to/'Mortality in Crisis.enl' \
# LIVE_SHEET_ID=<source-sheet-id> SHEET_MODE=copy \
# Rscript analysis/10_hrp_correction.R

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(httr2)
  library(jsonlite)
})

source("R/endnote_helpers.R")
source("R/pdf_helpers.R")

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x

screening_csv <- Sys.getenv("SCREENING_CSV", unset = "analysis/data-raw/screening_data.csv")
endnote_path <- Sys.getenv("ENDNOTE_ENL_PATH", unset = "analysis/data-raw/Mortality in Crisis.enl")
live_sheet_id <- Sys.getenv("LIVE_SHEET_ID", unset = "")
sheet_mode <- tolower(Sys.getenv("SHEET_MODE", unset = "local"))
download_pdfs <- tolower(Sys.getenv("DOWNLOAD_PDFS", unset = "true")) %in% c("true", "1", "yes")
review_dir <- "analysis/hrp_correction_review"
pdf_dir <- file.path(review_dir, "pdfs")

dir.create(review_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(pdf_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(screening_csv)) {
  stop("Screening CSV not found: ", screening_csv,
       "\nSet SCREENING_CSV or run the screening-download step first.")
}

screened <- readr::read_csv(screening_csv, show_col_types = FALSE)
required <- c("decision", "I1", "I2", "E1", "E2", "E3", "E4")
missing <- setdiff(required, names(screened))
if (length(missing) > 0) stop("Missing screening columns: ", paste(missing, collapse = ", "))

# Country evidence is searched across the bibliographic information and the
# screener's explanation. Keeping this evidence in the output makes the
# correction auditable later.
evidence_columns <- intersect(c("title", "abstract", "explanation"), names(screened))
if (length(evidence_columns) == 0) stop("Expected one of title, abstract, or explanation.")
evidence_text <- apply(screened[, evidence_columns, drop = FALSE], 1, function(x) paste(x[!is.na(x)], collapse = " "))
has_gambia <- str_detect(evidence_text, regex("\\bGambia\\b", ignore_case = TRUE))
has_djibouti <- str_detect(evidence_text, regex("\\bDjibouti\\b", ignore_case = TRUE))

additional <- screened |>
  mutate(
    .decision = tolower(trimws(as.character(.data$decision))),
    .I1 = tolower(trimws(as.character(.data$I1))),
    .I2 = tolower(trimws(as.character(.data$I2))),
    .E1 = tolower(trimws(as.character(.data$E1))),
    .E2 = tolower(trimws(as.character(.data$E2))),
    .E3 = tolower(trimws(as.character(.data$E3))),
    .E4 = tolower(trimws(as.character(.data$E4))),
    hrp_correction_country = case_when(
      has_gambia & has_djibouti ~ "Gambia; Djibouti",
      has_gambia ~ "Gambia",
      has_djibouti ~ "Djibouti",
      TRUE ~ NA_character_
    ),
    hrp_correction_reason = "I1 would pass after adding Djibouti/Gambia to HRP countries"
  ) |>
  filter(
    .decision == "exclude", .I1 == "fail", .I2 == "pass",
    .E1 == "pass", .E2 == "pass", .E3 == "pass", .E4 == "pass",
    !is.na(.data$hrp_correction_country)
  ) |>
  select(-starts_with("."))

if (nrow(additional) == 0) stop("No additional studies met the HRP-correction rule.")

normalise_title <- function(x) {
  x |> tolower() |> str_replace_all("[^a-z0-9]", "")
}

# First use the same EndNote lookup as scripts 02/06. If that library is not
# present, use an exact title match from OpenAlex as a clearly-labelled fallback.
additional$doi <- NA_character_
additional$doi_source <- NA_character_
if (file.exists(endnote_path) && file.info(endnote_path)$size > 0) {
  # The screening export preserves the originating EndNote record number in
  # `rec_number`. Query those exact records directly instead of loading the
  # whole (835 MB) library merely to enrich a small correction set.
  requested_ids <- unique(as.character(additional$rec_number))
  requested_ids <- requested_ids[!is.na(requested_ids) & grepl("^[0-9]+$", requested_ids)]
  endnote_con <- DBI::dbConnect(RSQLite::SQLite(), endnote_path)
  on.exit(DBI::dbDisconnect(endnote_con), add = TRUE)
  if (length(requested_ids) > 0) {
    id_sql <- paste(requested_ids, collapse = ",")
    refs <- DBI::dbGetQuery(endnote_con, paste0("SELECT * FROM refs WHERE id IN (", id_sql, ")"))
  } else {
    refs <- read_endnote_refs(endnote_path)
  }
  refs$id <- as.character(refs$id)
  # Preserve the complete bibliographic fields held in the full EndNote
  # library.  These are authoritative for this review and remain useful when
  # Crossref/OpenAlex cannot resolve a conference abstract or supplement DOI.
  endnote_metadata <- refs |>
    transmute(
      endnote_id = as.character(.data$id),
      title_key = normalise_title(.data$title),
      endnote_journal_name = as.character(.data$secondary_title),
      endnote_year = as.character(.data$year),
      endnote_url = as.character(.data$url),
      endnote_accession_number = as.character(.data$accession_number),
      endnote_notes = as.character(.data$notes),
      endnote_reference_type = as.character(.data$reference_type)
    ) |>
    filter(!is.na(.data$title_key), nzchar(.data$title_key)) |>
    distinct(.data$title_key, .keep_all = TRUE)
  doi_lookup <- extract_endnote_dois(refs) |>
    mutate(title_key = normalise_title(.data$title)) |>
    filter(!is.na(.data$title_key), nzchar(.data$title_key)) |>
    distinct(.data$title_key, .keep_all = TRUE)
  additional <- additional |>
    mutate(title_key = normalise_title(.data$title)) |>
    left_join(doi_lookup |> select(title_key, endnote_doi = doi), by = "title_key") |>
    left_join(endnote_metadata, by = "title_key") |>
    mutate(
      doi = .data$endnote_doi,
      doi_source = if_else(!is.na(.data$endnote_doi), "EndNote title match", NA_character_)
    ) |>
    select(-.data$endnote_doi, -.data$title_key)
}

doi_by_title <- function(title) {
  if (is.na(title) || !nzchar(trimws(title))) return(NA_character_)
  query <- utils::URLencode(title, reserved = TRUE)
  # Crossref is checked first because OpenAlex can be intermittently
  # unavailable. Both paths accept exact normalized-title matches only.
  crossref <- get_json_with_retry(paste0("https://api.crossref.org/works?rows=10&query.title=", query))
  items <- crossref$message$items %||% NULL
  if (!is.null(items) && is.data.frame(items) && "title" %in% names(items) && "DOI" %in% names(items)) {
    item_titles <- vapply(items$title, function(x) if (length(x) == 0) "" else x[[1]], character(1))
    exact <- items[normalise_title(item_titles) == normalise_title(title), , drop = FALSE]
    if (nrow(exact) > 0 && !is.na(exact$DOI[[1]]) && nzchar(exact$DOI[[1]])) return(tolower(exact$DOI[[1]]))
  }

  response <- get_json_with_retry(paste0("https://api.openalex.org/works?per-page=10&search=", query))
  if (is.null(response) || is.null(response$results) || !is.data.frame(response$results) || !"display_name" %in% names(response$results)) return(NA_character_)
  exact <- response$results[normalise_title(response$results$display_name) == normalise_title(title), , drop = FALSE]
  if (nrow(exact) == 0 || !"doi" %in% names(exact) || is.na(exact$doi[[1]])) return(NA_character_)
  sub("^https?://doi.org/", "", tolower(exact$doi[[1]]))
}

for (i in which(is.na(additional$doi) | !nzchar(additional$doi))) {
  candidate <- doi_by_title(additional$title[[i]])
  if (!is.na(candidate)) {
    additional$doi[[i]] <- candidate
    additional$doi_source[[i]] <- "OpenAlex/Crossref exact title match"
  }
}

# Crossref is also used for journal/year fields and publisher-hosted PDF links.
# We retain the source URLs in the audit output even if a publisher rejects an
# automated download, rather than treating absence from OpenAlex as proof that
# no open-access PDF exists.
crossref_metadata_by_doi <- function(doi) {
  empty <- list(journal_name = NA_character_, year = NA_character_, pdf_urls = character(0))
  if (is.na(doi) || !nzchar(doi)) return(empty)
  work <- get_json_with_retry(paste0("https://api.crossref.org/works/", utils::URLencode(doi, reserved = TRUE)))
  message <- work$message %||% NULL
  if (is.null(message)) return(empty)

  journal <- message$`container-title` %||% message$container_title %||% character(0)
  journal <- if (length(journal) > 0) as.character(journal[[1]]) else NA_character_
  year <- NA_character_
  for (field in c("published-print", "published-online", "issued", "created")) {
    value <- message[[field]]
    if (!is.null(value)) {
      numbers <- unlist(value, use.names = FALSE)
      candidate <- suppressWarnings(as.integer(numbers[[1]]))
      if (!is.na(candidate) && candidate > 1000 && candidate < 3000) {
        year <- as.character(candidate)
        break
      }
    }
  }

  links <- message$link %||% NULL
  urls <- character(0)
  if (is.data.frame(links) && "URL" %in% names(links)) urls <- as.character(links$URL)
  if (is.list(links) && !is.data.frame(links)) {
    urls <- unlist(lapply(links, function(x) x$URL %||% x$url %||% character(0)), use.names = FALSE)
  }
  list(journal_name = journal, year = year, pdf_urls = unique(urls[!is.na(urls) & nzchar(urls)]))
}

metadata <- lapply(additional$doi, crossref_metadata_by_doi)
crossref_journal_name <- vapply(metadata, `[[`, character(1), "journal_name")
crossref_year <- vapply(metadata, `[[`, character(1), "year")
# EndNote is the bibliographic source used by this review. Prefer its exact
# journal/year values, then fall back to Crossref where the library is blank.
# Keep Crossref's values separately in the audit output for traceability.
additional$crossref_journal_name <- crossref_journal_name
additional$crossref_year <- crossref_year
additional$journal_name <- dplyr::coalesce(additional$endnote_journal_name, crossref_journal_name)
additional$year <- dplyr::coalesce(additional$endnote_year, crossref_year)
additional$crossref_pdf_urls <- lapply(metadata, `[[`, "pdf_urls")

query_unpaywall_pdf_urls <- function(doi) {
  if (is.na(doi) || !nzchar(doi)) return(character(0))
  email <- Sys.getenv("UNPAYWALL_EMAIL", unset = "oj.watson92@gmail.com")
  result <- get_json_with_retry(paste0(
    "https://api.unpaywall.org/v2/", utils::URLencode(doi, reserved = TRUE),
    "?email=", utils::URLencode(email, reserved = TRUE)
  ))
  if (is.null(result)) return(character(0))
  locations <- result$oa_locations %||% list()
  urls <- c(result$best_oa_location$url_for_pdf %||% character(0))
  if (is.data.frame(locations) && "url_for_pdf" %in% names(locations)) urls <- c(urls, locations$url_for_pdf)
  if (is.list(locations) && !is.data.frame(locations)) {
    urls <- c(urls, unlist(lapply(locations, function(x) x$url_for_pdf %||% character(0)), use.names = FALSE))
  }
  unique(as.character(urls[!is.na(urls) & nzchar(urls)]))
}

download_candidate_pdf <- function(url, destination) {
  if (is.na(url) || !nzchar(url)) return(list(found = FALSE, detail = "empty URL"))
  request <- httr2::request(url) |>
    httr2::req_user_agent("mortality-crisis-endnote-compendium/0.1") |>
    httr2::req_headers(Accept = "application/pdf") |>
    httr2::req_timeout(12)
  response <- tryCatch(httr2::req_perform(request), error = function(e) e)
  if (inherits(response, "error")) return(list(found = FALSE, detail = conditionMessage(response)))
  content_type <- httr2::resp_header(response, "content-type") %||% ""
  body <- httr2::resp_body_raw(response)
  is_pdf <- length(body) >= 4 && identical(rawToChar(body[1:4]), "%PDF")
  if (httr2::resp_status(response) >= 200 && httr2::resp_status(response) < 300 &&
      (grepl("application/pdf|application/octet-stream", content_type, ignore.case = TRUE) || is_pdf) && is_pdf) {
    writeBin(body, destination)
    return(list(found = TRUE, detail = paste0("HTTP ", httr2::resp_status(response))))
  }
  list(found = FALSE, detail = paste0("HTTP ", httr2::resp_status(response), "; ", content_type))
}

record_id <- if ("record_index" %in% names(additional)) as.character(additional$record_index) else as.character(seq_len(nrow(additional)))
pdf_results <- vector("list", nrow(additional))
for (i in seq_len(nrow(additional))) {
  doi <- additional$doi[[i]]
  destination <- file.path(pdf_dir, paste0(record_id[[i]], ".pdf"))
  publisher_urls <- additional$crossref_pdf_urls[[i]] %||% character(0)
  unpaywall_urls <- query_unpaywall_pdf_urls(doi)
  oa_urls <- if (is.na(doi) || !nzchar(doi)) character() else unique(c(query_openalex_pdf_urls(doi), query_europepmc_pdf_urls(doi)))
  endnote_urls <- additional$endnote_url[[i]] %||% character(0)
  urls <- unique(c(publisher_urls, unpaywall_urls, oa_urls, endnote_urls))
  found <- file.exists(destination)
  used_url <- if (found) NA_character_ else NA_character_
  used_source <- if (found) "already_downloaded" else NA_character_
  attempted <- character(0)
  if (download_pdfs && !found) {
    for (url in urls) {
      attempt <- download_candidate_pdf(url, destination)
      attempted <- c(attempted, paste0(url, " [", attempt$detail, "]"))
      if (attempt$found) {
        found <- TRUE
        used_url <- url
        used_source <- if (url %in% publisher_urls) "Crossref publisher link" else if (url %in% unpaywall_urls) "Unpaywall OA link" else "OpenAlex/Europe PMC"
        break
      }
    }
  }
  pdf_results[[i]] <- data.frame(
    record_index = record_id[[i]], pdf_found = found, pdf_source = used_source,
    pdf_url = used_url, pdf_path = if (found) destination else NA_character_,
    pdf_access_status = if (found) "Downloaded" else if (length(urls) > 0) "OA/publisher/EndNote URL found; download needs follow-up" else "No OA/publisher/EndNote PDF URL found",
    pdf_urls_checked = paste(urls, collapse = " | "), pdf_attempt_log = paste(attempted, collapse = " | "),
    stringsAsFactors = FALSE
  )
}

make_study_rows <- function(candidates, sheet_columns) {
  # sheet_append is positional. Construct all source-sheet columns in the
  # source order, leaving reviewer fields blank, so no values can shift.
  rows <- as.data.frame(
    setNames(replicate(length(sheet_columns), rep(NA_character_, nrow(candidates)), simplify = FALSE), sheet_columns),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  put <- function(column, value) {
    if (column %in% names(rows)) rows[[column]] <<- as.character(value)
  }
  put("record_index", candidates$record_index)
  put("rec_number", candidates$rec_number)
  put("title", candidates$title)
  put("journal_name", candidates$journal_name)
  put("year", candidates$year)
  put("abstract", candidates$abstract)
  put("doi", candidates$doi)
  put("pdf_found", candidates$pdf_found)
  put("screening_status", ifelse(candidates$pdf_found, "PDF found – review link", candidates$pdf_access_status))
  rows
}

additional <- additional |>
  mutate(record_index = record_id) |>
  left_join(bind_rows(pdf_results), by = "record_index")

local_csv <- file.path(review_dir, "additional_hrp_correction_studies.csv")
local_rds <- file.path(review_dir, "additional_hrp_correction_studies.rds")
write_csv(additional, local_csv)
saveRDS(additional, local_rds)

# The Google write path is intentionally opt-in and copy-only. It adds records
# to the copied `studies` tab when its standard columns are available, and it
# always creates a complete audit tab in the copy.
review_url <- NA_character_
if (sheet_mode == "copy") {
  if (!nzchar(live_sheet_id)) stop("SHEET_MODE=copy requires LIVE_SHEET_ID; refusing to guess a source sheet.")
  suppressPackageStartupMessages({ library(googledrive); library(googlesheets4) })
  drive_auth()
  gs4_auth()
  copy_name <- paste0("HRP correction review copy - ", format(Sys.time(), "%Y-%m-%d %H%M"))
  copied <- tryCatch(
    drive_cp(as_id(live_sheet_id), name = copy_name),
    error = function(e) NULL
  )
  if (is.null(copied)) {
    # Some shared sheets allow reading but disallow Drive's native copy action.
    # In that case make an owned value-level clone of every existing tab. This
    # is still review-only and leaves the source spreadsheet unchanged.
    source_tabs <- sheet_properties(live_sheet_id)$name
    source_values <- lapply(source_tabs, function(tab) read_sheet(live_sheet_id, sheet = tab))
    names(source_values) <- source_tabs
    copied <- gs4_create(name = copy_name, sheets = source_values)
  }
  copied_id <- if (is.atomic(copied) && length(copied) == 1) {
    as.character(copied)
  } else {
    as.character(copied$id)
  }
  review_url <- paste0("https://docs.google.com/spreadsheets/d/", copied_id)
  audit_tab <- "HRP correction candidates"
  sheet_add(copied_id, audit_tab)

  # Upload PDFs only to a dedicated folder owned by this review run, never to
  # the production sheet's PDF folder. These links are then placed in both
  # legacy PDF-link columns used by the existing studies tab.
  additional$pdf_drive_link <- NA_character_
  found_paths <- which(additional$pdf_found & !is.na(additional$pdf_path) & file.exists(additional$pdf_path))
  if (length(found_paths) > 0) {
    folder <- drive_mkdir(paste0("HRP correction review PDFs - ", format(Sys.time(), "%Y-%m-%d %H%M")))
    folder_id <- as.character(folder$id)
    for (i in found_paths) {
      uploaded <- drive_upload(additional$pdf_path[[i]], path = as_id(folder_id), name = basename(additional$pdf_path[[i]]), type = "application/pdf")
      additional$pdf_drive_link[[i]] <- paste0("https://drive.google.com/file/d/", as.character(uploaded$id), "/view")
    }
  }
  sheet_write(additional, ss = copied_id, sheet = audit_tab)

  tabs <- sheet_properties(copied_id)$name
  if ("studies" %in% tabs) {
    existing <- read_sheet(copied_id, sheet = "studies")
    if ("record_index" %in% names(existing)) {
      new_rows <- additional |> filter(!.data$record_index %in% as.character(existing$record_index))
      if (nrow(new_rows) > 0) {
        append_rows <- make_study_rows(new_rows, names(existing))
        for (pdf_column in intersect(c("pdf_link...9", "pdf_link...10"), names(append_rows))) {
          append_rows[[pdf_column]] <- new_rows$pdf_drive_link
        }
        sheet_append(copied_id, data = append_rows, sheet = "studies")
      }
    }
  }
}

cat("Additional studies:", nrow(additional), "\n")
cat("Gambia:", sum(additional$hrp_correction_country == "Gambia"), "\n")
cat("Djibouti:", sum(additional$hrp_correction_country == "Djibouti"), "\n")
cat("PDFs found:", sum(additional$pdf_found), "\n")
cat("Local review CSV:", local_csv, "\n")
if (!is.na(review_url)) cat("Review sheet copy:", review_url, "\n")
