# Step 10: HRP-country correction
# Add Djibouti and Gambia to the HRP list, enrich the extra inclusions from
# EndNote, then use the standard OpenAlex/Europe PMC PDF lookup.
#
# Default: review copy + new Drive PDF folder
# Live:    DESTINATION_MODE=live appends to production + uses Retrieved_PDFs

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})

source("R/endnote_helpers.R")
source("R/pdf_helpers.R")

screening_csv <- Sys.getenv("SCREENING_CSV", "analysis/data-raw/screening_data.csv")
endnote_path <- Sys.getenv("ENDNOTE_ENL_PATH", "analysis/data-raw/Mortality in Crisis.enl")
live_sheet_id <- Sys.getenv("LIVE_SHEET_ID", "")
destination_mode <- tolower(Sys.getenv("DESTINATION_MODE", "review"))
download_pdfs <- tolower(Sys.getenv("DOWNLOAD_PDFS", "true")) %in% c("true", "1", "yes")

review_dir <- "analysis/hrp_correction_review"
pdf_dir <- file.path(review_dir, "pdfs")
manual_urls_path <- "analysis/data-raw/hrp_correction_manual_oa_urls.csv"
drive_pdf_folder_id <- Sys.getenv("PDF_DRIVE_FOLDER_ID", "1C7ufZYeYVyvzULW7zisp_PKlEYUyqYIU")
dir.create(pdf_dir, recursive = TRUE, showWarnings = FALSE)

if (!destination_mode %in% c("review", "live", "local")) stop("DESTINATION_MODE must be review, live, or local.")
if (!file.exists(screening_csv)) stop("Screening CSV not found: ", screening_csv)
if (!file.exists(endnote_path)) stop("EndNote library not found: ", endnote_path)

# 1. Identify records which would pass after adding Djibouti/Gambia to HRP ----
screened <- read_csv(screening_csv, show_col_types = FALSE)
required <- c("decision", "I1", "I2", "E1", "E2", "E3", "E4", "title", "rec_number", "record_index")
if (length(setdiff(required, names(screened))) > 0) stop("Screening CSV is missing: ", paste(setdiff(required, names(screened)), collapse = ", "))

country_text <- apply(screened[, intersect(c("title", "abstract", "explanation"), names(screened)), drop = FALSE], 1, paste, collapse = " ")
additional <- screened |>
  mutate(
    hrp_correction_country = case_when(
      str_detect(country_text, regex("\\bGambia\\b", TRUE)) & str_detect(country_text, regex("\\bDjibouti\\b", TRUE)) ~ "Gambia; Djibouti",
      str_detect(country_text, regex("\\bGambia\\b", TRUE)) ~ "Gambia",
      str_detect(country_text, regex("\\bDjibouti\\b", TRUE)) ~ "Djibouti",
      TRUE ~ NA_character_
    )
  ) |>
  filter(
    tolower(decision) == "exclude", tolower(I1) == "fail", tolower(I2) == "pass",
    tolower(E1) == "pass", tolower(E2) == "pass", tolower(E3) == "pass", tolower(E4) == "pass",
    !is.na(hrp_correction_country)
  )

if (nrow(additional) == 0) stop("No additional studies met the HRP correction rule.")

# 2. Enrich from the original EndNote records ---------------------------------
con <- DBI::dbConnect(RSQLite::SQLite(), endnote_path)
on.exit(DBI::dbDisconnect(con), add = TRUE)
ids <- unique(as.integer(additional$rec_number))
refs <- DBI::dbGetQuery(con, sprintf("SELECT * FROM refs WHERE id IN (%s)", paste(ids, collapse = ",")))
refs$id <- as.character(refs$id)
doi <- extract_endnote_dois(refs) |> select(id, doi)

field <- function(name) if (name %in% names(refs)) as.character(refs[[name]]) else NA_character_
metadata <- tibble(
  rec_number = refs$id,
  journal_name = field("secondary_title"),
  year = field("year"),
  endnote_url = field("url"),
  accession_number = field("accession_number"),
  reference_type = field("reference_type")
) |>
  left_join(doi, by = c("rec_number" = "id"))

additional <- additional |>
  mutate(rec_number = as.character(rec_number), record_index = as.character(record_index)) |>
  select(-any_of(c("doi", "journal_name", "year"))) |>
  left_join(metadata, by = "rec_number")

# 3. Use the same OA sources as analysis/06_fetch_pdfs_full_endnote.R --------
manual_urls <- if (file.exists(manual_urls_path)) {
  read_csv(manual_urls_path, show_col_types = FALSE) |>
    transmute(record_index = as.character(record_index), manual_url = pdf_url, manual_source = pdf_source)
} else tibble(record_index = character(), manual_url = character(), manual_source = character())

# The no-DOI recovery workflow and this script share this Drive folder.
# Set PDF_DRIVE_FOLDER_ID to avoid the name lookup when running non-interactively.
drive_pdf_folder <- NULL
if (download_pdfs) {
  suppressPackageStartupMessages(library(googledrive))
  drive_auth()
  if (nzchar(drive_pdf_folder_id)) {
    drive_pdf_folder <- as_id(drive_pdf_folder_id)
  } else {
    parent <- drive_find(pattern = "^Causes_of_Mortality_Review$", type = "folder")
    drive_pdf_folder <- drive_ls(parent) |> filter(name == "Retrieved_PDFs")
  }
}

download_drive_pdf <- function(path) {
  if (is.null(drive_pdf_folder)) return(FALSE)
  file <- drive_ls(drive_pdf_folder) |> filter(name == basename(path))
  if (nrow(file) == 0) return(FALSE)
  tryCatch({ drive_download(file[1, ], path = path, overwrite = TRUE); file.exists(path) }, error = function(e) FALSE)
}

pdf_results <- lapply(seq_len(nrow(additional)), function(i) {
  record_id <- additional$record_index[[i]]
  pdf_path <- file.path(pdf_dir, paste0(record_id, ".pdf"))
  manual <- filter(manual_urls, record_index == record_id)
  urls <- c(manual$manual_url, query_openalex_pdf_urls(additional$doi[[i]]), query_europepmc_pdf_urls(additional$doi[[i]]))
  urls <- unique(urls[!is.na(urls) & nzchar(urls)])
  found <- file.exists(pdf_path) || download_drive_pdf(pdf_path)
  used_url <- if (found && nrow(manual) > 0) manual$manual_url[[1]] else NA_character_
  source <- if (found && nrow(manual) > 0) manual$manual_source[[1]] else if (found) "Drive PDF folder" else NA_character_

  if (!found && download_pdfs) for (url in urls) {
    if (download_pdf_with_retry(url, pdf_path)) {
      found <- TRUE
      used_url <- url
      source <- if (url %in% manual$manual_url) manual$manual_source[[match(url, manual$manual_url)]] else "OpenAlex/Europe PMC"
      break
    }
  }
  tibble(record_index = record_id, pdf_found = found, pdf_source = source, pdf_url = used_url,
         pdf_path = if (found) pdf_path else NA_character_)
}) |> bind_rows()

additional <- left_join(additional, pdf_results, by = "record_index")
write_csv(additional, file.path(review_dir, "additional_hrp_correction_studies.csv"))
saveRDS(additional, file.path(review_dir, "additional_hrp_correction_studies.rds"))

# 4. Write locally, to a review copy (default), or to production --------------
if (destination_mode != "local") {
  if (!nzchar(live_sheet_id)) stop("LIVE_SHEET_ID is required unless DESTINATION_MODE=local.")
  suppressPackageStartupMessages({ library(googledrive); library(googlesheets4) })
  drive_auth(); gs4_auth()

  target_id <- live_sheet_id
  if (destination_mode == "review") {
    copy_name <- paste0("HRP correction review - ", format(Sys.time(), "%Y-%m-%d %H%M"))
    copied <- tryCatch(drive_cp(as_id(live_sheet_id), name = copy_name), error = function(e) NULL)
    if (is.null(copied)) {
      tabs <- sheet_properties(live_sheet_id)$name
      copied <- gs4_create(copy_name, sheets = setNames(lapply(tabs, function(tab) read_sheet(live_sheet_id, sheet = tab)), tabs))
    }
    target_id <- as.character(copied$id)
  }

  audit_tab <- "HRP correction candidates"
  if (!audit_tab %in% sheet_properties(target_id)$name) sheet_add(target_id, audit_tab)
  sheet_write(additional, target_id, sheet = audit_tab)

  found <- filter(additional, pdf_found, !is.na(pdf_path), file.exists(pdf_path))
  if (nrow(found) > 0) {
    folder <- if (destination_mode == "review") {
      drive_mkdir(paste0("HRP correction review PDFs - ", format(Sys.time(), "%Y-%m-%d %H%M")))
    } else {
      parent <- drive_find(pattern = "^Causes_of_Mortality_Review$", type = "folder")
      drive_ls(parent) |> filter(name == "Retrieved_PDFs")
    }
    found$pdf_drive_link <- vapply(found$pdf_path, function(path) {
      upload <- drive_upload(path, path = folder, name = basename(path), type = "application/pdf")
      drive_share(upload, role = "reader", type = "anyone")
      upload$drive_resource[[1]]$webViewLink
    }, character(1))
    additional <- left_join(additional, found |> select(record_index, pdf_drive_link), by = "record_index")
  }

  # sheet_append is positional: build empty rows in the source column order.
  studies <- read_sheet(target_id, sheet = "studies")
  new <- filter(additional, !record_index %in% as.character(studies$record_index))
  if (nrow(new) > 0) {
    rows <- as.data.frame(setNames(replicate(ncol(studies), rep(NA_character_, nrow(new)), simplify = FALSE), names(studies)))
    for (name in intersect(names(rows), names(new))) rows[[name]] <- as.character(new[[name]])
    for (name in intersect(c("pdf_link...9", "pdf_link...10"), names(rows))) rows[[name]] <- new$pdf_drive_link
    sheet_append(target_id, rows, sheet = "studies")
  }
  cat("Target sheet: https://docs.google.com/spreadsheets/d/", target_id, "\n", sep = "")
}

cat("Additional studies:", nrow(additional), "\n")
cat("PDFs found:", sum(additional$pdf_found), "\n")
