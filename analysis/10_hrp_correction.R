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
  refs <- read_endnote_refs(endnote_path)
  doi_lookup <- extract_endnote_dois(refs) |>
    mutate(title_key = normalise_title(.data$title)) |>
    filter(!is.na(.data$title_key), nzchar(.data$title_key)) |>
    distinct(.data$title_key, .keep_all = TRUE)
  additional <- additional |>
    mutate(title_key = normalise_title(.data$title)) |>
    left_join(doi_lookup |> select(title_key, endnote_doi = doi), by = "title_key") |>
    mutate(
      doi = .data$endnote_doi,
      doi_source = if_else(!is.na(.data$endnote_doi), "EndNote title match", NA_character_)
    ) |>
    select(-.data$endnote_doi, -.data$title_key)
}

doi_by_title <- function(title) {
  if (is.na(title) || !nzchar(trimws(title))) return(NA_character_)
  query <- utils::URLencode(title, reserved = TRUE)
  response <- get_json_with_retry(paste0("https://api.openalex.org/works?per-page=10&search=", query))
  if (!is.null(response) && !is.null(response$results) && is.data.frame(response$results) && "display_name" %in% names(response$results)) {
    exact <- response$results[normalise_title(response$results$display_name) == normalise_title(title), , drop = FALSE]
    if (nrow(exact) > 0 && "doi" %in% names(exact) && !is.na(exact$doi[[1]])) {
      return(sub("^https?://doi.org/", "", tolower(exact$doi[[1]])))
    }
  }

  # OpenAlex may be temporarily unavailable; Crossref gives an independent
  # exact-title fallback without silently accepting a fuzzy match.
  crossref <- get_json_with_retry(paste0("https://api.crossref.org/works?rows=10&query.title=", query))
  items <- crossref$message$items %||% NULL
  if (is.null(items) || !is.data.frame(items) || !"title" %in% names(items) || !"DOI" %in% names(items)) return(NA_character_)
  item_titles <- vapply(items$title, function(x) if (length(x) == 0) "" else x[[1]], character(1))
  exact <- items[normalise_title(item_titles) == normalise_title(title), , drop = FALSE]
  if (nrow(exact) == 0 || is.na(exact$DOI[[1]]) || !nzchar(exact$DOI[[1]])) return(NA_character_)
  tolower(exact$DOI[[1]])
}

for (i in which(is.na(additional$doi) | !nzchar(additional$doi))) {
  candidate <- doi_by_title(additional$title[[i]])
  if (!is.na(candidate)) {
    additional$doi[[i]] <- candidate
    additional$doi_source[[i]] <- "OpenAlex/Crossref exact title match"
  }
}

record_id <- if ("record_index" %in% names(additional)) as.character(additional$record_index) else as.character(seq_len(nrow(additional)))
pdf_results <- vector("list", nrow(additional))
for (i in seq_len(nrow(additional))) {
  doi <- additional$doi[[i]]
  destination <- file.path(pdf_dir, paste0(record_id[[i]], ".pdf"))
  urls <- if (is.na(doi) || !nzchar(doi)) character() else unique(c(query_openalex_pdf_urls(doi), query_europepmc_pdf_urls(doi)))
  found <- file.exists(destination)
  used_url <- if (found) NA_character_ else NA_character_
  used_source <- if (found) "already_downloaded" else NA_character_
  if (download_pdfs && !found) {
    for (url in urls) {
      if (download_pdf_with_retry(url, destination)) {
        found <- TRUE
        used_url <- url
        used_source <- "OpenAlex/Europe PMC"
        break
      }
    }
  }
  pdf_results[[i]] <- data.frame(
    record_index = record_id[[i]], pdf_found = found, pdf_source = used_source,
    pdf_url = used_url, pdf_path = if (found) destination else NA_character_,
    stringsAsFactors = FALSE
  )
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
  copied <- drive_cp(as_id(live_sheet_id), name = paste0("HRP correction review copy - ", format(Sys.time(), "%Y-%m-%d %H%M")))
  copied_id <- as.character(copied$id)
  review_url <- paste0("https://docs.google.com/spreadsheets/d/", copied_id)
  audit_tab <- "HRP correction candidates"
  sheet_add(copied_id, audit_tab)
  sheet_write(additional, ss = copied_id, sheet = audit_tab)

  tabs <- sheet_properties(copied_id)$name
  if ("studies" %in% tabs) {
    existing <- read_sheet(copied_id, sheet = "studies")
    shared <- intersect(names(existing), names(additional))
    if ("record_index" %in% shared) {
      new_rows <- additional |> filter(!.data$record_index %in% as.character(existing$record_index))
      if (nrow(new_rows) > 0) {
        append_rows <- new_rows[, shared, drop = FALSE]
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
