# Run each section interactively from the repository root. The three country
# CSVs contain the reviewed decisions, bibliography and verified PDF sources.
# Restore records for full-text screening, not final review inclusion.
library(dplyr)
library(readr)
library(googledrive)
library(googlesheets4)
source("R/pdf_helpers.R")

# 1. Read all screening records and find country-related exclusions.
screened <- read_csv("analysis/data-raw/screening_data.csv", show_col_types = FALSE)
country_pattern <- "\\b(djibouti(an)?|djibuti|jibouti|jibuti|dschibuti|gambia(n)?|gambie)\\b"
country_excluded <- screened %>% filter(decision == "Exclude", I1 == "fail")
country_hits <- country_excluded %>%
  filter(grepl(country_pattern, paste(title, abstract, explanation), ignore.case = TRUE)) %>%
  select(record_index, rec_number, title, abstract, explanation, I1, I2, E1, E2, E3, E4)

# Other I/E flags were encoded inconsistently. Review the text, not an all-pass
# rule. There is no paper cap. Keep the complete matched audit trail.
write_csv(country_hits, "analysis/data-derived/country_exclusion_candidates.csv")
counts <- data.frame(stage = c("All records", "Excluded", "Excluded with I1 fail", "Country matches"),
                    records = c(nrow(screened), sum(screened$decision == "Exclude"),
                                nrow(country_excluded), nrow(country_hits)))
print(counts)
write_csv(counts, "analysis/data-derived/country_exclusion_counts.csv")

# 2. Join the explicit per-record review and bibliography, keyed by both IDs.
# These small CSVs retain the review reasons, DOI and unresolved eligibility checks.
review <- read_csv("data-raw/country_exclusion_review.csv", show_col_types = FALSE)
bibliography <- read_csv("data-raw/country_bibliography.csv", show_col_types = FALSE) %>%
  select(record_index, rec_number, journal_name, year, doi)
reviewed <- country_hits %>%
  left_join(review, by = c("record_index", "rec_number")) %>%
  left_join(bibliography, by = c("record_index", "rec_number"))
count(reviewed, action)
restore <- reviewed %>% filter(action == "restore_for_full_text")
write_csv(reviewed, "analysis/data-derived/country_exclusion_reviewed.csv")
write_csv(restore, "analysis/data-derived/country_records_to_restore.csv")

# 3. Retrieve PDFs with the existing helpers. Leave FALSE for selection-only runs.
# A successful download still needs title/DOI/content review before upload.
download_pdfs <- FALSE
pdf_dir <- "analysis/hrp_correction_review/pdfs"
if (download_pdfs) {
  dir.create(pdf_dir, recursive = TRUE, showWarnings = FALSE)
  for (i in seq_len(nrow(restore))) {
    pdf_path <- file.path(pdf_dir, paste0(restore$record_index[i], ".pdf"))
    if (file.exists(pdf_path) || is.na(restore$doi[i]) || restore$doi[i] == "") next
    urls <- unique(c(query_openalex_pdf_urls(restore$doi[i]),
                     query_europepmc_pdf_urls(restore$doi[i])))
    for (url in urls) {
      if (download_pdf_with_retry(url, pdf_path)) break
    }
  }
}

# 4. Copy/upload reviewed PDFs and append the missing screening records.
# Six PDFs have existing source Drive IDs; four use the reviewed local copies.
sync_to_google <- FALSE
if (sync_to_google) {
  drive_auth()
  gs4_auth()
  sheet_id <- "1foHMPBCWYzWX8975UKjdacE2i_j_48n8uTMdAQ-ETcg"
  folder_id <- "1C7ufZYeYVyvzULW7zisp_PKlEYUyqYIU"
  pdf_review <- read_csv("data-raw/country_pdf_review.csv", show_col_types = FALSE)
  current <- range_read(sheet_id, sheet = "studies", col_types = "c", .name_repair = "minimal")
  headers <- c("record_index", "rec_number", "title", "journal_name", "year",
               "abstract", "doi", "pdf_found", "pdf_link", "pdf_link", "screening_status")
  if (!identical(names(current)[1:11], headers)) stop("Check the studies Sheet columns before appending.")
  files <- drive_ls(as_id(folder_id), n_max = Inf)

  for (i in seq_len(nrow(restore))) {
    row <- restore[i, ]
    if (as.character(row$record_index) %in% current[[1]]) next
    pdf <- pdf_review %>% filter(record_index == row$record_index, verified)
    pdf_link <- NA_character_
    if (nrow(pdf) == 1) {
      filename <- paste0(row$record_index, ".pdf")
      existing <- files %>% filter(name == filename)
      if (nrow(existing) > 1) stop("Multiple Drive PDFs named ", filename)
      if (nrow(existing) == 1) {
        file <- existing
      } else if (!is.na(pdf$source_drive_id)) {
        source_pdf <- drive_get(as_id(pdf$source_drive_id))
        if (source_pdf$drive_resource[[1]]$md5Checksum != pdf$md5)
          stop("Source PDF differs from the reviewed copy: ", filename)
        file <- drive_cp(source_pdf, path = as_id(folder_id), name = filename, overwrite = FALSE)
      } else {
        path <- file.path(pdf_dir, filename)
        if (!identical(unname(tools::md5sum(path)), pdf$md5))
          stop("Place the reviewed PDF in ", path)
        file <- drive_upload(path, path = as_id(folder_id), name = filename)
      }
      file <- drive_get(file$id)
      if (file$drive_resource[[1]]$md5Checksum != pdf$md5)
        stop("Destination PDF differs from the reviewed copy: ", filename)
      pdf_link <- paste0("https://drive.google.com/file/d/", file$id, "/view?usp=drivesdk")
    }

    # Keep the Sheet's duplicate pdf_link column as its existing I-column formula.
    current <- range_read(sheet_id, sheet = "studies", col_types = "c", .name_repair = "minimal")
    if (as.character(row$record_index) %in% current[[1]]) next
    row_number <- nrow(current) + 2L
    values <- data.frame(row$record_index, as.character(row$rec_number), row$title,
      row$journal_name, as.character(row$year), row$abstract, row$doi, !is.na(pdf_link),
      pdf_link, if (!is.na(pdf_link)) gs4_formula(paste0("=ARRAYFORMULA(I", row_number, ")")) else NA_character_,
      ifelse(!is.na(pdf_link), "PDF uploaded", "PDF missing"))
    names(values) <- headers
    sheet_append(sheet_id, values, sheet = "studies")
  }
}
