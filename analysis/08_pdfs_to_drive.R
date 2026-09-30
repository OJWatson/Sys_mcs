# analysis/08_pdfs_to_drive

library(dplyr)
library(readr)
library(stringr)
library(googledrive)
library(googlesheets4)

source("R/endnote_helpers.R")

# Paths
final_pdf_rds_path <- "analysis/data-derived/included_studies_with_pdf_full_endnote.rds"
full_endnote_path <- "~/Desktop/PhD files /1st year/Systematic review /endnote/Mortality in Crisis.enl"
pdf_download_dir <- "analysis/pdf_download"

# Google Drive folder names
parent_folder_name <- "Causes_of_Mortality_Review"
pdf_drive_folder_name <- "Retrieved_PDFs"

# Authenticate
drive_auth()
gs4_auth()

# Read final PDF dataset
final_pdf_df <- readRDS(final_pdf_rds_path)

# Read full EndNote library to get journal names
refs_df_new <- read_endnote_refs(full_endnote_path)

journal_lookup_df <- refs_df_new %>%
  mutate(id = as.character(id)) %>%
  select(
    id,
    journal_name = secondary_title
  ) %>%
  distinct(id, .keep_all = TRUE)

# Add journal names
final_pdf_df <- final_pdf_df %>%
  mutate(rec_number = as.character(rec_number)) %>%
  left_join(
    journal_lookup_df,
    by = c("rec_number" = "id")
  ) %>%
  mutate(
    journal_name = ifelse(
      is.na(journal_name) | trimws(journal_name) == "",
      "Unknown journal",
      journal_name
    )
  )

# Deduplicate study records
final_pdf_df <- final_pdf_df %>%
  distinct(record_index, .keep_all = TRUE)

# Create/find parent folder
parent_folder <- drive_find(
  pattern = parent_folder_name,
  type = "folder"
)

if (nrow(parent_folder) == 0) {
  parent_folder <- drive_mkdir(parent_folder_name)
}

# Create/find PDF folder inside parent folder
pdf_drive_folder <- drive_ls(parent_folder) %>%
  filter(name == pdf_drive_folder_name)

if (nrow(pdf_drive_folder) == 0) {
  pdf_drive_folder <- drive_mkdir(
    name = pdf_drive_folder_name,
    path = parent_folder
  )
}

# Prepare unique PDFs for upload
pdf_upload_df <- final_pdf_df %>%
  filter(pdf_found == TRUE, !is.na(pdf_path), pdf_path != "") %>%
  mutate(
    pdf_filename = basename(pdf_path)
  ) %>%
  distinct(pdf_filename, .keep_all = TRUE)

cat("Unique PDFs to upload/check:", nrow(pdf_upload_df), "\n")

# Upload PDFs and collect links
uploaded_files_list <- list()

for (i in seq_len(nrow(pdf_upload_df))) {

  local_pdf_path <- pdf_upload_df$pdf_path[i]
  pdf_filename <- pdf_upload_df$pdf_filename[i]

  cat("Uploading/checking", i, "of", nrow(pdf_upload_df), ":", pdf_filename, "\n")

  existing_file <- drive_ls(pdf_drive_folder) %>%
    filter(name == pdf_filename)

  if (nrow(existing_file) > 0) {
    uploaded_file <- existing_file
  } else {
    uploaded_file <- drive_upload(
      media = local_pdf_path,
      path = pdf_drive_folder,
      name = pdf_filename,
      overwrite = FALSE
    )
  }

  drive_share(
    uploaded_file,
    role = "reader",
    type = "anyone"
  )

  uploaded_files_list[[i]] <- data.frame(
    pdf_filename = pdf_filename,
    drive_pdf_link = uploaded_file$drive_resource[[1]]$webViewLink,
    stringsAsFactors = FALSE
  )
}

drive_links_df <- bind_rows(uploaded_files_list) %>%
  distinct(pdf_filename, .keep_all = TRUE)

# Create master Google Sheet table with all records
screening_sheet_df <- final_pdf_df %>%
  mutate(
    pdf_filename = ifelse(
      pdf_found == TRUE & !is.na(pdf_path),
      basename(pdf_path),
      NA_character_
    )
  ) %>%
  left_join(
    drive_links_df,
    by = "pdf_filename"
  ) %>%
  mutate(
    pdf_link = ifelse(
      pdf_found == TRUE & !is.na(drive_pdf_link),
      drive_pdf_link,
      ""
    ),
    screening_status = ifelse(
      pdf_link != "",
      "PDF uploaded",
      "PDF missing"
    )
  ) %>%
  transmute(
    record_index,
    rec_number,
    title = title_screening,
    journal_name,
    year,
    abstract,
    doi,
    pdf_found,
    pdf_link,
    screening_status,
    Nisreen = "",
    OJ = "",
    Paula = "",
    Bhargavi = ""
  )

# Save local backup
write_csv(
  screening_sheet_df,
  "analysis/data-derived/google_drive_screening_sheet.csv"
)

# Create Google Sheet
sheet <- gs4_create(
  name = "Causes of Mortality - PDF Screening Sheet",
  sheets = list(
    studies = screening_sheet_df
  )
)

cat("Google Sheet created:\n")
cat(sheet$spreadsheet_url, "\n")
cat("Done.\n")
