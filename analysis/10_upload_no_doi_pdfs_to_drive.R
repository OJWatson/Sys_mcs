# Run interactively from the repository root.
library(dplyr)
library(readr)
library(googledrive)
library(googlesheets4)

# Read the reviewed matches and keep unresolved records separate.
review <- read_csv("data-raw/missing_pdf_review.csv", col_types = cols(.default = "c"))
pdfs <- review %>% filter(file_action %in% c("copy", "reuse"))
held <- review %>% filter(file_action == "hold")
pdfs$destination_file_id[is.na(pdfs$destination_file_id)] <- ""
dir.create("analysis/data-derived", recursive = TRUE, showWarnings = FALSE)
write_csv(pdfs, "analysis/data-derived/recovered_pdf_reviewed.csv")
write_csv(held, "analysis/data-derived/recovered_pdf_holds.csv")
cat(nrow(review), "reviewed records;", nrow(pdfs), "reviewed matches;", nrow(held), "held.\n")

# Set TRUE to copy missing PDFs and update the existing screening Sheet.
sync_to_google <- FALSE
if (sync_to_google) {
  drive_auth()
  gs4_auth()
  folder_id <- "1C7ufZYeYVyvzULW7zisp_PKlEYUyqYIU"
  sheet_id <- "1foHMPBCWYzWX8975UKjdacE2i_j_48n8uTMdAQ-ETcg"
  files <- drive_ls(as_id(folder_id))
  studies <- range_read(sheet_id, sheet = "studies", col_types = "c", .name_repair = "minimal")
  studies[is.na(studies)] <- ""

  for (i in seq_len(nrow(pdfs))) {
    row <- which(studies[[1]] == pdfs$record_index[i])
    if (!length(row)) next
    if (length(row) != 1 || studies[[3]][row] != pdfs$title[i])
      stop("Check the Sheet match for record ", pdfs$record_index[i])

    # Keep delivered IDs; otherwise reuse the filename or copy the reviewed PDF.
    if (pdfs$destination_file_id[i] == "") {
      if (pdfs$file_action[i] == "reuse") stop("Missing reuse ID for record ", pdfs$record_index[i])
      file <- files[files$name == pdfs$destination_filename[i], ]
      if (!nrow(file)) {
        file <- drive_cp(as_id(pdfs$source_drive_file_id[i]), path = as_id(folder_id),
          name = pdfs$destination_filename[i], overwrite = FALSE)
        files <- bind_rows(files, file)
      }
      if (nrow(file) != 1) stop("Check duplicate filenames for record ", pdfs$record_index[i])
      pdfs$destination_file_id[i] <- file$id
    }
    pdf_link <- paste0("https://drive.google.com/file/d/", pdfs$destination_file_id[i], "/view")
    existing_link <- studies[[9]][row]
    if (!existing_link %in% c("", pdf_link, paste0(pdf_link, "?usp=drivesdk")))
      stop("Check the existing PDF link for record ", pdfs$record_index[i])
    if (existing_link != "") pdf_link <- existing_link
    sheet_row <- row + 1L

    # Keep the duplicate-link formula and any screening decisions.
    if (!isTRUE(as.logical(studies[[8]][row])) || existing_link == "")
      range_write(sheet_id, data.frame(pdf_found = TRUE, pdf_link = pdf_link),
        range = paste0("studies!H", sheet_row), col_names = FALSE, reformat = FALSE)
    if (studies[[10]][row] == "") {
      cell <- range_read_cells(sheet_id, range = paste0("studies!J", sheet_row),
        cell_data = "full", discard_empty = FALSE)
      if (is.null(cell$cell[[1]]$userEnteredValue$formulaValue))
        range_write(sheet_id, data.frame(pdf_link = gs4_formula(paste0("=I", sheet_row))),
          range = paste0("studies!J", sheet_row), col_names = FALSE, reformat = FALSE)
    }
    if (studies[[11]][row] %in% c("", "PDF missing"))
      range_write(sheet_id, data.frame(screening_status = "PDF uploaded"),
        range = paste0("studies!K", sheet_row), col_names = FALSE, reformat = FALSE)
  }
  write_csv(pdfs, "analysis/data-derived/recovered_pdf_reviewed.csv")
}
