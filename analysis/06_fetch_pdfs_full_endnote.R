# 1. Load packages
library(dplyr)
library(readr)

# 2. Source helper functions
source("R/pdf_helpers.R")

# 3. Define paths
input_rds_path <- "analysis/data-derived/included_studies_with_doi_full_endnote.rds"

pdf_download_dir <- "analysis/pdf_download"

pdf_results_csv_path <- "analysis/data-derived/pdf_download_results_full_endnote.csv"
final_csv_path <- "analysis/data-derived/included_studies_with_pdf_full_endnote.csv"
final_rds_path <- "analysis/data-derived/included_studies_with_pdf_full_endnote.rds"

# 4. Create folder
dir.create(pdf_download_dir, recursive = TRUE, showWarnings = FALSE)

# 5. Read DOI-enriched table
doi_df <- readRDS(input_rds_path)

# 6. Keep rows with DOI
lookup_df <- doi_df %>%
  filter(!is.na(doi), doi != "")

cat("Rows with DOI to check:", nrow(lookup_df), "\n")

# 7. Create results list
pdf_results_list <- list()

# 8. Loop over DOI rows
for (i in seq_len(nrow(lookup_df))) {

  record_id <- lookup_df$record_index[i]
  doi_value <- lookup_df$doi[i]

  pdf_path <- file.path(pdf_download_dir, paste0(record_id, ".pdf"))

  cat("Checking", i, "of", nrow(lookup_df), "DOI:", doi_value, "\n")

  # Skip if PDF already exists
  if (file.exists(pdf_path)) {

    pdf_results_list[[i]] <- data.frame(
      record_index = record_id,
      doi = doi_value,
      pdf_found = TRUE,
      pdf_source = "already_downloaded",
      pdf_url = NA_character_,
      pdf_path = pdf_path,
      stringsAsFactors = FALSE
    )

    next
  }

  # Query sources
  openalex_urls <- query_openalex_pdf_urls(doi_value)
  europepmc_urls <- query_europepmc_pdf_urls(doi_value)

  candidate_urls <- c(openalex_urls, europepmc_urls)
  candidate_sources <- c(
    rep("OpenAlex", length(openalex_urls)),
    rep("Europe PMC", length(europepmc_urls))
  )

  # Remove duplicate URLs
  keep <- !duplicated(candidate_urls)
  candidate_urls <- candidate_urls[keep]
  candidate_sources <- candidate_sources[keep]

  pdf_found <- FALSE
  pdf_source <- NA_character_
  pdf_url <- NA_character_

  # Try downloading
  if (length(candidate_urls) > 0) {
    for (j in seq_along(candidate_urls)) {

      success <- download_pdf_with_retry(candidate_urls[j], pdf_path)

      if (success) {
        pdf_found <- TRUE
        pdf_source <- candidate_sources[j]
        pdf_url <- candidate_urls[j]
        break
      }
    }
  }

  # Store results
  pdf_results_list[[i]] <- data.frame(
    record_index = record_id,
    doi = doi_value,
    pdf_found = pdf_found,
    pdf_source = pdf_source,
    pdf_url = pdf_url,
    pdf_path = ifelse(pdf_found, pdf_path, NA_character_),
    stringsAsFactors = FALSE
  )
}

# 9. Combine results
pdf_results_df <- bind_rows(pdf_results_list)

# 10. Merge results back to main table
final_pdf_df <- doi_df %>%
  left_join(pdf_results_df, by = c("record_index", "doi"))

# 11. Summary checks
cat("DOI rows checked:", nrow(pdf_results_df), "\n")
cat("PDFs already downloaded:",
    sum(pdf_results_df$pdf_source == "already_downloaded", na.rm = TRUE), "\n")
cat("New PDFs downloaded:",
    sum(pdf_results_df$pdf_found & pdf_results_df$pdf_source != "already_downloaded", na.rm = TRUE), "\n")
cat("Total PDFs found:",
    sum(pdf_results_df$pdf_found, na.rm = TRUE), "\n")
cat("PDFs not found:",
    sum(!pdf_results_df$pdf_found, na.rm = TRUE), "\n")

# 12. Save outputs
write_csv(pdf_results_df, pdf_results_csv_path)
write_csv(final_pdf_df, final_csv_path)
saveRDS(final_pdf_df, final_rds_path)
