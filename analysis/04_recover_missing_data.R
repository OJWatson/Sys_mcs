# 1. Load packages
library(dplyr)
library(readr)

# 2. Source helper functions
source("R/endnote_helpers.R")

# 3. Define paths
included_rds_path <- "analysis/data-derived/included_studies.rds"
full_endnote_path <- "~/Desktop/PhD files /1st year/Systematic review /endnote/Mortality in Crisis.enl"
output_csv_path <- "analysis/data-derived/included_studies_with_doi_full_endnote.csv"
output_rds_path <- "analysis/data-derived/included_studies_with_doi_full_endnote.rds"

# 4. Read included studies
included_df <- readRDS(included_rds_path)

# 5. Read full EndNote library
refs_df_new <- read_endnote_refs("~/Desktop/PhD files /1st year/Systematic review /endnote/Mortality in Crisis.enl")

# 6. Extract DOI lookup table
doi_lookup_df_new <- extract_endnote_dois(refs_df_new)

# 7. Match included studies to full EndNote library by record number
included_df_id <- included_df %>%
  mutate(rec_number = as.character(rec_number))

doi_lookup_df_new_id <- doi_lookup_df_new %>%
  mutate(id = as.character(id))

included_with_doi_by_id <- included_df_id %>%
  left_join(
    doi_lookup_df_new_id,
    by = c("rec_number" = "id"),
    suffix = c("_screening", "_endnote")
  )

# 8. Check if matching worked
cat("Included rows:", nrow(included_df_id), "\n")
cat("Rows after ID join:", nrow(included_with_doi_by_id), "\n")
cat("Rows matched by ID:",
    sum(!is.na(included_with_doi_by_id$title_endnote)), "\n")
cat("Rows with DOI:",
    sum(!is.na(included_with_doi_by_id$doi)), "\n")

# 9. Identify studies still missing DOI after full EndNote match
missing_doi_after_full_endnote_df <- included_with_doi_by_id %>%
  filter(is.na(doi))

cat("Rows still missing DOI:",
    nrow(missing_doi_after_full_endnote_df), "\n")

# 10. Check duplicated DOIs

doi_duplicate_summary <- included_with_doi_by_id %>%

  filter(!is.na(doi)) %>%

  count(doi) %>%

  filter(n > 1) %>%

  summarise(

    unique_duplicate_dois = n(),

    total_rows_with_duplicate_dois = sum(n),

    rows_to_remove = sum(n) - n()

  )

print(doi_duplicate_summary)

# 11. Inspect duplicated DOI records

duplicated_doi_records <- included_with_doi_by_id %>%

  filter(!is.na(doi)) %>%

  group_by(doi) %>%

  filter(n() > 1) %>%

  ungroup() %>%

  arrange(doi)

View(duplicated_doi_records)

# 12. Remove duplicated DOIs, keeping one row per DOI

# Keep all rows without DOI because they cannot be deduplicated by DOI

included_with_doi_dedup_df <- included_with_doi_by_id %>%

  filter(is.na(doi)) %>%

  bind_rows(

    included_with_doi_by_id %>%

      filter(!is.na(doi)) %>%

      distinct(doi, .keep_all = TRUE)

  )

# 13. Check row counts after deduplication

cat("Rows before DOI deduplication:",

    nrow(included_with_doi_by_id), "\n")

cat("Rows after DOI deduplication:",

    nrow(included_with_doi_dedup_df), "\n")

cat("Rows removed:",

    nrow(included_with_doi_by_id) - nrow(included_with_doi_dedup_df), "\n")

# 14. Save deduplicated DOI dataset

write_csv(included_with_doi_dedup_df, output_csv_path)

saveRDS(included_with_doi_dedup_df, output_rds_path)

