# 1. Load packages
library(dplyr)
library(readr)
library(stringr)

# 2. Define paths

input_rds_path <- "analysis/data-derived/included_studies_with_pdf_full_endnote.rds"
missing_pdf_csv_path <- "analysis/data-derived/missing_pdf_by_journal.csv"
missing_pdf_journal_summary_path <- "analysis/data-derived/missing_pdf_journal_summary.csv"
missing_pdf_database_summary_path <- "analysis/data-derived/missing_pdf_database_summary.csv"

names(pdf_df)
head(pdf_df)
# 3. Read final PDF dataset
pdf_df <- readRDS(input_rds_path)

# 4. Keep studies where PDF was not found
missing_pdf_df <- pdf_df %>%
  filter(pdf_found == FALSE | is.na(pdf_found))

cat("Rows with missing PDFs:", nrow(missing_pdf_df), "\n")

missing_pdf_journal_summary <- missing_pdf_df %>%
  count(name_of_database, sort = TRUE)
missing_pdf_journal_summary

# 6. Create journal/source fields

missing_pdf_df <- missing_pdf_df %>%
  mutate(
    journal_source = case_when(
      !is.na(name_of_database) & name_of_database != "" ~ name_of_database,
      TRUE ~ "Unknown journal/source"
    ),
    journal_source_clean = str_to_lower(trimws(journal_source))
  )

# 7. Summarise missing PDFs by journal/source

missing_pdf_journal_summary <- missing_pdf_df %>%
  count(journal_source, sort = TRUE)
missing_pdf_journal_summary

# 8. Clean results:

missing_pdf_journal_summary_clean <- missing_pdf_df %>%

  mutate(

    journal_source_clean = case_when(

      is.na(journal_source) ~ "Unknown journal/source",

      trimws(journal_source) == "" ~ "Unknown journal/source",

      str_detect(journal_source, "UI -") ~ "Corrupted metadata record",

      str_detect(journal_source, "EmbUI") ~ "Corrupted metadata record",

      str_detect(journal_source, "EmbaUI") ~ "Corrupted metadata record",

      str_detect(journal_source, "EmbaseUI") ~ "Corrupted metadata record",

      TRUE ~ journal_source

    )

  ) %>%

  count(journal_source_clean, sort = TRUE)

# 9. view the final summary:
missing_pdf_journal_summary_clean

# 10. Save missing PDF categorisation table

missing_pdf_review_df <- missing_pdf_df %>%

  mutate(

    journal_source_clean = case_when(

      str_detect(name_of_database, "Embase") ~ "Embase",

      str_detect(name_of_database, "MEDLINE") ~ "MEDLINE",

      str_detect(name_of_database, "Global Health") ~ "Global Health",

      str_detect(name_of_database, "UI -|EUI -|EmbUI|EmbaUI") ~

        "Corrupted metadata record",

      is.na(name_of_database) | name_of_database == "" ~

        "Unknown journal/source",

      TRUE ~ name_of_database

    )

  )

# save outputs:

write_csv(missing_pdf_review_df, "analysis/data-derived/missing_pdf_review_table.csv")
saveRDS(missing_pdf_review_df,"analysis/data-derived/missing_pdf_review_table.rds")
