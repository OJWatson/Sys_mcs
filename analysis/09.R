#Get the missing pdfs journal name
# 1. Load packages
library(dplyr)
library(readr)
library(stringr)

# 2. Source EndNote helper
source("R/endnote_helpers.R")

# 3. Define paths
final_pdf_rds_path <- "analysis/data-derived/included_studies_with_pdf_full_endnote.rds"
full_endnote_path <- "~/Desktop/PhD files /1st year/Systematic review /endnote/Mortality in Crisis.enl"

missing_pdf_with_journal_csv <- "analysis/data-derived/missing_pdf_with_journal_names.csv"
missing_pdf_journal_summary_csv <- "analysis/data-derived/missing_pdf_journal_summary.csv"

# 4. Read final pdf data set
pdf_df <- readRDS(final_pdf_rds_path)

# 5. Read full EndNote library again
refs_df_new <- read_endnote_refs(full_endnote_path)

#6. Create journal lookup table from EndNote
journal_lookup_df <- refs_df_new %>%
  mutate(id = as.character(id)) %>%
  select(
    id,
    journal_name = secondary_title
  ) %>%
  distinct(id, .keep_all = TRUE)

pdf_df_with_journal <- pdf_df %>%
  mutate(rec_number = as.character(rec_number)) %>%
  left_join(
    journal_lookup_df,
    by = c("rec_number" = "id"))

# 8. Keep missing PDFs only
missing_pdf_with_journal_df <- pdf_df_with_journal %>%
  filter(pdf_found == FALSE | is.na(pdf_found)) %>%
  mutate(
    journal_name_clean = case_when(
      is.na(journal_name) | trimws(journal_name) == "" ~ "Unknown journal",
      TRUE ~ journal_name
    )
  )

nrow(missing_pdf_df)
names(missing_pdf_df)
missing_pdf_df$journal_name


#10. Summaries missing PDFs by journal name
missing_pdf_journal_summary <- missing_pdf_with_journal_df %>%
  count(journal_name_clean, sort = TRUE)

missing_pdf_journal_summary
head(missing_pdf_journal_summary, 30)

missing_pdf_df[missing_pdf_df$journal_name == "Kidney International Reports", "title_screening"]

missing_pdf_df %>%
  filter(journal_name == "Kidney International Reports") %>%
select("title_screening", "doi")

# Categorization of missing pdf publications
missing_pdf_df <- missing_pdf_df %>%

  mutate(

    publication_type = case_when(

      str_detect(journal_name, regex("Conference|Congress|Meeting|Symposium|ATS|AAN|ID Week|AFENET|SIOP",
                                     ignore_case = TRUE)) ~
        "Conference abstract",

      str_detect(journal_name, regex("Demographic and Health Survey|Enquete Demographique",
                                     ignore_case = TRUE)) ~
        "National survey report",

      str_detect(journal_name, regex("Weekly Epidemiological Record|Disease Surveillance|Rapid risk assessment",
                                     ignore_case = TRUE)) ~
        "Surveillance report",

      str_detect(journal_name, regex("World Bank|Working Paper|National Academies Press|WIDER",
                                     ignore_case = TRUE)) ~
        "Working paper / report",

      str_detect(journal_name, regex("WHO|World Health Organization",
                                     ignore_case = TRUE)) ~
        "WHO report",

      str_detect(journal_name, regex("medRxiv|SSRN",
                                     ignore_case = TRUE)) ~
        "Preprint",

      str_detect(journal_name, regex("clinicaltrials.gov",
                                     ignore_case = TRUE)) ~
        "Clinical trial registry",

      TRUE ~
        "Peer-reviewed journal"
    )
  )

missing_pdf_publication_summary <- missing_pdf_df %>%

  count(publication_type, sort = TRUE) %>%

  mutate(
    percent = round(n / sum(n) * 100, 1)
  )

missing_pdf_publication_summary

top_missing_pdf_journals <- missing_pdf_df %>%

  count(journal_name_clean, sort = TRUE) %>%

  slice_head(n = 20)

top_missing_pdf_journals

#Join journal names onto the missing DOI records
missing_doi_with_journal_df <- missing_doi_df %>%
  left_join(
    refs_df_new %>%
      select(
        id,
        secondary_title
      ) %>%
      mutate(id = as.character(id)),
    by = c("rec_number" = "id")
  ) %>%
  rename(
    journal_name = secondary_title
  )

#summaries journals
missing_doi_journal_summary <- missing_doi_with_journal_df %>%

  count(journal_name, sort = TRUE)

missing_doi_journal_summary

View(missing_doi_journal_summary)
head(missing_doi_journal_summary, 30)


missing_doi_with_journal_df %>%
  mutate(
    source_type = case_when(
      str_detect(
        journal_name,
        regex("Conference|Congress|Meeting|Symposium", ignore_case = TRUE)
      ) ~ "Conference abstract",
      TRUE ~ "Other"
    )
  ) %>%
  count(source_type)

# Combine missing pdfs and missing dois

final_pdf_df <- final_pdf_df %>%
  mutate(
    missing_category = case_when(
      is.na(doi) ~
        "Missing DOI",
      !is.na(doi) &
        (pdf_found == FALSE | is.na(pdf_found)) ~
        "DOI present, PDF not retrieved",
      pdf_found == TRUE ~
        "PDF retrieved"
    )
  )
names(final_pdf_df)
# summarise the results by journal

pdf_df_with_journal <- pdf_df_with_journal %>%
  mutate(
    journal_name_clean = case_when(
      is.na(journal_name) | trimws(journal_name) == "" ~ "Unknown journal",
      TRUE ~ journal_name
    ),

    missing_category = case_when(
      is.na(doi) ~ "Missing DOI",

      !is.na(doi) &
        (pdf_found == FALSE | is.na(pdf_found)) ~
        "DOI present, PDF not retrieved",

      pdf_found == TRUE ~ "PDF retrieved"
    )
  )

journal_missing_breakdown <- pdf_df_with_journal %>%
  filter(missing_category != "PDF retrieved") %>%
  count(
    journal_name_clean,
    missing_category,
    sort = TRUE
  )

View(journal_missing_breakdown)

journal_missing_wide <- journal_missing_breakdown %>%
  tidyr::pivot_wider(
    names_from = missing_category,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(
    total_missing_pdf = `Missing DOI` + `DOI present, PDF not retrieved`
  ) %>%
  arrange(desc(total_missing_pdf))

View(journal_missing_wide)

#11. Save outputs
write_csv(missing_pdf_with_journal_df,"analysis/data-derived/missing_pdf_with_actual_journal_names.csv")

write_csv(missing_pdf_journal_summary,"analysis/data-derived/missing_pdf_actual_journal_summary.csv")

View(missing_pdf_journal_summary)
