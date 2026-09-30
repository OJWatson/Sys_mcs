# 1. Load packages
install.packages("DBI")

install.packages("RSQLite")

library(DBI)
library(RSQLite)
library(dplyr)
library(readr)
library(stringr)
library(tidyr)


# 2. Define paths
input_rds_path <- "analysis/data-derived/included_studies_with_doi_full_endnote.rds"
missing_doi_csv_path <- "analysis/data-derived/missing_doi_categorised.csv"
missing_doi_url_type_summary_path <- "analysis/data-derived/missing_doi_url_type_summary.csv"
missing_doi_ovid_database_summary_path <- "analysis/data-derived/missing_doi_ovid_database_summary.csv"
missing_doi_study_type_summary_path <- "analysis/data-derived/missing_doi_study_type_summary.csv"

# 3. Read DOI dataset
df <- readRDS(input_rds_path)

# 4. Keep rows still missing DOI
missing_doi_df <- df %>%
  filter(is.na(doi))

missing_doi_df <- df %>%
  filter(is.na(doi))

head(missing_doi_df)

cat("Rows missing DOI:", nrow(missing_doi_df), "\n")

# 5. Split URL field into one row per URL
missing_doi_urls_long <- missing_doi_df %>%
  mutate(url = str_replace_all(url, "\r", ";")) %>%
  separate_rows(url, sep = ";") %>%
  mutate(
    url = trimws(url),
    url_lower = str_to_lower(url)
  )

# Look into URLs
missing_doi_urls_long %>%
  select(
    title_screening,
    url
  ) %>%
  print(n = 20)

missing_doi_urls_long %>%
  select(
    record_index,
    title_screening,
    url
  ) %>%
  View()

# Read full length of th urls
missing_doi_urls_long$url[]


# 6. Categorise URL types
missing_doi_urls_long <- missing_doi_urls_long %>%
  mutate(
    url_type = case_when(
      is.na(url_lower) | url_lower == "" ~ "No URL recorded",
      str_detect(url_lower, "ovidsp") ~ "Ovid platform link",
      str_detect(url_lower, "primo-explore|exlibrisgroup") ~ "Library resolver link",
      str_detect(url_lower, "doi.org") ~ "Direct DOI link",
      str_detect(url_lower, "clinicaltrials.gov|isrctn|trialsearch|pactr|chictr|anzctr") ~ "Clinical trial registry",
      str_detect(url_lower, "who.int") ~ "WHO / report",
      str_detect(url_lower, "pdf") ~ "Direct PDF link",
      TRUE ~ "Other publisher / unknown"
    )
  )

# 7. Summarise URL types
missing_doi_url_type_summary <- missing_doi_urls_long %>%
  count(url_type, sort = TRUE) %>%
  mutate(percent = round(n / sum(n) * 100, 1))

print(missing_doi_url_type_summary)

# 8. updated categorization:
missing_doi_url_patterns <- missing_doi_urls_long %>%
  mutate(
    url_lower = tolower(url),
    url_pattern = case_when(
      str_detect(url_lower, "do=") ~

        "Malformed DOI metadata record",

      str_detect(url_lower, "d=emctr&an") ~

        "Embase conference abstract accession record",

      str_detect(url_lower, "d=emed[0-9]+&an") ~

        "Embase indexed accession record",

      str_detect(url_lower, "d=cagh[0-9]*&an") ~

        "CAB Abstracts accession record",

      str_detect(url_lower, "d=pmnm[0-9]*&an") ~

        "Ovid MEDLINE/PubMed accession record",

      str_detect(url_lower, "d=med[0-9]*&an|d=medp&an|d=medl&an") ~

        "Ovid MEDLINE accession record",

      str_detect(url_lower, "d=empp&an") ~

        "Ovid Embase publication record",

      str_detect(url_lower, "primo-explore|openurl|exlibrisgroup") ~

        "Library resolver metadata link",

      TRUE ~

        "Other / unclear URL pattern"

    ),

    likely_reason = case_when(

      str_detect(url_lower, "do=") ~

        "Malformed DOI-like metadata present but not a valid article DOI",

      str_detect(url_lower, "d=emctr&an") ~

        "Conference abstract record; DOI may not exist or may not be exported",

      str_detect(url_lower, "d=cagh[0-9]*&an") ~

        "CAB/Global Health record uses accession number rather than DOI",

      str_detect(url_lower, "d=emed[0-9]+&an|d=pmnm[0-9]*&an|d=med[0-9]*&an|d=medp&an|d=medl&an|d=empp&an") ~

        "Ovid record uses database accession number rather than DOI",

      str_detect(url_lower, "primo-explore|openurl|exlibrisgroup") ~

        "Library resolver contains bibliographic metadata but no DOI",

      TRUE ~

        "Needs manual review"

    )

  )

# 9. Summarise URL types
missing_doi_url_patterns %>%

  count(url_pattern, likely_reason, sort = TRUE)


# 10. Classify broad study type from title
missing_doi_study_type_summary <- missing_doi_df %>%
  mutate(
    title_lower = str_to_lower(coalesce(title_screening, "")),
    study_type = case_when(
      str_detect(title_lower, "protocol") ~ "Protocol",
      str_detect(title_lower, "trial") ~ "Trial",
      str_detect(title_lower, "randomised|randomized") ~ "Randomised study",
      str_detect(title_lower, "systematic review|meta-analysis") ~ "Systematic review",
      str_detect(title_lower, "review") ~ "Review",
      str_detect(title_lower, "modelling|modeling") ~ "Modelling study",
      str_detect(title_lower, "outbreak") ~ "Outbreak report",
      str_detect(title_lower, "epidemiolog") ~ "Epidemiological study",
      str_detect(title_lower, "case report") ~ "Case report",
      TRUE ~ "Other / unclear"
    )
  ) %>%
  count(study_type, sort = TRUE)

print(missing_doi_study_type_summary)

# 11. Create final categorised missing DOI table
missing_doi_audit_df <- missing_doi_url_patterns %>%
  select(

    record_index,

    rec_number,

    title_screening,

    title_endnote,

    year,

    doi,

    url,

    url_pattern,

    likely_reason

  ) %>%



  arrange(url_pattern, title_screening)


con <- dbConnect(SQLite(), full_endnote_path)
dbListTables(con)
dbDisconnect(con)

# 4. Read EndNote references
refs_df <- read_endnote_refs(full_endnote_path)
refs_doi_df <- refs_df %>% filter(id %in% missing_doi_df$rec_number)

head(refs_doi_df)

# updated summary

missing_doi_chain_summary <- missing_doi_audit_df %>%

  mutate(

    url_lower = tolower(url),



    url_chain_pattern = case_when(

      str_detect(url_lower, "d=emctr&an") ~ "D=emctr&AN (Embase conference abstract)",

      str_detect(url_lower, "d=emed[0-9]+&an") ~ "D=emedXX&AN (Embase indexed record)",

      str_detect(url_lower, "d=cagh[0-9]*&an") ~ "D=cagh&AN (CAB Abstracts accession record)",

      str_detect(url_lower, "d=med[0-9]*&an") ~ "D=medXX&AN (MEDLINE accession record)",

      str_detect(url_lower, "d=pmnm[0-9]*&an") ~ "D=pmnm&AN (PubMed/MEDLINE record)",

      str_detect(url_lower, "d=empp&an") ~ "D=empp&AN (Embase publication record)",

      str_detect(url_lower, "do=") ~ "DO= field present (Malformed DOI metadata)",

      str_detect(url_lower, "primo-explore|openurl") ~ "Library resolver / OpenURL link",

      TRUE ~ "Other / unclear"

    )

  ) %>%

  count(url_chain_pattern, sort = TRUE) %>%

  mutate(

    percent = round(n / sum(n) * 100, 1)

  )

missing_doi_chain_summary

# 12. Save outputs
write_csv(missing_doi_categorised_df, missing_doi_csv_path)
write_csv(missing_doi_audit_df,"analysis/data-derived/missing_doi_audit_table.csv")
saveRDS(missing_doi_audit_df, "analysis/data-derived/missing_doi_audit_table.rds")

