# analysis/02_fetch_dois.R

# 1. Load packages
library(dplyr)
library(readr)
library(DBI)
library(RSQLite)

# 2. Source helper functions
source("R/endnote_helpers.R")

# 3. Define Paths
included_rds_path <- "analysis/data-derived/included_studies.rds"
endnote_enl_path <- "~/Desktop/PhD files /1st year/Systematic review /endnote/Imported References.enl"
output_csv_path <- "analysis/data-derived/included_studies_with_doi.csv"
output_rds_path <- "analysis/data-derived/included_studies_with_doi.rds"

# 4. Check .enl file tables

file.exists(endnote_enl_path)
file.info(endnote_enl_path)$size
con <- dbConnect(SQLite(), endnote_enl_path)
dbListTables(con)
dbDisconnect(con)

# 4. Read included studies
included_df <- readRDS(included_rds_path)

# 5. Read EndNote references
refs_df <- read_endnote_refs(endnote_enl_path)

# 6. Extract DOI lookup table
doi_lookup_df <- extract_endnote_dois(refs_df)

# 7. Check key columns before joining
names(included_df)
names(doi_lookup_df)

# 8. Join DOI data
included_df <- included_df %>%
  mutate(rec_number = as.character(rec_number))

doi_lookup_df <- doi_lookup_df %>%
  mutate(id = as.character(id))

included_with_doi_df <- included_df %>%
  left_join(
    doi_lookup_df,
    by = c("rec_number" = "id")
  )

# 9. Run checks
names(included_with_doi_df)
cat("Included rows:", nrow(included_df), "\n")
cat("Rows after join:", nrow(included_with_doi_df), "\n")
cat("Rows with DOI:", sum(!is.na(included_with_doi_df$doi)), "\n")
included_with_doi_df %>%
  select(rec_number, title_screening, doi, doi_source_field) %>%
  filter(!is.na(doi)) %>%
  head()

# 10. Extra checks
# How many included rows matched an EndNote record?
cat("Rows matched to EndNote:",
    sum(!is.na(included_with_doi_df$title_endnote)), "\n")

#  How many have DOI?
cat("Rows with DOI:",
    sum(!is.na(included_with_doi_df$doi)), "\n")

#  Where did the DOIs come from?
table(included_with_doi_df$doi_source_field, useNA = "ifany")

included_with_doi_df <- included_with_doi_df %>%
  rename(
    title_screening = title.x,
    title_endnote = title.y
  )

sum(!is.na(included_with_doi_df$title_endnote))

# check unmatched records
included_with_doi_df %>%
  filter(is.na(title_endnote)) %>%
  select(rec_number, title_screening) %>%
  head()

head(included_df$rec_number)
head(doi_lookup_df$id)

range(as.numeric(included_df$rec_number), na.rm = TRUE)
range(as.numeric(doi_lookup_df$id), na.rm = TRUE)
sum(included_df$rec_number %in% doi_lookup_df$id)
head(included_df$foreign_keys, 20)

head(included_df$record_index, 20)
range(as.numeric(included_df$record_index), na.rm = TRUE)

head(included_df$source_row_number, 20)
range(as.numeric(included_df$source_row_number), na.rm = TRUE)

sum(
  tolower(trimws(included_df$title)) %in% tolower(trimws(doi_lookup_df$title)),
  na.rm = TRUE
)

# 11. Join by title

included_df_title <- included_df %>%
  mutate(title_join = tolower(trimws(title)))

doi_lookup_df_title <- doi_lookup_df %>%
  mutate(title_join = tolower(trimws(title)))

included_with_doi_df <- included_df_title %>%
  left_join(
    doi_lookup_df_title,
    by = "title_join",
    suffix = c("_screening", "_endnote")
  )

# 12. checks on the new joined file

cat("Included rows:", nrow(included_df), "\n")
cat("Rows after join:", nrow(included_with_doi_df), "\n")
cat("Rows with EndNote title match:",
    sum(!is.na(included_with_doi_df$title_endnote)), "\n")
cat("Rows with DOI:",
    sum(!is.na(included_with_doi_df$doi)), "\n")

# 13. Create cleaned title for joining (included studies)
# Create a cleaned version of the screening title for matching
# Lowercase + trim spaces to improve consistency when joining
included_df_title <- included_df %>%
  mutate(title_join = tolower(trimws(title)))

# 14. Create cleaned title in EndNote data and remove duplicates
# Keep one record per title to avoid many-to-many joins
doi_lookup_df_title <- doi_lookup_df %>%
  mutate(title_join = tolower(trimws(title))) %>%
  group_by(title_join) %>%
  slice(1) %>%
  ungroup()

# 15. Join screening data with EndNote data using cleaned titles
# left_join keeps all included studies and adds DOI information where available
included_with_doi_df <- included_df_title %>%
  left_join(
    doi_lookup_df_title,
    by = "title_join",
    suffix = c("_screening", "_endnote")
  )

# 16. Check that row count is preserved and assess matching success
cat("Included rows:", nrow(included_df), "\n")
cat("Rows after join:", nrow(included_with_doi_df), "\n")
cat("Rows with EndNote title match:",
    sum(!is.na(included_with_doi_df$title_endnote)), "\n")
cat("Rows with DOI:",
    sum(!is.na(included_with_doi_df$doi)), "\n")

write_csv(included_with_doi_df, output_csv_path)
saveRDS(included_with_doi_df, output_rds_path)

