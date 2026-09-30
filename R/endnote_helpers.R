library(DBI)
library(RSQLite)
library(stringr)

extract_doi_from_text <- function(text) {
  if (is.na(text) || !nzchar(trimws(as.character(text)))) {
    return(NA_character_)
  }

  text <- as.character(text)
  text <- tryCatch(utils::URLdecode(text), error = function(e) text)
  doi <- stringr::str_extract(text, "10\\.[0-9]{4,9}/[-._;()/:A-Za-z0-9]+")

  if (is.na(doi)) {
    return(NA_character_)
  }

  doi <- sub("[\\.,;\\)\\]\\}\\\"']+$", "", doi)
  tolower(doi)
}

read_endnote_refs <- function(path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  refs_df <- DBI::dbReadTable(con, "refs")
  refs_df$id <- as.character(refs_df$id)
  refs_df
}

extract_endnote_dois <- function(refs_df) {
  doi_source_fields <- c(
    "url",
    "electronic_resource_number",
    "doi",
    "accession_number",
    "custom_7",
    "notes",
    "research_notes",
    "abstract",
    "title",
    "pages"
  )

  doi_source_fields <- intersect(doi_source_fields, names(refs_df))

  doi_lookup_df <- refs_df
  doi_lookup_df$id <- as.character(doi_lookup_df$id)

  if ("title" %in% names(doi_lookup_df)) {
    doi_lookup_df$title <- as.character(doi_lookup_df$title)
  }

  if ("year" %in% names(doi_lookup_df)) {
    doi_lookup_df$year <- as.character(doi_lookup_df$year)
  }

  doi_lookup_df$doi <- NA_character_
  doi_lookup_df$doi_source_field <- NA_character_

  for (i in seq_len(nrow(doi_lookup_df))) {
    for (field in doi_source_fields) {
      value <- doi_lookup_df[[field]][i]
      doi_value <- extract_doi_from_text(value)

      if (!is.na(doi_value)) {
        doi_lookup_df$doi[i] <- doi_value
        doi_lookup_df$doi_source_field[i] <- field
        break
      }
    }
  }

  keep_columns <- c(
    "id",
    "trash_state",
    "title",
    "year",
    "name_of_database",
    "url",
    "accession_number",
    "electronic_resource_number",
    "doi",
    "doi_source_field"
  )

  keep_columns <- intersect(keep_columns, names(doi_lookup_df))
  doi_lookup_df[, keep_columns, drop = FALSE]
}

