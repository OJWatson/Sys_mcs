# Coding Step Guide

## 1. Purpose of the repository

The repository is meant to do three practical jobs.

1. Read in a screened dataset and keep only the studies that were included.
2. Link those included studies back to the EndNote library and attach DOI information.
3. Use the DOI values to look for open-access PDF links and record which PDFs were found.

## 2. Final file structure to aim for

A simple version of the repository should revolve around these files and folders.

```text
README.md
DESCRIPTION
R/
  endnote_helpers.R
  pdf_helpers.R
analysis/
  01_fetch_all_studies.R
  02_fetch_dois.R
  03_fetch_pdfs.R
  data-raw/
  data-derived/
  pdf_download/
```

The exact folder names above matter because the scripts use explicit relative paths.

The general idea is:
- `analysis/` holds the three main scripts you run in order.
- `R/` holds a small number of reusable functions.
- `analysis/data-raw/` holds raw input files.
- `analysis/data-derived/` holds processed output files.
- `analysis/pdf_download/` holds downloaded PDF files.

### Folders and files you should create first

Before writing the scripts, you should set up the repository so the paths already exist.

You should create:
- a top-level `R/` folder
- an `analysis/` folder
- `analysis/data-raw/`
- `analysis/data-derived/`
- `analysis/pdf_download/`

You should then create these empty script files:
- `analysis/01_fetch_all_studies.R`
- `analysis/02_fetch_dois.R`
- `analysis/03_fetch_pdfs.R`
- `R/endnote_helpers.R`
- `R/pdf_helpers.R`

This matters because the scripts use explicit relative paths such as:
- `source("R/endnote_helpers.R")`
- `source("R/pdf_helpers.R")`
- `file.path("analysis", "data-derived", "included_studies.rds")`
- `file.path("analysis", "pdf_download", paste0(record_index, ".pdf"))`

If the folder structure is different, those paths will not work.

## 3. What you should understand before writing code

The workflow moves through a sequence of data frames.

At each stage, you should know:
- what file is being read
- what object is being created
- what columns matter
- whether rows are being filtered, joined, or extended with new columns
- what file is being written at the end of the script

The code will be much easier to follow if each script is written as:
- load packages
- define paths
- read data
- inspect data
- transform data
- check the data with a few summaries
- save data

## 4. Core objects and what they are for

These are the main objects you will probably create during the workflow.

| Object name | Where it is created | What it represents | Why it is needed |
| --- | --- | --- | --- |
| `screened_df` | `01_fetch_all_studies.R` | The full screening dataset read from file | Starting point for identifying included studies |
| `included_df` | `01_fetch_all_studies.R` | Only the included rows from the screening dataset | Main input for step 02 |
| `refs_df` | `02_fetch_dois.R` | The EndNote `refs` table read from the `.enl` SQLite database | Contains EndNote metadata linked to included studies |
| `doi_lookup_df` | `02_fetch_dois.R` | A DOI-focused table built from `refs_df` | Holds the identifier information that will be merged onto included studies |
| `included_with_doi_df` | `02_fetch_dois.R` | Included studies after DOI and EndNote fields have been added | Main input for step 03 |
| `pdf_results_df` | `03_fetch_pdfs.R` | One row per study with PDF search/download result fields | Records whether a PDF was found and where it was saved |
| `final_pdf_df` | `03_fetch_pdfs.R` | DOI-enriched study table with PDF result columns attached | Final output for this stage of the project |

These names do not have to be exact, but you should be encouraged to use names that describe the object clearly.

## 5. Packages that may be needed

For the simplified version of the repository, these are the main packages likely needed

```r
install.packages(c(
  "dplyr",
  "readr",
  "googledrive",
  "DBI",
  "RSQLite",
  "stringr",
  "httr2",
  "jsonlite"
))
```

### What each package is for

- `dplyr`
  - filtering rows
  - adding new columns
  - selecting columns
  - joining tables

- `readr`
  - reading CSV files
  - writing CSV files

- `googledrive`
  - downloading the screening file if you are asked to get it directly from Google Drive

- `DBI` and `RSQLite`
  - reading the EndNote `.enl` SQLite database inside a helper function

- `stringr`
  - extracting DOI patterns inside a helper function

- `httr2`
  - making web requests to OpenAlex, Europe PMC, and candidate PDF URLs

- `jsonlite`
  - turning API responses into R objects inside helper functions

You do not need to master all of these at once, but these will be helpful for a lot of what you need to do.

## 6. General coding conventions for this repository

The code should be written so that you can follow it from top to bottom without having to guess why a block exists.

### Recommended conventions

- Use short step comments such as:
  - `# 1. Load packages`
  - `# 2. Define file paths`
  - `# 3. Read the screening data`
  - `# 4. Filter to included studies`

- Use object names that show what the object contains.

- Use one main action per block of code.

- Save outputs at the end of each script.

- Do a short check after each major result, for example:
  - total rows read
  - included rows kept
  - rows matched to EndNote
  - DOI values found
  - PDFs found

## 7. Script 1: `analysis/01_fetch_all_studies.R`

### Aim of the file

This file should create a clean table containing only the studies that were included after screening.

### Main input

The main input is the screening dataset.

This comes from:
- a Google Drive file that is downloaded into `analysis/data-raw/`

### Main output

The main output should be one file containing included studies only.

For example:
- `analysis/data-derived/included_studies.csv`
- `analysis/data-derived/included_studies.rds`

The CSV is easy to inspect manually.
The RDS keeps R data types exactly and is convenient for the next step.

### Detailed steps for this file

#### 1. Load packages

Load only the packages used in the script.

Likely packages:
- `dplyr`
- `readr`
- `googledrive`

#### 2. Define file paths

You should create variables for:
- the raw input path
- the derived output CSV path
- the derived output RDS path

This makes the file easier to edit later.

#### 3. Read the screening data

You should read the screening file into `screened_df`.

The main purpose here is simply to bring the dataset into R as a data frame.

#### 4. Inspect the data

Before filtering anything, you should look at:
- `names(screened_df)` to see all columns and what is the name of the include column
- create a variable for the name of the include_column, e.g. `include_column <- "..."`
- `table(screened_df[[include_column]], useNA = "ifany")` to see all decision values 

The purpose of this inspection step is to confirm two things:
- which column stores the decision
- what exact value means the study is included

This is important because filtering only works if you use the exact text that appears in the data.

#### 5. Declare the decision column and include value explicitly

Once you have inspected the data, you should write those strings directly into the script.

For example:

```r
include_column <- "decision"
include_value <- "Include"
```

The point is to make the logic visible and specific.

#### 6. Filter to included studies

You should create `included_df` by keeping only rows where the decision column equals the include value.

The aim of the filter is to reduce the full screening dataset to the subset that matters for the next stage of the review.

#### 7. Check the result

You should print:
- total number of rows in `screened_df`
- total number of rows in `included_df`

This confirms that the filter worked and gives an immediate sense of the size of the included subset.

#### 8. Save the included studies

You should write:
- a CSV version for inspection
- an RDS version for the next script

## 8. Script 2: `analysis/02_fetch_dois.R`

### Aim of the file

This file should attach EndNote and DOI information to the included studies from step 01.

The logic should be easy to follow:
- read the included studies
- use helper functions to read EndNote records
- use helper functions to create a DOI lookup table
- merge DOI information onto the included studies
- save the result

### Main inputs

- `analysis/data-derived/included_studies.rds` or `.csv`
- the EndNote `.enl` file in `analysis/data-raw/`

### Main output

A DOI-enriched version of the included studies table.

For example:
- `analysis/data-derived/included_studies_with_doi.csv`
- `analysis/data-derived/included_studies_with_doi.rds`

### Other functions you will use in this file

Before you write `analysis/02_fetch_dois.R`, you should create a helper file called `R/endnote_helpers.R`.

This file should live in the top-level `R/` folder.

The step 02 script will load it with:

```r
source("R/endnote_helpers.R")
```

You can put the exact file contents below into that helper file.

```r
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
```

### How script 2 uses this helper file

You do not need to call every function in this file directly.

You should understand the roles like this:

- `read_endnote_refs(endnote_enl_path)`
  - reads the EndNote `.enl` SQLite database
  - returns the `refs` table as `refs_df`

- `extract_endnote_dois(refs_df)`
  - takes the full EndNote table
  - searches across several EndNote fields for DOI text
  - returns a smaller DOI lookup table that can be joined onto the included studies

- `extract_doi_from_text(text)`
  - is used inside the helper file
  - the main step 02 script does not need to call it directly

The teaching point is that the main script stays simple while the more technical DOI extraction logic lives in `R/endnote_helpers.R`.

### Objects to create in this file

#### `included_df`
The included studies table created in step 01.

#### `endnote_enl_path`
A string containing the location of the `.enl` file.

#### `refs_df`
The full EndNote `refs` table returned by the helper function.

#### `doi_lookup_df`
A DOI-focused data frame created from `refs_df`.

#### `included_with_doi_df`
The final joined table containing included studies plus DOI fields.

### The join you need to understand

This is the main join in the repository.

The screening data contains a record identifier, usually `rec_number`.
The EndNote lookup table contains an identifier column, usually `id`.

The aim is to join:
- `included_df$rec_number`
with
- `doi_lookup_df$id`

The purpose of the join is to bring DOI-related fields from EndNote onto the included studies table.

A `left_join()` is usually the correct join here because:
- every included study should stay in the output
- EndNote and DOI fields are added where available
- unmatched rows stay visible and can be counted later

### Detailed steps for this file

#### 1. Load packages

The script itself will probably need:
- `dplyr`
- `readr`

The helper functions may use other packages, but the main script should stay short.

#### 2. Source the helper file if needed

If the helper functions are stored in `R/`, load them in a simple and obvious way.

You should be able to see immediately that the script depends on those functions.

#### 3. Define paths

You should define:
- the input path for included studies
- the input path for the EndNote `.enl` file
- the output CSV path
- the output RDS path

#### 4. Read the included studies

Read the output from step 01 into `included_df`.

This object represents the studies that are in scope for the DOI stage.

#### 5. Read the EndNote `refs` table

Call the helper function to create `refs_df`.

The purpose of this step is to bring in the EndNote metadata that corresponds to the screened records.

#### 6. Build the DOI lookup table

Call the DOI helper function to create `doi_lookup_df`.

This object should be smaller and more focused than `refs_df`. It should contain the columns you need for joining and for simple identifier reporting.

#### 7. Check the key columns before joining

Before joining, confirm:
- which column in `included_df` holds the EndNote record number
- which column in `doi_lookup_df` holds the EndNote ID

This avoids writing a join that looks correct but uses the wrong columns.

#### 8. Merge DOI data onto included studies

Use a `left_join()`.

The purpose of the merge is to keep the full included table and extend it with identifier information.

This is an important distinction:
- filtering changes which rows are present
- joining keeps rows and adds columns from another table

In this step the aim is not to remove rows. The aim is to attach more information to each included study.

#### 9. Do simple summary checks

After the join, calculate and print:
- number of included rows
- number of rows that matched an EndNote record
- number of rows with a DOI value

These checks tell you whether the join worked and how much identifier coverage you achieved.

#### 10. Save the DOI-enriched data frame

Write both CSV and RDS outputs.

This gives you:
- a human-readable file for checking
- a clean input for the PDF stage

### Optional columns worth carrying forward

Depending on how the helper is written, the joined output can carry fields such as:
- `doi`
- `doi_source_field`
- `url`
- `accession_number`
- `electronic_resource_number`
- `title`
- `year`

The aim is to keep enough metadata to support later PDF retrieval and manual checking.

### Why this file matters

This file is the bridge between the screening output and the PDF retrieval stage. It creates the identifier table that makes later lookup possible.

## 9. Script 3: `analysis/03_fetch_pdfs.R`

### Aim of the file

This file should take the DOI-enriched included studies table, try to find PDF links for each DOI, and record the result.

The script should read as a clear loop over studies.

### Main input

- `analysis/data-derived/included_studies_with_doi.rds` or `.csv`

### Main outputs

At minimum:
- a final table with PDF result columns added
- downloaded PDF files in `analysis/pdf_download/`

Useful output files could be:
- `analysis/data-derived/included_studies_with_pdf.csv`
- `analysis/data-derived/included_studies_with_pdf.rds`
- `analysis/data-derived/pdf_download_results.csv`

### Other functions you will use in this file

Before you write `analysis/03_fetch_pdfs.R`, you should create a helper file called `R/pdf_helpers.R`.

This file should also live in the top-level `R/` folder.

The step 03 script will load it with:

```r
source("R/pdf_helpers.R")
```

You can put the exact file contents below into that helper file.

```r
get_json_with_retry <- function(url, tries = 3, wait_seconds = 2) {
  for (attempt in seq_len(tries)) {
    req <- httr2::request(url) |>
      httr2::req_user_agent("mortality-crisis-endnote-compendium/0.1") |>
      httr2::req_timeout(30)

    resp <- tryCatch(httr2::req_perform(req), error = function(e) NULL)

    if (!is.null(resp)) {
      status <- httr2::resp_status(resp)

      if (status >= 200 && status < 300) {
        text <- httr2::resp_body_string(resp)
        parsed <- tryCatch(jsonlite::fromJSON(text, simplifyVector = TRUE), error = function(e) NULL)
        return(parsed)
      }
    }

    if (attempt < tries) {
      Sys.sleep(wait_seconds)
    }
  }

  NULL
}

query_openalex_pdf_urls <- function(doi) {
  doi <- utils::URLencode(doi, reserved = TRUE)
  url <- paste0("https://api.openalex.org/works/https://doi.org/", doi)
  result <- get_json_with_retry(url)

  if (is.null(result)) {
    return(character(0))
  }

  urls <- c(
    result$primary_location$pdf_url,
    result$best_oa_location$pdf_url
  )

  if (!is.null(result$locations) && "pdf_url" %in% names(result$locations)) {
    urls <- c(urls, result$locations$pdf_url)
  }

  urls <- unique(urls)
  urls <- urls[!is.na(urls) & nzchar(urls)]
  urls
}

query_europepmc_pdf_urls <- function(doi) {
  query <- utils::URLencode(paste0('DOI:"', doi, '"'), reserved = TRUE)
  url <- paste0(
    "https://www.ebi.ac.uk/europepmc/webservices/rest/search?resultType=core&format=json&pageSize=25&query=",
    query
  )
  result <- get_json_with_retry(url)

  if (is.null(result) || is.null(result$resultList$result)) {
    return(character(0))
  }

  result_rows <- result$resultList$result
  if (is.data.frame(result_rows)) {
    result_rows <- split(result_rows, seq_len(nrow(result_rows)))
  }

  urls <- character(0)

  for (i in seq_along(result_rows)) {
    full_text <- result_rows[[i]]$fullTextUrlList$fullTextUrl

    if (is.null(full_text)) {
      next
    }

    if (is.list(full_text) && length(full_text) == 1 && is.data.frame(full_text[[1]])) {
      full_text <- full_text[[1]]
    }

    candidate_urls <- full_text$url
    candidate_styles <- full_text$documentStyle

    for (j in seq_along(candidate_urls)) {
      candidate_url <- candidate_urls[[j]]
      if (length(candidate_styles) >= j) {
        candidate_style <- candidate_styles[[j]]
      } else {
        candidate_style <- ""
      }
      looks_like_pdf <- grepl("pdf", tolower(paste(candidate_url, candidate_style)))

      if (!is.na(candidate_url) && nzchar(candidate_url) && looks_like_pdf) {
        urls <- c(urls, candidate_url)
      }
    }
  }

  urls <- unique(urls)
  urls <- urls[!is.na(urls) & nzchar(urls)]
  urls
}

download_pdf_with_retry <- function(url, dest_path, tries = 3, wait_seconds = 2) {
  if (is.na(url) || !nzchar(url)) {
    return(FALSE)
  }

  for (attempt in seq_len(tries)) {
    req <- httr2::request(url) |>
      httr2::req_user_agent("mortality-crisis-endnote-compendium/0.1") |>
      httr2::req_headers(Accept = "application/pdf") |>
      httr2::req_timeout(45)

    resp <- tryCatch(httr2::req_perform(req), error = function(e) NULL)

    if (!is.null(resp)) {
      content_type <- httr2::resp_header(resp, "content-type")

      if (!is.null(content_type) && grepl("application/pdf", tolower(content_type))) {
        writeBin(httr2::resp_body_raw(resp), dest_path)
        return(TRUE)
      }
    }

    if (attempt < tries) {
      Sys.sleep(wait_seconds)
    }
  }

  FALSE
}
```

### How script 3 uses this helper file

The roles of the helper functions are:

- `query_openalex_pdf_urls(doi)`
  - returns a character vector of candidate PDF URLs from OpenAlex

- `query_europepmc_pdf_urls(doi)`
  - returns a character vector of candidate PDF URLs from Europe PMC

- `download_pdf_with_retry(url, dest_path)`
  - tries to save one real PDF file to a known local path
  - returns `TRUE` if the download succeeded and looked like a PDF
  - returns `FALSE` if the request failed or the response was not a PDF

- `get_json_with_retry(url)`
  - is an internal helper used by the two API query functions
  - the main step 03 script does not need to call it directly

You should understand that step 03 uses these functions in a loop, but the web request details stay hidden inside `R/pdf_helpers.R`.

### Objects to create in this file

#### `doi_df`
The DOI-enriched table from step 02.

#### `pdf_results_list`
A list used to collect one result row per study while looping.

#### `pdf_results_df`
A data frame created by binding the list of results together.

#### `final_pdf_df`
The DOI-enriched table after PDF result columns have been merged back onto it.

### Why a results table is useful

It is usually easier to search for PDFs in one object and store the results in a separate table first.

That result table can then be joined back onto the main DOI table.

This makes the code easier to read because it separates:
- the lookup process
- the final merge back into the main study table

### Columns that the PDF results should record

A sensible results table could include:
- `record_index`
- `doi`
- `pdf_path`
- `pdf_found`
- `pdf_source`
- `pdf_url`

#### Why each of these matters

- `record_index`
  - identifies which study the result belongs to

- `doi`
  - shows which DOI was used for lookup

- `pdf_path`
  - records where the file was saved locally

- `pdf_found`
  - gives a simple yes or no result

- `pdf_source`
  - records which source worked, for example OpenAlex or Europe PMC

- `pdf_url`
  - records the actual URL that succeeded

### Detailed steps for this file

#### 1. Load packages

The script itself may only need a small set, for example:
- `dplyr`
- `readr`

The helper functions can carry the more technical web request logic.

#### 2. Source helper functions if needed

You should be able to see clearly which helper functions are being used for API querying and downloading.

#### 3. Define paths

Define:
- the input DOI-enriched file
- the output final table path
- the output results table path
- the PDF download folder path

#### 4. Read the DOI-enriched table

Read the step 02 output into `doi_df`.

#### 5. Keep only rows with DOI values for lookup

Create a smaller object if needed that contains only rows where DOI is present.

The purpose is simple: if there is no DOI, there is nothing to query against OpenAlex or Europe PMC.

#### 6. Decide which record identifier will be used for filenames

Use one known identifier column, for example `record_index`.

The purpose is to create a stable file name such as:

```r
analysis/pdf_download/12345.pdf
```

This lets you connect saved files back to the study table later.

#### 7. Start a loop over rows

Loop over the DOI rows one at a time.

For each row, extract:
- the record identifier
- the DOI
- the destination file path

The loop should be easy to follow and not overly abstract.

For the final version of the script, the loop should process all DOI rows.

The simplest final form is:

```r
for (i in seq_len(nrow(lookup_df))) {
```

If you want to test the script on a small subset first, you can temporarily use something like:

```r
for (i in seq_len(min(25, nrow(lookup_df)))) {
```

That is only for testing. The full script should eventually go back to looping over all DOI rows.

#### 8. Query OpenAlex first

Call the OpenAlex helper and store the returned URLs.

The purpose is to get a list of candidate PDF links from one source.

#### 9. Query Europe PMC second

Call the Europe PMC helper and store the returned URLs.

The purpose is to get candidate PDF links from a second source if OpenAlex does not provide a usable result.

#### 10. Combine candidate URLs

Combine the candidate URLs into one vector or one small temporary data frame.

At this stage, the purpose is:
- collect all possible links for this DOI
- keep track of which source each link came from
- remove duplicates if needed

#### 11. Try candidate URLs in order

Loop through the candidates and call the download helper.

The aim is:
- try the first URL
- if it works, record success and stop trying more URLs for that study
- if it fails, move to the next URL

This is a useful pattern because it shows how a loop can work through options until one succeeds.

#### 12. Create one result row for that study

After the loop for one study finishes, create one small data frame recording:
- which study was processed
- whether a PDF was found
- which source worked
- which URL worked
- where the file was saved

This row is then added to `pdf_results_list`.

#### 13. Bind all result rows together

After the outer loop finishes, turn the list of rows into a single data frame.

This gives you `pdf_results_df`.

#### 14. Merge PDF results back onto the DOI table

Join `pdf_results_df` back onto `doi_df`.

The purpose is to create a single final table that shows both identifier and PDF retrieval status for each study.

This final table is usually the easiest thing to inspect and share.

#### 15. Print summary counts

Print:
- how many DOI rows were checked
- how many PDFs were found
- how many PDFs were not found

This gives a quick sense of how successful the retrieval stage was.

#### 16. Save outputs

Save:
- the result table alone
- the final merged DOI-plus-PDF table

The final merged table is usually the most useful output for later work.

## 10. What you should understand about joins
 
Because this workflow depends on joins, it is worth stating the purpose of each join explicitly.

### Join in step 02

A join is used to add DOI and EndNote fields to the included studies table.

You should understand:
- the included table is the main table
- the DOI lookup table is supplying extra columns
- the join key is the record identifier shared between both tables

### Join in step 03

A join is used to add PDF result fields back onto the DOI-enriched study table.

You should understand:
- the DOI-enriched table is still the main table
- the PDF results table stores one lookup result per study
- the join key should be a stable record identifier, not the title text

## 11. What you should understand about new columns

New columns should be created only when they help the workflow.

Examples of useful new columns are:
- `doi`
- `doi_source_field`
- `pdf_path`
- `pdf_found`
- `pdf_source`
- `pdf_url`

Each new column should answer a clear question.

For example:
- `doi`: what DOI do we think belongs to this study?
- `doi_source_field`: where in the EndNote record was the DOI found?
- `pdf_found`: was a PDF successfully retrieved?
- `pdf_source`: which service gave the successful PDF URL?

You should avoid adding columns that do not have a clear use later in the workflow.

## 12. What to look up while building the scripts

These are the main topics you will probably need to look up.

### Basic file input and output
- `readr::read_csv()`
- `readr::write_csv()`
- `saveRDS()`
- `readRDS()`

### Data inspection
- `names()`
- `head()`
- `table()`
- `nrow()`
- `View()`

### Data manipulation with `dplyr`
- `filter()`
- `mutate()`
- `select()`
- `left_join()`
- `distinct()`

### Working with missing values
- `is.na()`
- checking for empty strings

### Loops and control flow
- `for`
- `if`
- `break`

### Strings and file paths
- `paste0()`
- `file.path()`

## 13. Recommended order of work for you

You should not try to write everything at once.

A better order is:

### Stage 1
Write and test `01_fetch_all_studies.R` until it reliably creates the included studies output.

### Stage 2
Use the provided helper functions to write `02_fetch_dois.R` and confirm that DOI values are being attached correctly.

### Stage 3
Write `03_fetch_pdfs.R` so it reads the DOI table, asks OpenAlex and Europe PMC for candidate URLs, tries the URLs in order, and records the result in `pdf_results_list`.

### Stage 4
Test step 03 on a small number of DOI rows first so you can check that the loop, result table, and downloaded files behave as expected.

### Stage 5
Remove any temporary loop cap, run the full PDF stage, and save both the PDF results table and the final merged DOI-plus-PDF table.

This order matters because each file depends on the previous one.

## 14. Checks that can be done after each file

### After step 01
- Did the script read the file correctly?
- Does the decision column look like the expected one?
- Does the included row count make sense?
- Were the output files written?

### After step 02
- Did the EndNote helper return a `refs` table?
- Does the join key line up between the included table and EndNote table?
- How many rows have a DOI after the join?
- Were the DOI-enriched output files written?

### After step 03
- How many rows had a DOI and were attempted?
- How many PDF links were found?
- How many files were downloaded successfully?
- Does the final table contain the new PDF result columns?

