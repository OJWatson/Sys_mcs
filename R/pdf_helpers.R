library(httr2)
library(jsonlite)

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

# Conference abstracts can share a generic DOI or have incomplete DOI metadata.
# Search by exact normalized title as a fallback, accepting locations only from
# the exact bibliographic match rather than a fuzzy search hit.
query_openalex_title_pdf_urls <- function(title) {
  if (is.na(title) || !nzchar(trimws(title))) return(character(0))
  key <- function(x) tolower(gsub("[^a-z0-9]", "", as.character(x)))
  url <- paste0(
    "https://api.openalex.org/works?per-page=25&search=",
    utils::URLencode(title, reserved = TRUE)
  )
  result <- get_json_with_retry(url)
  works <- result$results
  if (is.null(works) || !is.data.frame(works) || !"display_name" %in% names(works)) return(character(0))
  works <- works[key(works$display_name) == key(title), , drop = FALSE]
  if (nrow(works) == 0) return(character(0))
  location_pdf_url <- function(location) {
    if (is.null(location) || length(location) == 0 || (is.atomic(location) && all(is.na(location)))) return(character(0))
    if (is.data.frame(location) && "pdf_url" %in% names(location)) return(as.character(location$pdf_url))
    if (is.list(location) && "pdf_url" %in% names(location)) return(as.character(location$pdf_url))
    character(0)
  }
  primary <- if ("primary_location" %in% names(works)) works$primary_location[[1]] else NULL
  best <- if ("best_oa_location" %in% names(works)) works$best_oa_location[[1]] else NULL
  urls <- c(location_pdf_url(primary), location_pdf_url(best))
  locations <- works$locations[[1]]
  if (is.data.frame(locations) && "pdf_url" %in% names(locations)) urls <- c(urls, locations$pdf_url)
  unique(urls[!is.na(urls) & nzchar(urls)])
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
