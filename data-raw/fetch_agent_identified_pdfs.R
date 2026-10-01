#!/usr/bin/env Rscript

# Reproducibility wrapper for no-DOI PDFs identified by agents.
#
# The manifest consumed by this script is not a deterministic search result.
# These records were searched agentically, verified against the article
# metadata, and the PDFs that were found were staged in a Google Drive folder.
# This script documents that handoff: it validates the manifest, optionally
# downloads the staged PDFs into the repository's normal local PDF directory,
# optionally syncs them into a review Drive folder, and writes a derived status
# report. Rows that were not found stay explained by missing_reason.
#
# Defaults are local and conservative. The script validates the manifest and
# writes a report without contacting Google Drive unless dry-run is disabled.
# External Drive uploads require a separate explicit gate.
#
# Main env vars:
#   SYS_MCS_AGENT_PDF_SOURCE_FOLDER_ID / SYS_MCS_AGENT_PDF_SOURCE_FOLDER_NAME
#   SYS_MCS_AGENT_PDF_DEST_FOLDER_ID / SYS_MCS_AGENT_PDF_DEST_FOLDER_NAME
#   SYS_MCS_AGENT_PDF_ALLOW_DEST_UPLOAD=true

truthy <- function(value) {
  tolower(trimws(value)) %in% c("1", "true", "yes", "y")
}

env_truthy <- function(name, default = "false") {
  truthy(Sys.getenv(name, unset = default))
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

arg_value <- function(prefix, args) {
  hit <- grep(paste0("^", prefix, "="), args, value = TRUE)
  if (length(hit) == 0) return(NULL)
  sub(paste0("^", prefix, "="), "", hit[[1]])
}

script_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1] %||% "")
script_dir <- if (nzchar(script_file)) dirname(normalizePath(script_file, mustWork = FALSE)) else getwd()
repo_root <- normalizePath(file.path(script_dir, ".."), mustWork = FALSE)
args <- commandArgs(trailingOnly = TRUE)

manifest_csv <- arg_value("--manifest", args) %||%
  Sys.getenv(
    "SYS_MCS_AGENT_PDF_MANIFEST",
    unset = file.path(repo_root, "data-raw", "agent_identified_pdf_manifest.csv")
  )
local_pdf_dir <- arg_value("--pdf-dir", args) %||%
  Sys.getenv("SYS_MCS_AGENT_PDF_LOCAL_DIR", unset = file.path(repo_root, "analysis", "pdf_download"))
report_csv <- arg_value("--report", args) %||%
  Sys.getenv(
    "SYS_MCS_AGENT_PDF_REPORT",
    unset = file.path(repo_root, "analysis", "data-derived", "agent_identified_pdf_status.csv")
  )

dry_run <- env_truthy("SYS_MCS_AGENT_PDF_DRY_RUN", default = "true") || "--dry-run" %in% args
if ("--run" %in% args) dry_run <- FALSE

allow_dest_upload <- env_truthy("SYS_MCS_AGENT_PDF_ALLOW_DEST_UPLOAD")
if (allow_dest_upload && dry_run) {
  stop("Refusing destination Drive upload while dry-run is enabled. Use --run as well.")
}

read_csv <- function(path) {
  if (!file.exists(path)) stop("Missing manifest CSV: ", path)
  if (requireNamespace("readr", quietly = TRUE)) {
    return(as.data.frame(readr::read_csv(path, show_col_types = FALSE), stringsAsFactors = FALSE))
  }
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

write_csv <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (requireNamespace("readr", quietly = TRUE)) {
    readr::write_csv(x, path, na = "")
  } else {
    write.csv(x, path, row.names = FALSE, na = "")
  }
}

is_pdf <- function(path) {
  if (is.na(path) || !nzchar(path) || !file.exists(path)) return(FALSE)
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  identical(readBin(con, what = "raw", n = 4), charToRaw("%PDF"))
}

resolve_drive_folder <- function(folder_id_env, folder_name_env, purpose) {
  folder_id <- Sys.getenv(folder_id_env, unset = "")
  folder_name <- Sys.getenv(folder_name_env, unset = "")

  if (nzchar(folder_id)) {
    return(googledrive::as_id(folder_id))
  }
  if (!nzchar(folder_name)) {
    stop(purpose, " requires ", folder_id_env, " or ", folder_name_env, ".")
  }

  candidates <- googledrive::drive_find(pattern = folder_name, type = "folder")
  candidates <- candidates[candidates$name == folder_name, , drop = FALSE]
  if (nrow(candidates) != 1) {
    stop(
      purpose, " folder name must match exactly one Drive folder; found ",
      nrow(candidates), " matches for: ", folder_name
    )
  }
  candidates
}

locate_drive_file <- function(folder, filename, purpose) {
  files <- googledrive::drive_ls(folder)
  matches <- files[files$name == filename, , drop = FALSE]
  if (nrow(matches) == 0) {
    stop("No staged PDF named ", filename, " found in ", purpose, ".")
  }
  if (nrow(matches) > 1) {
    stop("Ambiguous staged PDF filename in ", purpose, ": ", filename)
  }
  matches
}

manifest <- read_csv(manifest_csv)
required_cols <- c("record_index", "pdf_filename", "status", "missing_reason")
missing_cols <- setdiff(required_cols, names(manifest))
if (length(missing_cols)) {
  stop("Manifest is missing required columns: ", paste(missing_cols, collapse = ", "))
}

manifest$record_index <- as.character(manifest$record_index)
manifest$pdf_filename <- as.character(manifest$pdf_filename)
manifest$status <- as.character(manifest$status)
manifest$missing_reason <- as.character(manifest$missing_reason)

if (anyDuplicated(manifest$record_index)) {
  dupes <- unique(manifest$record_index[duplicated(manifest$record_index)])
  stop("Manifest has duplicate record_index values: ", paste(head(dupes, 10), collapse = ", "))
}

found_statuses <- c("found", "found_pdf", "pdf_found", "staged")
manifest$is_found_manifest <- tolower(trimws(manifest$status)) %in% found_statuses
found_rows <- manifest[manifest$is_found_manifest, , drop = FALSE]
missing_rows <- manifest[!manifest$is_found_manifest, , drop = FALSE]

if (nrow(found_rows) > 0) {
  if (any(!nzchar(found_rows$pdf_filename))) {
    stop("Found rows must have pdf_filename values.")
  }
  if (any(!grepl("\\.pdf$", found_rows$pdf_filename, ignore.case = TRUE))) {
    stop("Found rows must name PDF files with a .pdf extension.")
  }
  if (anyDuplicated(found_rows$pdf_filename)) {
    dupes <- unique(found_rows$pdf_filename[duplicated(found_rows$pdf_filename)])
    stop("Found rows have duplicate pdf_filename values: ", paste(head(dupes, 10), collapse = ", "))
  }
}

if (nrow(missing_rows) > 0 && any(!nzchar(trimws(missing_rows$missing_reason)))) {
  stop("Rows that are not marked found must carry a missing_reason.")
}

dir.create(local_pdf_dir, recursive = TRUE, showWarnings = FALSE)

report <- data.frame(
  record_index = manifest$record_index,
  pdf_filename = manifest$pdf_filename,
  status = manifest$status,
  pdf_found = manifest$is_found_manifest,
  local_pdf_path = ifelse(
    manifest$is_found_manifest,
    file.path(local_pdf_dir, manifest$pdf_filename),
    ""
  ),
  local_pdf_present = FALSE,
  source_drive_file_id = "",
  destination_drive_file_id = "",
  drive_sync_status = ifelse(manifest$is_found_manifest, "planned_dry_run", "not_found_in_manifest"),
  missing_reason = manifest$missing_reason,
  stringsAsFactors = FALSE
)

if (!dry_run && nrow(found_rows) > 0) {
  if (!requireNamespace("googledrive", quietly = TRUE)) {
    stop("Package googledrive is required when dry-run is disabled.")
  }
  googledrive::drive_auth()

  source_folder <- resolve_drive_folder(
    "SYS_MCS_AGENT_PDF_SOURCE_FOLDER_ID",
    "SYS_MCS_AGENT_PDF_SOURCE_FOLDER_NAME",
    "Source staging"
  )
  destination_folder <- NULL
  if (allow_dest_upload) {
    destination_folder <- resolve_drive_folder(
      "SYS_MCS_AGENT_PDF_DEST_FOLDER_ID",
      "SYS_MCS_AGENT_PDF_DEST_FOLDER_NAME",
      "Destination review"
    )
  }

  for (i in seq_len(nrow(found_rows))) {
    idx <- found_rows$record_index[[i]]
    filename <- found_rows$pdf_filename[[i]]
    report_row <- which(report$record_index == idx)
    local_path <- file.path(local_pdf_dir, filename)

    staged_file <- locate_drive_file(source_folder, filename, "source staging folder")
    googledrive::drive_download(staged_file, path = local_path, overwrite = TRUE)
    if (!is_pdf(local_path)) {
      stop("Downloaded file is not a valid PDF for record_index ", idx, ": ", local_path)
    }

    report$source_drive_file_id[report_row] <- staged_file$id[[1]]
    report$local_pdf_present[report_row] <- TRUE
    report$drive_sync_status[report_row] <- "downloaded_from_staging"

    if (allow_dest_upload) {
      destination_files <- googledrive::drive_ls(destination_folder)
      destination_matches <- destination_files[destination_files$name == filename, , drop = FALSE]
      if (nrow(destination_matches) > 1) {
        stop("Ambiguous destination Drive filename: ", filename)
      }
      if (nrow(destination_matches) == 1) {
        dest_file <- destination_matches
        report$drive_sync_status[report_row] <- "already_in_destination"
      } else {
        dest_file <- googledrive::drive_upload(
          media = local_path,
          path = destination_folder,
          name = filename,
          overwrite = FALSE
        )
        report$drive_sync_status[report_row] <- "uploaded_to_destination"
      }
      report$destination_drive_file_id[report_row] <- dest_file$id[[1]]
    }
  }
} else {
  local_paths <- report$local_pdf_path[report$pdf_found]
  report$local_pdf_present[report$pdf_found] <- vapply(local_paths, is_pdf, logical(1))
  report$drive_sync_status[report$pdf_found & report$local_pdf_present] <- "already_local_pdf"
}

write_csv(report, report_csv)

cat("Manifest rows:", nrow(manifest), "\n")
cat("Found rows:", nrow(found_rows), "\n")
cat("Missing rows:", nrow(missing_rows), "\n")
cat("Dry-run:", dry_run, "\n")
cat("Destination upload enabled:", allow_dest_upload, "\n")
cat("Report:", report_csv, "\n")
