#!/usr/bin/env Rscript

# Upload recovered no-DOI PDFs to the existing Drive PDF folder and update the
# screening Sheet using the same record_index/pdf_link pattern as 08_pdfs_to_drive.R.
# Dry-run is the default; live Drive/Sheet writes require explicit env gates.

truthy <- function(name) {
  tolower(trimws(Sys.getenv(name, unset = "false"))) %in% c("1", "true", "yes", "y")
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

csv_read <- function(path) {
  if (!file.exists(path)) {
    stop("Missing required file: ", path)
  }
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

csv_write <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write.csv(x, path, row.names = FALSE, na = "")
}

is_pdf <- function(path) {
  if (is.na(path) || !nzchar(path) || !file.exists(path)) {
    return(FALSE)
  }
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  identical(readBin(con, what = "raw", n = 4), charToRaw("%PDF"))
}

make_drive_link <- function(file_id) {
  if (is.na(file_id) || !nzchar(file_id)) {
    return(NA_character_)
  }
  paste0("https://drive.google.com/file/d/", file_id, "/view")
}

ensure_columns <- function(df, cols, allow_add) {
  missing_cols <- setdiff(cols, names(df))
  if (length(missing_cols) && !allow_add) {
    stop("Missing required Sheet columns: ", paste(missing_cols, collapse = ", "))
  }
  for (col in missing_cols) {
    df[[col]] <- ""
  }
  df
}

update_records_with_drive_links <- function(records, drive_manifest, allow_add_columns, status_value) {
  records$record_index <- as.character(records$record_index)
  pdf_link_col <- Sys.getenv("SYS_MCS_NODOI_SHEET_PDF_LINK_COL", unset = "pdf_link")
  pdf_found_col <- Sys.getenv("SYS_MCS_NODOI_SHEET_PDF_FOUND_COL", unset = "pdf_found")
  status_col <- Sys.getenv("SYS_MCS_NODOI_SHEET_STATUS_COL", unset = "screening_status")
  filename_col <- Sys.getenv("SYS_MCS_NODOI_SHEET_PDF_FILENAME_COL", unset = "")

  required_cols <- c(pdf_link_col, pdf_found_col, status_col)
  if (nzchar(filename_col) && filename_col %in% names(records)) {
    required_cols <- c(required_cols, filename_col)
  }
  records <- ensure_columns(records, required_cols, allow_add = allow_add_columns)

  for (i in seq_len(nrow(drive_manifest))) {
    idx <- as.character(drive_manifest$record_index[i])
    row_match <- which(records$record_index == idx)
    if (!length(row_match)) {
      next
    }

    link <- drive_manifest$drive_pdf_link[i]
    if (is.na(link) || !nzchar(link)) {
      link <- paste0("DRY_RUN_PENDING_DRIVE_LINK:", drive_manifest$pdf_filename[i])
    }

    records[row_match, pdf_link_col] <- link
    records[row_match, pdf_found_col] <- TRUE
    records[row_match, status_col] <- status_value
    if (nzchar(filename_col) && filename_col %in% names(records)) {
      records[row_match, filename_col] <- drive_manifest$pdf_filename[i]
    }
  }

  records
}

write_summary_json <- function(summary, path) {
  encode_value <- function(x) {
    if (is.logical(x)) {
      return(tolower(as.character(x)))
    }
    if (is.numeric(x)) {
      return(as.character(x))
    }
    paste0('"', gsub('"', '\\"', as.character(x), fixed = TRUE), '"')
  }

  lines <- sprintf(
    '  "%s": %s',
    names(summary),
    vapply(summary, encode_value, character(1))
  )
  json <- paste0("{\n", paste(lines, collapse = ",\n"), "\n}\n")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(json, path)
  cat(json)
}

script_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1] %||% "")
script_dir <- if (nzchar(script_file)) dirname(normalizePath(script_file, mustWork = FALSE)) else getwd()
repo_root <- normalizePath(file.path(script_dir, ".."), mustWork = FALSE)

manifest_csv <- Sys.getenv(
  "SYS_MCS_NODOI_MANIFEST",
  unset = file.path(repo_root, "analysis", "data-derived", "no_doi_pdf_manifest.csv")
)
drive_manifest_csv <- Sys.getenv(
  "SYS_MCS_NODOI_DRIVE_MANIFEST",
  unset = file.path(repo_root, "analysis", "data-derived", "no_doi_drive_manifest.csv")
)
sheet_preview_csv <- Sys.getenv(
  "SYS_MCS_NODOI_SHEET_PREVIEW",
  unset = file.path(repo_root, "analysis", "data-derived", "no_doi_sheet_update_preview.csv")
)
summary_json <- Sys.getenv(
  "SYS_MCS_NODOI_SUMMARY",
  unset = file.path(repo_root, "analysis", "data-derived", "no_doi_upload_summary.json")
)
sheet_preview_source <- Sys.getenv(
  "SYS_MCS_NODOI_SHEET_PREVIEW_SOURCE",
  unset = file.path(repo_root, "analysis", "data-derived", "missing_doi_categorised.csv")
)

allow_drive_upload <- truthy("SYS_MCS_NODOI_ALLOW_DRIVE_UPLOAD")
allow_sheet_update <- truthy("SYS_MCS_NODOI_ALLOW_SHEET_UPDATE")
allow_add_sheet_columns <- truthy("SYS_MCS_NODOI_ALLOW_ADD_SHEET_COLUMNS")
allow_share_anyone <- truthy("SYS_MCS_NODOI_ALLOW_SHARE_ANYONE")

if (allow_sheet_update && !allow_drive_upload) {
  stop("Refusing live Sheet update unless Drive upload is enabled too.")
}

manifest <- csv_read(manifest_csv)
required_manifest_cols <- c("record_index", "pdf_filename", "staged_pdf_path")
missing_manifest_cols <- setdiff(required_manifest_cols, names(manifest))
if (length(missing_manifest_cols)) {
  stop("Manifest is missing required columns: ", paste(missing_manifest_cols, collapse = ", "))
}

manifest$record_index <- as.character(manifest$record_index)
manifest$pdf_filename <- as.character(manifest$pdf_filename)
manifest$staged_pdf_path <- as.character(manifest$staged_pdf_path)

if (anyDuplicated(manifest$record_index)) {
  stop("Manifest has duplicate record_index values.")
}
if (anyDuplicated(manifest$pdf_filename)) {
  stop("Manifest has duplicate pdf_filename values.")
}

manifest$pdf_valid <- vapply(manifest$staged_pdf_path, is_pdf, logical(1))
found <- manifest[manifest$pdf_valid, , drop = FALSE]
if (nrow(found) == 0) {
  stop("No valid staged PDFs found in manifest.")
}

if (!allow_drive_upload) {
  drive_manifest <- data.frame(
    record_index = found$record_index,
    pdf_filename = found$pdf_filename,
    local_pdf_path = found$staged_pdf_path,
    drive_file_id = NA_character_,
    drive_pdf_link = NA_character_,
    upload_status = "planned_dry_run",
    stringsAsFactors = FALSE
  )
} else {
  if (!requireNamespace("googledrive", quietly = TRUE)) {
    stop("Package googledrive is required for live Drive upload.")
  }

  googledrive::drive_auth()

  folder_id <- Sys.getenv("SYS_MCS_NODOI_DRIVE_FOLDER_ID", unset = "")
  parent_folder_name <- Sys.getenv("SYS_MCS_PARENT_DRIVE_FOLDER_NAME", unset = "Causes_of_Mortality_Review")
  pdf_drive_folder_name <- Sys.getenv("SYS_MCS_PDF_DRIVE_FOLDER_NAME", unset = "Retrieved_PDFs")

  if (nzchar(folder_id)) {
    pdf_drive_folder <- googledrive::as_id(folder_id)
  } else {
    parent_folder <- googledrive::drive_find(pattern = parent_folder_name, type = "folder")
    if (nrow(parent_folder) == 0) {
      parent_folder <- googledrive::drive_mkdir(parent_folder_name)
    }
    pdf_drive_folder <- googledrive::drive_ls(parent_folder) |>
      subset(name == pdf_drive_folder_name)
    if (nrow(pdf_drive_folder) == 0) {
      pdf_drive_folder <- googledrive::drive_mkdir(
        name = pdf_drive_folder_name,
        path = parent_folder
      )
    }
  }

  existing_files <- googledrive::drive_ls(pdf_drive_folder)
  uploads <- vector("list", nrow(found))

  for (i in seq_len(nrow(found))) {
    pdf_filename <- found$pdf_filename[i]
    existing_file <- existing_files[existing_files$name == pdf_filename, , drop = FALSE]

    if (nrow(existing_file) > 0) {
      uploaded_file <- existing_file[1, , drop = FALSE]
      upload_status <- "already_on_drive"
    } else {
      uploaded_file <- googledrive::drive_upload(
        media = found$staged_pdf_path[i],
        path = pdf_drive_folder,
        name = pdf_filename,
        overwrite = FALSE
      )
      upload_status <- "uploaded"
    }

    if (allow_share_anyone) {
      googledrive::drive_share(uploaded_file, role = "reader", type = "anyone")
    }

    uploads[[i]] <- data.frame(
      record_index = found$record_index[i],
      pdf_filename = pdf_filename,
      local_pdf_path = found$staged_pdf_path[i],
      drive_file_id = uploaded_file$id[[1]],
      drive_pdf_link = uploaded_file$drive_resource[[1]]$webViewLink %||% make_drive_link(uploaded_file$id[[1]]),
      upload_status = upload_status,
      stringsAsFactors = FALSE
    )
  }

  drive_manifest <- do.call(rbind, uploads)
}

csv_write(drive_manifest, drive_manifest_csv)

preview_records <- if (file.exists(sheet_preview_source)) {
  csv_read(sheet_preview_source)
} else {
  manifest[, c("record_index", "title"), drop = FALSE]
}
if (!"record_index" %in% names(preview_records)) {
  stop("Sheet preview source must contain a record_index column.")
}

sheet_status <- if (allow_drive_upload) {
  "PDF uploaded from no-DOI recovery"
} else {
  "PDF found locally; Drive upload dry-run pending"
}
preview_records <- update_records_with_drive_links(
  preview_records,
  drive_manifest,
  allow_add_columns = TRUE,
  status_value = sheet_status
)
csv_write(preview_records, sheet_preview_csv)

sheet_action <- "dry_run"
sheet_id <- Sys.getenv("SYS_MCS_NODOI_GOOGLE_SHEET_ID", unset = "")
sheet_name <- Sys.getenv("SYS_MCS_NODOI_GOOGLE_SHEET_NAME", unset = "studies")

if (allow_sheet_update) {
  if (!nzchar(sheet_id)) {
    stop("SYS_MCS_NODOI_GOOGLE_SHEET_ID is required for live Sheet update.")
  }
  if (!requireNamespace("googlesheets4", quietly = TRUE)) {
    stop("Package googlesheets4 is required for live Sheet update.")
  }

  googlesheets4::gs4_auth()
  live_sheet <- googlesheets4::read_sheet(sheet_id, sheet = sheet_name)
  live_sheet <- as.data.frame(live_sheet, stringsAsFactors = FALSE, check.names = FALSE)
  if (!"record_index" %in% names(live_sheet)) {
    stop("Live Sheet must contain a record_index column.")
  }

  live_sheet <- update_records_with_drive_links(
    live_sheet,
    drive_manifest,
    allow_add_columns = allow_add_sheet_columns,
    status_value = "PDF uploaded from no-DOI recovery"
  )
  googlesheets4::range_write(ss = sheet_id, data = live_sheet, sheet = sheet_name, range = "A1")
  sheet_action <- "updated_live_sheet"
}

summary <- list(
  generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  manifest = manifest_csv,
  valid_local_pdfs = nrow(found),
  allow_drive_upload = allow_drive_upload,
  allow_sheet_update = allow_sheet_update,
  allow_share_anyone = allow_share_anyone,
  drive_manifest_rows = nrow(drive_manifest),
  drive_manifest = drive_manifest_csv,
  sheet_preview_rows = nrow(preview_records),
  sheet_preview = sheet_preview_csv,
  sheet_action = sheet_action
)

write_summary_json(summary, summary_json)
