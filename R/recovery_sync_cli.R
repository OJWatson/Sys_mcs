# Shared command line for reviewed Drive-to-Sheets recovery.
recovery_cli_options <- function(args, workflow, repo_root) {
  if(all(c("--apply","--dry-run") %in% args)) stop("Choose only one of --apply and --dry-run.")
  opt <- list(apply=FALSE, help=FALSE, snapshot_dir="",
    manifest=file.path(repo_root,"data-raw",if(workflow=="country") "hrp_country_correction_records.csv" else "recovered_pdf_inventory.csv"),
    sheet_id="1foHMPBCWYzWX8975UKjdacE2i_j_48n8uTMdAQ-ETcg", sheet_tab="studies",
    source_folder_id="1m03L684qekbvcQQlKE40_6KIJjdoQDO4",
    destination_folder_id="1C7ufZYeYVyvzULW7zisp_PKlEYUyqYIU",
    receipt_dir=file.path(repo_root,"analysis","recovery_runs"))
  keys <- c("manifest"="manifest","sheet-id"="sheet_id","sheet-tab"="sheet_tab",
    "source-folder"="source_folder_id","destination-folder"="destination_folder_id",
    "output-dir"="receipt_dir","snapshot-dir"="snapshot_dir")
  i <- 1L
  while(i<=length(args)) {
    a <- args[[i]]
    if(a=="--apply") opt$apply <- TRUE
    else if(a=="--dry-run") opt$apply <- FALSE
    else if(a %in% c("--help","-h")) opt$help <- TRUE
    else {
      key <- sub("^--","",a)
      if(!startsWith(a,"--") || !key %in% names(keys)) stop("Unknown option: ",a)
      i <- i+1L
      if(i>length(args) || startsWith(args[[i]],"--")) stop("Missing value for ",a)
      opt[[keys[[key]]]] <- args[[i]]
    }
    i <- i+1L
  }
  if(opt$apply && nzchar(opt$snapshot_dir)) stop("--snapshot-dir is read-only; cannot combine with --apply.")
  opt
}
recovery_cli_help <- function(workflow) {
  script <- if(workflow=="country") "analysis/10_hrp_correction.R" else "analysis/10_upload_no_doi_pdfs_to_drive.R"
  cat("Usage: Rscript ",script," [--dry-run | --apply] [options]\n\n",sep="")
  cat("Default: read current Drive/Sheet state and save a plan without remote writes.\n",
      "--apply                    Copy verified PDFs and apply exact row/cell changes\n",
      "--manifest PATH            Reviewed CSV manifest\n",
      "--sheet-id ID              Target spreadsheet (default: production)\n",
      "--sheet-tab NAME           Target tab (default: studies)\n",
      "--source-folder ID         Default source folder; reviewed row overrides allowed\n",
      "--destination-folder ID    Destination PDFs (default: production)\n",
      "--output-dir PATH          Parent directory for dated plans/receipts\n",
      "--snapshot-dir PATH        Offline dry run using sheet/source/destination JSON\n",
      "--help                     Help without Google authentication\n\n",sep="")
  cat("No sharing changes. Absent no-DOI IDs are reported, not appended.\n",
      "Country additions enter full-text screening; they are not final inclusions.\n",sep="")
}
recovery_cli_manifest <- function(path,workflow) {
  if(!file.exists(path)) stop("Reviewed manifest not found: ",path)
  x <- read.csv(path,colClasses="character",check.names=FALSE,stringsAsFactors=FALSE,na.strings=character())
  if(!all(c("record_index","title") %in% names(x))) stop("Manifest requires record_index and title.")
  if(anyDuplicated(x$record_index)) stop("Duplicate manifest record_index.")
  held <- x[FALSE,,drop=FALSE]
  if(workflow=="no_doi") {
    needed <- c("validation_status","source_drive_file_id","source_size_bytes","source_md5")
    if(length(setdiff(needed,names(x)))) stop("Inventory missing: ",paste(setdiff(needed,names(x)),collapse=", "))
    held <- x[x$validation_status!="verified",,drop=FALSE]
    x <- x[x$validation_status=="verified",,drop=FALSE]
    x$source_file_id <- x$source_drive_file_id
    x$source_size <- x$source_size_bytes
    x$pdf_verified <- TRUE
    x$pdf_found <- TRUE
    x$pdf_filename <- paste0(x$record_index,".pdf")
  } else {
    if(!all(c("action","screening_restoration_reviewed") %in% names(x)) ||
       any(x$action!="append_for_full_text_screening") ||
       any(tolower(x$screening_restoration_reviewed)!="true"))
      stop("Country records must be explicitly reviewed for full-text-screening restoration.")
    needed <- c("rec_number","journal_name","year","abstract","doi","pdf_found","pdf_verified")
    if(length(setdiff(needed,names(x)))) stop("Country manifest missing: ",paste(setdiff(needed,names(x)),collapse=", "))
    for(nm in c("pdf_found","pdf_verified")) {
      if(any(!tolower(x[[nm]]) %in% c("true","false"))) stop("Invalid logical field: ",nm)
      x[[nm]] <- tolower(x[[nm]])=="true"
    }
  }
  if(!nrow(x)) stop("No verified records available; review held inventory.")
  list(records=x,held=held)
}
recovery_snapshot_adapter <- function(directory) {
  required <- c("sheet.json","source_files.json","destination_files.json")
  paths <- file.path(directory,required)
  if(!all(file.exists(paths))) stop("Snapshot directory needs ",paste(required,collapse=", "))
  sheet <- jsonlite::fromJSON(paths[[1]],simplifyVector=FALSE)
  sheet$headers <- unlist(sheet$headers,use.names=FALSE)
  src <- jsonlite::fromJSON(paths[[2]],simplifyVector=TRUE)
  dst <- jsonlite::fromJSON(paths[[3]],simplifyVector=TRUE)
  empty <- data.frame(id=character(),name=character(),mime_type=character(),
    md5_checksum=character(),size=character(),folder_id=character(),stringsAsFactors=FALSE)
  if(!length(src)) src <- empty
  if(!length(dst)) dst <- empty
  all_files <- rbind(src,dst)
  forbidden <- function(...) stop("Offline snapshot adapter cannot write.")
  list(read_sheet=function(...) sheet,
    list_files=function(folder_id,name=NULL,...) {
      selected <- all_files[all_files$folder_id==folder_id,,drop=FALSE]
      if(!is.null(name)) selected <- selected[selected$name==name,,drop=FALSE]
      selected
    },
    copy_file=forbidden,write_plan=forbidden)
}
recovery_sync_main <- function(workflow,repo_root,args=commandArgs(trailingOnly=TRUE)) {
  opt <- recovery_cli_options(args,workflow,repo_root)
  if(opt$help) { recovery_cli_help(workflow); return(invisible(NULL)) }
  if(!requireNamespace("jsonlite",quietly=TRUE)) stop("Install jsonlite before running.")
  input <- recovery_cli_manifest(opt$manifest,workflow)
  config <- c(opt[c("sheet_id","sheet_tab","source_folder_id","destination_folder_id","receipt_dir")],list(workflow=workflow))
  dir.create(opt$receipt_dir,recursive=TRUE,showWarnings=FALSE)
  if(nrow(input$held)) {
    held_path <- file.path(opt$receipt_dir,paste0("held_",workflow,"_",format(Sys.time(),"%Y%m%dT%H%M%S"),"_",Sys.getpid(),".csv"))
    write.csv(input$held,held_path,row.names=FALSE,na="")
    cat(nrow(input$held)," records held for PDF identity review; excluded from copies and updates.\n",
        "Held ledger: ",held_path,"\n",sep="")
  }
  adapter <- if(nzchar(opt$snapshot_dir)) recovery_snapshot_adapter(opt$snapshot_dir) else recovery_google_adapter(config)
  result <- recovery_sync_run(input$records,config,adapter=adapter,apply=opt$apply)
  cat("Status:",result$receipt$status,"\n")
  for(nm in names(result$plan$summary)) cat(nm,": ",result$plan$summary[[nm]],"\n",sep="")
  cat("Receipt:",file.path(result$receipt_dir,"receipt.json"),"\n")
  invisible(result)
}
