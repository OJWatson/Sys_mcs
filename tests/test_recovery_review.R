#!/usr/bin/env Rscript
# Independent adversarial review: identity, exact columns and resumability.
script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value=TRUE)[1])
root <- normalizePath(file.path(dirname(script),".."))
source(file.path(root,"R/recovery_sync_helpers.R"))
source(file.path(root,"R/recovery_sync_cli.R"))
checks <- 0L
check <- function(ok, label) { if(!isTRUE(ok)) stop(label,call.=FALSE); checks <<- checks+1L }
fails <- function(expr, message=NULL) {
  err <- tryCatch({force(expr);NULL},error=function(e)conditionMessage(e))
  check(!is.null(err),"Expected refusal")
  if(!is.null(message)) check(grepl(message,err,fixed=TRUE),paste("Wrong refusal:",err))
}
headers <- c("record_index","rec_number","title","reviewer","screening_status","decision","pdf_found","pdf_link","pdf_link","comments","doi","abstract","year","journal_name")
row <- c("1","10","One trial","OJ","PDF missing","Exclude","FALSE","","","Keep this comment","10.0/one","Abstract", "2024","Journal")
sheet <- list(headers=headers,rows=list(row),sheet_id=42L,grid_row_count=2L,grid_column_count=length(headers))
source_file <- data.frame(id="source-one",name="1.pdf",mime_type="application/pdf",md5_checksum=paste(rep("a",32),collapse=""),size="1000",folder_id="source",stringsAsFactors=FALSE)
empty_files <- source_file[FALSE,,drop=FALSE]
manifest <- data.frame(record_index="1",title="One trial",pdf_found=TRUE,pdf_filename="1.pdf",source_file_id="source-one",source_md5=source_file$md5_checksum,source_size="1000",pdf_verified=TRUE,source_folder_id="source",source_filename="1.pdf",stringsAsFactors=FALSE)
plan <- recovery_sync_plan(manifest,sheet,source_file,empty_files,"no_doi")
check(all(plan$cells$column %in% c(5L,7L,8L,9L)),"Planner touched protected physical columns")
check(identical(sheet$rows[[1]][c(4,6,10)],c("OJ","Exclude","Keep this comment")),"Planner mutated input")
fails(recovery_sheet_requests(plan,sheet),"unresolved Drive copies")

# Normalization must not invent value-derived vector names: expected-sheet
# mutation and fresh API JSON readback must compare by physical values.
normalized <- recovery_sheet(sheet);changed <- sheet
normalized$rows[[1]][7] <- "TRUE";changed$rows[[1]][7] <- "TRUE"
check(recovery_same_sheet(normalized,changed),"Normalization is not independent of prior cell values")

# Wrong title cannot be legitimised by the right record ID and PDF filename.
bad <- sheet; bad$rows[[1]][3] <- "Different trial"
fails(recovery_sync_plan(manifest,bad,source_file,empty_files,"no_doi"),"title mismatch")
# Existing target bytes must be exactly the reviewed source bytes.
badfile <- source_file; badfile$id <- "target";badfile$folder_id <- "destination";badfile$md5_checksum <- paste(rep("b",32),collapse="")
fails(recovery_sync_plan(manifest,sheet,source_file,badfile,"no_doi"),"checksum differs")
# Duplicate physical PDF columns and duplicate source filenames cannot be selected silently.
bad <- sheet;bad$headers <- c(bad$headers,"pdf_link")
fails(recovery_sync_plan(manifest,bad,source_file,empty_files,"no_doi"),"one or two physical")
badfiles <- rbind(source_file,transform(source_file,id="ambiguous"))
fails(recovery_sync_plan(manifest,sheet,badfiles,empty_files,"no_doi"),"Source filename missing or duplicated")

# Blank displayed links can still carry a hidden hyperlink/rich-text target.
metadata_sheet <- sheet
metadata_sheet$protected_metadata <- list("2:8"=list(userEnteredFormat=list(textFormat=list(link=list(uri="https://example.org/wrong")))))
fails(recovery_sync_plan(manifest,metadata_sheet,source_file,empty_files,"no_doi"),"formatting hyperlink")
metadata_sheet$protected_metadata <- list("2:10"=list(note="Preserve reviewer note"))
changed_metadata <- metadata_sheet;changed_metadata$protected_metadata[["2:10"]]$note <- "Concurrent reviewer edit"
check(!recovery_same_sheet(metadata_sheet,changed_metadata),"Concurrent reviewer metadata edit was ignored")

# A selected final report keeps its own identity; source filename can explicitly
# differ. Conference-only missing PDF is appended FALSE with no PDF link.
country <- rbind(transform(manifest,record_index="39727",title="Final article",pdf_filename="39727.pdf",source_filename="124095.pdf"),
 transform(manifest,record_index="108251",title="Conference article",pdf_found=FALSE,pdf_filename="108251.pdf",source_file_id="",source_md5="",source_size="",pdf_verified=FALSE,source_filename=""))
country$doi <- c("10.1371/journal.pone.0277377","10.1016/j.ekir.2024.02.057")
country$decision <- "Include" # Must never propagate even if a caller includes it.
country$comments <- "Do not copy into review comments"
country$reviewer <- "Do not assign"
renamed_source <- transform(source_file,name="124095.pdf")
cp <- recovery_sync_plan(country,sheet,renamed_source,empty_files,"country")
check(cp$summary$rows_appended==2L && cp$summary$copies==1L,"Wrong country plan counts")
check(identical(cp$files$pdf_filename,"39727.pdf") && identical(cp$files$source_filename,"124095.pdf"),"Final version remap lost")
check(all(cp$append_rows[[1]]$values[c(4,6,10)]==""),"New rows inherited screening decisions/comments")
check(cp$append_rows[[2]]$values[7]=="FALSE" && all(cp$append_rows[[2]]$values[c(8,9)]==""),"Poster index falsely counted as PDF")
requests <- recovery_sheet_requests(cp,sheet,allow_pending=TRUE)
check(requests[[1]]$appendDimension$length==2L,"Grid extension wrong")
pastes <- Filter(function(x)!is.null(x$copyPaste),requests)
check(setequal(vapply(pastes,function(x)x$copyPaste$pasteType,character(1)),c("PASTE_FORMAT","PASTE_DATA_VALIDATION")),"Wrong data/format copy mode")
updates <- Filter(function(x)!is.null(x$updateCells),requests)
check(all(vapply(updates,function(x)x$updateCells$fields=="userEnteredValue",logical(1))),"Requests overwrite notes/formatting")
# Existing missing-PDF candidate must not be downgraded after a human recovered it.
existing <- sheet;existing$rows[[1]][1:3] <- c("108251","132354","Conference article");existing$rows[[1]][5] <- "Screening complete";existing$rows[[1]][7:9] <- c("TRUE","manual-link","manual-link")
missing <- country[2,,drop=FALSE]
mp <- recovery_sync_plan(missing,existing,empty_files,empty_files,"country")
check(mp$summary$cells_updated==0L && mp$summary$rows_appended==0L,"Existing PDF status was downgraded")

# In-memory Google-shaped adapter simulates a lost copy response after success;
# the next invocation must reuse that exact file and protect all screening data.
state <- new.env();state$sheet <- sheet;state$dest <- empty_files;state$copies <- 0L;state$writes <- 0L;state$lose_response <- TRUE
adapter <- list(
 read_sheet=function() {
   s <- state$sheet;s$rows <- lapply(s$rows,function(r)as.list(unname(r)))
   recovery_sheet(s) # Same JSON-list -> normalized-sheet boundary as Google.
 },
 list_files=function(folder_id,name=NULL) {
   x <- if(folder_id=="source")source_file else state$dest
   if(!is.null(name))x <- x[x$name==name,,drop=FALSE]
   x
 },
 copy_file=function(source_id,destination_folder_id,filename) {
   state$copies <- state$copies+1L
   x <- source_file;x$id <- "copied-one";x$folder_id <- destination_folder_id;x$name <- filename
   state$dest <- rbind(state$dest,x)
   if(state$lose_response)stop("simulated lost copy response")
 },
 write_plan=function(plan,sheet) {
   state$writes <- state$writes+1L
   state$sheet <- recovery_expected_sheet(sheet,plan)
 })
config <- list(workflow="no_doi",sheet_id="test",sheet_tab="studies",source_folder_id="source",destination_folder_id="destination",receipt_dir=tempfile("review-receipts-"))
fails(recovery_sync_run(manifest,config,adapter,apply=TRUE),"simulated lost copy response")
check(state$copies==1L && state$writes==0L && nrow(state$dest)==1L,"Copy timeout caused unexpected sheet mutation")
failed_receipts <- list.files(config$receipt_dir,pattern="^receipt[.]json$",recursive=TRUE,full.names=TRUE)
check(length(failed_receipts)==1L,"Failed copy did not preserve one receipt")
failed_receipt <- jsonlite::fromJSON(failed_receipts[[1]],simplifyVector=FALSE)
check(identical(failed_receipt$status,"partial_or_uncertain_failure") && length(failed_receipt$copy_attempts)==1L,"Lost copy response was incorrectly classified as a pre-mutation failure")
state$lose_response <- FALSE
result <- recovery_sync_run(manifest,config,adapter,apply=TRUE)
check(state$copies==1L && state$writes==1L,"Rerun after lost copy response duplicated file")
check(identical(unname(state$sheet$rows[[1]][c(4,6,10)]),c("OJ","Exclude","Keep this comment")),"Apply changed screening fields")
result2 <- recovery_sync_run(manifest,config,adapter,apply=TRUE)
check(state$copies==1L && state$writes==1L && result2$plan$summary$cells_updated==0L,"Identical rerun was not a no-op")

# CLI protections do not depend on OAuth state or environment modes.
fails(recovery_cli_options(c("--apply","--snapshot-dir","anything"),"country",root),"read-only")
check(!recovery_cli_options(character(),"country",root)$apply,"CLI is not dry-run by default")
country_input <- recovery_cli_manifest(file.path(root,"data-raw/hrp_country_correction_records.csv"),"country")$records
check(nrow(country_input)==9L && sum(country_input$pdf_found)==6L,"Reviewed country CSV invalid")
unlink(config$receipt_dir,recursive=TRUE)
cat("Independent recovery review:",checks,"checks passed.\n")
