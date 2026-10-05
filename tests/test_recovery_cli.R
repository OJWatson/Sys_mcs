#!/usr/bin/env Rscript
source("R/recovery_sync_cli.R")
root <- normalizePath(".")
checks <- 0L
ok <- function(x) { stopifnot(isTRUE(x)); checks <<- checks+1L }
fails <- function(expr) { ok(inherits(tryCatch(force(expr),error=function(e)e),"error")) }
opt <- recovery_cli_options(character(),"country",root)
ok(!opt$apply)
ok(opt$sheet_id=="1foHMPBCWYzWX8975UKjdacE2i_j_48n8uTMdAQ-ETcg")
fails(recovery_cli_options(c("--apply","--dry-run"),"country",root))
fails(recovery_cli_options(c("--apply","--snapshot-dir","fixture"),"country",root))
fails(recovery_cli_options("--sheet-id","country",root))
fails(recovery_cli_options("--allow-share-anyone","country",root))
country <- recovery_cli_manifest(file.path(root,"data-raw/hrp_country_correction_records.csv"),"country")
ok(nrow(country$records)==9L && sum(country$records$pdf_found)==6L)
ok(!any(c("Include","Exclude") %in% names(country$records)))
ok("39727" %in% country$records$record_index && !"124095" %in% country$records$record_index)
replacement <- country$records[country$records$record_index=="39727",]
ok(replacement$source_filename=="124095.pdf" && replacement$pdf_filename=="39727.pdf")
nodo <- recovery_cli_manifest(file.path(root,"data-raw/recovered_pdf_inventory.csv"),"no_doi")
all <- read.csv("data-raw/recovered_pdf_inventory.csv",colClasses="character")
ok(nrow(nodo$records)+nrow(nodo$held)==nrow(all))
ok(all(nodo$records$pdf_verified) && all(nodo$records$pdf_found))
ok(!length(intersect(nodo$records$record_index,nodo$held$record_index)))
ok(all(nodo$records$pdf_filename==paste0(nodo$records$record_index,".pdf")))
tmp <- tempfile(fileext=".csv")
bad <- country$records;bad$action[1] <- "do_not_append"
write.csv(bad,tmp,row.names=FALSE)
fails(recovery_cli_manifest(tmp,"country"))
unlink(tmp)
cat("Passed",checks,"CLI/curated-manifest checks (offline).\n")
