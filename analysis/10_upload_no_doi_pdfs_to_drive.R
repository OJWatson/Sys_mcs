#!/usr/bin/env Rscript
# Reviewed Drive recovery. Default: dry run. --apply explicitly permits writes.
script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
repo_root <- normalizePath(file.path(dirname(script), ".."), mustWork = TRUE)
source(file.path(repo_root, "R", "recovery_sync_helpers.R"))
source(file.path(repo_root, "R", "recovery_sync_cli.R"))
recovery_sync_main("no_doi", repo_root)
