# 1. Download the screened studies and filter the included studies

# 2. Identify an R pacakge to help download from google drive
install.packages("googledrive")
library(googledrive)

# Authenticate with Google
drive_auth()

# 3. Download the file
folder <- drive_get("PDFs Zotero")

# List files
files <- drive_ls(folder)

# Download all PDFs

pdf_files <- subset(files, grepl("\\.pdf$", name,
    ignore.case = TRUE))

for (i in seq_len(nrow(pdf_files))) {
  drive_download(
    file = pdf_files[i, ],
    path = file.path ("analysis/data-raw/", pdf_files$name[i]),
    overwrite = TRUE
  )
}
  )
}
# Checks on the file

# Filter the file
saveRDS(df, "analysis/data-derived/included_studies.rds")
