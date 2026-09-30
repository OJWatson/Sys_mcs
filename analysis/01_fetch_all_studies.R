#1. Load packages
install.packages("dplyr")
install.packages("reader")

library(dplyr)
library(reader)

#2. Define paths
raw_paths <- "analysis/data-raw/screening_data.csv"
included_csv_path <- "analysis/data-derived/included_studies.csv"
included_rds_path <- "analysis/data-derived/included_studies.rds"

#3. Copy screening file from the device
file.copy(from = "~/Desktop/PhD files /1st year/Systematic review /Oj AI LLM/screening_data.csv",
          to = "analysis/data-raw/screening_data.csv"
          )

#4. Read Screening data
screened_df <- read.csv("analysis/data-raw/screening_data.csv")
screened_df

#5. Inspect data
names(screened_df)
head(screened_df)

#6. Declare include column and value
include_column <- "decision"
include_value <- "Include"
table(screened_df[[include_column]], useNA = "ifany")

#7. Filter included studies
included_df <- screened_df %>%
  filter(.data[[include_column]] == include_value)

#8. Checks
cat("Total rows read:", nrow(screened_df), "\n")
cat("Included rows kept:", nrow(included_df), "\n")

#9. Save outputs
write.csv(included_df, included_csv_path)
saveRDS(included_df, included_rds_path)
