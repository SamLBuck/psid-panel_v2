library(arrow); library(tidyverse)
p <- read_parquet("output/psid_family_year_2001_latest.parquet")

# Check: is there a "WHETHER MOVED" or "NUMBER OF MOVES" variable
# in the raw data that we're NOT pulling?
cat("=== All column names in panel ===\n")
cat(paste(sort(names(p)), collapse = "\n"), "\n")

# Check the .sps file for move-related variables
cat("\n=== Move-related variables in 2005 .sps ===\n")
lines <- readLines("data_raw/FAM2005ER.sps", warn = FALSE)
move_lines <- grep("MOV|RESID|ADDRESS|HOUSE|TENURE", lines,
                   value = TRUE, ignore.case = TRUE)
cat(head(move_lines, 30), sep = "\n")

cat("\n=== Move-related variables in 2001 .sps ===\n")
lines01 <- readLines("data_raw/FAM2001ER.sps", warn = FALSE)
move_lines01 <- grep("MOV|RESID|ADDRESS|HOUSE|TENURE", lines01,
                     value = TRUE, ignore.case = TRUE)
cat(head(move_lines01, 30), sep = "\n")

# Check: what's the question sequence?
# A49 = WHETHER MOVED, then A50 = WHY MOVED
cat("\n=== Looking for A49/A50 question labels in 2005 ===\n")
a49_lines <- grep("A49|A50|WTR MOVED|WHETHER MOVED|NUMBER.*(MOVE|TIME)",
                  lines, value = TRUE, ignore.case = TRUE)
cat(a49_lines, sep = "\n")

cat("\n=== Looking for A49/A50 question labels in 2001 ===\n")
a49_lines01 <- grep("A49|A50|A26|WTR MOVED|WHETHER MOVED|NUMBER.*(MOVE|TIME)",
                    lines01, value = TRUE, ignore.case = TRUE)
cat(a49_lines01, sep = "\n")
