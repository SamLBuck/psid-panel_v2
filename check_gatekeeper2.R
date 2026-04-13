library(tidyverse); library(readr); library(stringr)

# Parse .sps to get column positions
parse_sps <- function(sps_path) {
  lines <- readLines(sps_path, warn = FALSE)
  blob <- paste(lines, collapse = " ")
  mat <- str_match_all(blob,
    "(?i)\\b(ER[0-9A-Z_]+)\\s+(\\d+)\\s*-\\s*(\\d+)\\s*(\\([aA]\\))?")[[1]]
  tibble(
    var = mat[, 2],
    start = as.integer(mat[, 3]),
    end = as.integer(mat[, 4]),
    type = ifelse(!is.na(mat[, 5]) & mat[, 5] != "", "character", "numeric")
  ) |> distinct(var, .keep_all = TRUE)
}

read_vars <- function(stem, vars) {
  dict <- parse_sps(file.path("data_raw", paste0(stem, ".sps")))
  dict_sub <- dict |> filter(var %in% vars)
  if (nrow(dict_sub) == 0) return(NULL)
  ct <- paste(ifelse(dict_sub$type == "character", "c", "d"), collapse = "")
  read_fwf(
    file.path("data_raw", paste0(stem, ".txt")),
    fwf_positions(dict_sub$start, dict_sub$end, dict_sub$var),
    col_types = ct, progress = FALSE
  )
}

# 2001: ER17088 = WHETHER MOVED, ER17091 = WHY MOVED 1ST
cat("=== 2001 ===\n")
d01 <- read_vars("FAM2001ER", c("ER17088", "ER17091"))
cat("WHETHER MOVED (ER17088):\n")
print(table(d01$ER17088, useNA = "always"))
cat("\nWHY MOVED 1ST (ER17091):\n")
print(table(d01$ER17091, useNA = "always"))
cat("\nCross-tab (wtr_moved x why_moved):\n")
print(table(wtr = d01$ER17088, why = d01$ER17091, useNA = "always"))

# 2005: ER25098 = WHETHER MOVED, ER25101 = WHY MOVED 1ST
cat("\n=== 2005 ===\n")
d05 <- read_vars("FAM2005ER", c("ER25098", "ER25101"))
cat("WHETHER MOVED (ER25098):\n")
print(table(d05$ER25098, useNA = "always"))
cat("\nWHY MOVED 1ST (ER25101):\n")
print(table(d05$ER25101, useNA = "always"))
cat("\nCross-tab (wtr_moved x why_moved):\n")
print(table(wtr = d05$ER25098, why = d05$ER25101, useNA = "always"))

# 2021: check ER codes in .sps
cat("\n=== 2021 move variables ===\n")
lines21 <- readLines("data_raw/FAM2021ER.sps", warn = FALSE)
mv21 <- grep("MOV|A49|A50|A42|A44", lines21, value = TRUE,
             ignore.case = TRUE)
cat(head(mv21, 15), sep = "\n")
