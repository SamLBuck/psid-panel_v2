library(arrow); library(tidyverse)

# Read the raw 2005 data and check both the gatekeeper
# and "WHY MOVED" directly
source("rebuild_panel2.R")  # no, too slow

# Instead, read just 2005 raw
parse_sps_positions <- function(dict_path) {
  lines <- readLines(dict_path, warn = FALSE)
  dl <- which(grepl("^\\s*DATA\\s+LIST", lines, ignore.case = TRUE))
  if (!length(dl)) return(tibble(var = character(), start = integer(),
                                  end = integer(), width = integer(),
                                  type = character()))
  after <- lines[seq(dl[1], length(lines))]
  stop_idx <- which(grepl("^\\s*VARIABLE\\s+LABELS", after, ignore.case = TRUE))
  if (length(stop_idx)) after <- after[seq(1, stop_idx[1] - 1)]
  after <- gsub("\\t", " ", after)
  after <- gsub("\\*.*$", "", after)
  after <- trimws(after)
  after <- after[after != ""]
  sl <- which(grepl("^/", after))
  if (length(sl)) after <- after[seq(sl[1], length(after))]
  blob <- paste(after, collapse = " ")
  m <- regmatches(blob, gregexpr(
    "\\b([A-Za-z0-9_]+)\\s+(\\d+)\\s*-\\s*(\\d+)\\s*(\\([aA]\\))?",
    blob
  ))[[1]]
  # Use str_match_all for cleaner extraction
  library(stringr)
  mat <- str_match_all(blob,
    "(?i)\\b([A-Za-z0-9_]+)\\s+(\\d+)\\s*-\\s*(\\d+)\\s*(\\([aA]\\))?")[[1]]
  tibble(
    var = mat[, 2], start = as.integer(mat[, 3]),
    end = as.integer(mat[, 4]),
    type = ifelse(!is.na(mat[, 5]) & mat[, 5] != "", "character", "numeric")
  ) |> mutate(width = end - start + 1L) |> distinct(var, .keep_all = TRUE) |>
    arrange(start)
}

read_fwf_cols <- function(stem, cols) {
  dict <- parse_sps_positions(file.path("data_raw", paste0(stem, ".sps")))
  dict_sub <- dict |> filter(var %in% cols)
  col_types <- paste(ifelse(dict_sub$type == "character", "c", "d"),
                     collapse = "")
  readr::read_fwf(
    file = file.path("data_raw", paste0(stem, ".txt")),
    col_positions = readr::fwf_positions(dict_sub$start, dict_sub$end,
                                         dict_sub$var),
    col_types = col_types
  )
}

# 2005: gatekeeper = ER25098, why moved = ER25101
cat("=== 2005: WTR MOVED (ER25098) vs WHY MOVED (ER25101) ===\n")
d05 <- read_fwf_cols("FAM2005ER", c("ER25098", "ER25101"))
cat("WTR MOVED distribution:\n")
print(table(d05$ER25098, useNA = "always"))
cat("\nWHY MOVED distribution:\n")
print(table(d05$ER25101, useNA = "always"))

cat("\nCross-tab: WTR MOVED x WHY MOVED\n")
print(table(wtr = d05$ER25098, why = d05$ER25101, useNA = "always"))

# 2001: gatekeeper = ER17088, why moved = ER17091
cat("\n=== 2001: MOVED SINCE SPG (ER17088) vs WHY MOVED (ER17091) ===\n")
d01 <- read_fwf_cols("FAM2001ER", c("ER17088", "ER17091"))
cat("MOVED SINCE SPG distribution:\n")
print(table(d01$ER17088, useNA = "always"))
cat("\nWHY MOVED 1ST distribution:\n")
print(table(d01$ER17091, useNA = "always"))
cat("\nCross-tab:\n")
print(table(wtr = d01$ER17088, why = d01$ER17091, useNA = "always"))
