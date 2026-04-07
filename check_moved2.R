library(arrow); library(tidyverse)
p <- read_parquet("output/psid_family_year_2001_latest.parquet")

# WHY MOVED 1ST codes (from PSID codebook):
# 1 = Purposive, productive reasons
# 2 = Response to outside event (involuntary)
# 3 = To be nearer to relatives, friends, work
# 4 = Expansion/contraction of family
# 5 = To get own home/better housing
# 6 = Saving money/lower costs
# 7 = Neighborhood, better area
# 8 = Involuntary/forced (eviction, condemned, etc)
# 9 = Other
# 10-12 = (2021+ additional codes)

# Weighted vs unweighted move rate
cat("=== Weighted vs Unweighted move rate ===\n")
p |>
  mutate(
    val = as.numeric(`WHY MOVED 1ST`),
    moved = case_when(
      is.na(val) ~ 0L,
      val >= 97  ~ NA_integer_,
      val > 0    ~ 1L,
      TRUE       ~ 0L
    )
  ) |>
  filter(!is.na(moved), !is.na(family_weight), family_weight > 0) |>
  group_by(wave) |>
  summarise(
    n = n(),
    unwt_pct = round(100 * mean(moved), 1),
    wt_pct   = round(100 * weighted.mean(moved, family_weight), 1),
    .groups = "drop"
  ) |>
  print(n = 12)

# Check: is this "WHETHER MOVED" or "WHY MOVED"?
# Look at the variable name in .sps files
cat("\n=== Checking variable label ===\n")
sps_files <- list.files("data_raw", pattern = "FAM.*\\.sps$", full.names = TRUE)
for (f in sps_files[1:3]) {
  cat("\n", basename(f), ":\n")
  lines <- readLines(f, warn = FALSE)
  # Find lines related to our move variable
  move_lines <- grep("MOVED|ER17|ER21", lines, value = TRUE, ignore.case = TRUE)
  # Show lines mentioning "WHY MOVED" or the ER codes we use
  cat(head(move_lines, 5), sep = "\n")
}

