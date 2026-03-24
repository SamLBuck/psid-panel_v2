library(arrow)
library(tidyverse)

p <- read_parquet("output/psid_family_year_2001_latest.parquet")

dollar_cols <- c(
  "DOLLAR AMT OF RELIGIOUS DONATIONS",
  "DOLLAR AMT OF NEEDY DONATIONS",
  "DOLLAR AMT OF HEALTH DONATIONS",
  "DOLLAR AMT OF EDUCATION DONATIONS"
)

cat("=== Per-wave donation statistics ===\n\n")
for (dc in dollar_cols) {
  cat(dc, "\n")
  p |>
    group_by(wave) |>
    summarise(
      n_families = n(),
      n_valid = sum(!is.na(.data[[dc]]) & .data[[dc]] > 0 & .data[[dc]] < 999997),
      pct_donors = round(100 * n_valid / n_families, 1),
      mean_all = round(mean(if_else(.data[[dc]] < 999997, .data[[dc]], NA_real_), na.rm = TRUE), 0),
      mean_donors = round(mean(if_else(.data[[dc]] > 0 & .data[[dc]] < 999997, .data[[dc]], NA_real_), na.rm = TRUE), 0),
      .groups = "drop"
    ) |>
    print(n = 12)
  cat("\n")
}

# Check: is the "decline" just fewer families, or lower per-donor amounts?
cat("\n=== Total religious giving per wave ===\n")
p |>
  mutate(relig_clean = if_else(`DOLLAR AMT OF RELIGIOUS DONATIONS` < 999997,
                                `DOLLAR AMT OF RELIGIOUS DONATIONS`, NA_real_)) |>
  group_by(wave) |>
  summarise(
    n = n(),
    total_giving = sum(relig_clean, na.rm = TRUE),
    mean_all = round(mean(relig_clean, na.rm = TRUE), 0),
    mean_donors = round(mean(if_else(relig_clean > 0, relig_clean, NA_real_), na.rm = TRUE), 0),
    n_donors = sum(relig_clean > 0, na.rm = TRUE),
    pct_donors = round(100 * n_donors / n, 1),
    .groups = "drop"
  ) |>
  print(n = 12)

