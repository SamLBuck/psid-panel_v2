library(arrow)
library(tidyverse)

p <- read_parquet("output/psid_family_year_2001_latest.parquet")

donation_cols <- c(
  "DOLLAR AMT OF RELIGIOUS DONATIONS",
  "DOLLAR AMT OF NEEDY DONATIONS",
  "DOLLAR AMT OF HEALTH DONATIONS",
  "DOLLAR AMT OF EDUCATION DONATIONS"
)

# Check which donation columns have real data per wave
cat("=== Donation column availability per wave ===\n\n")
for (dc in donation_cols) {
  cat(dc, "\n")
  if (dc %in% names(p)) {
    p |>
      group_by(wave) |>
      summarise(
        n_total = n(),
        n_nonNA = sum(!is.na(.data[[dc]])),
        n_nonZero = sum(.data[[dc]] > 0 & .data[[dc]] < 999997, na.rm = TRUE),
        mean_valid = round(mean(
          if_else(.data[[dc]] > 0 & .data[[dc]] < 999997, .data[[dc]], NA_real_),
          na.rm = TRUE
        ), 0),
        .groups = "drop"
      ) |>
      print(n = 20)
  } else {
    cat("  NOT IN DATA\n")
  }
  cat("\n")
}
