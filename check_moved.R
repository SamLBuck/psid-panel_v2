library(arrow); library(tidyverse)
p <- read_parquet("output/psid_family_year_2001_latest.parquet")

cat("=== Distribution of WHY MOVED 1ST ===\n")
p |>
  mutate(val = as.numeric(`WHY MOVED 1ST`)) |>
  group_by(wave, val) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(wave) |>
  mutate(pct = round(100 * n / sum(n), 1)) |>
  filter(wave %in% c(2003, 2009, 2021)) |>
  arrange(wave, val) |>
  print(n = 60)

cat("\n=== Move rate by wave ===\n")
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
  group_by(wave) |>
  summarise(
    n = n(),
    n_moved = sum(moved == 1, na.rm = TRUE),
    pct_moved = round(100 * mean(moved, na.rm = TRUE), 1),
    .groups = "drop"
  ) |>
  print(n = 12)

