library(arrow); library(tidyverse)
p <- read_parquet("output/psid_family_year_2001_latest.parquet")
p <- p |> filter(!is.na(family_weight), family_weight > 0)

cats <- c(
  "DOLLAR AMT OF RELIGIOUS DONATIONS",
  "DOLLAR AMT OF NEEDY DONATIONS",
  "DOLLAR AMT OF HEALTH DONATIONS",
  "DOLLAR AMT OF EDUCATION DONATIONS"
)

cat("=== Weighted mean by category and wave ===\n")
for (cat_name in cats) {
  cat("\n---", cat_name, "---\n")
  p |>
    mutate(val = if_else(!!sym(cat_name) >= 999997, NA_real_,
                         as.numeric(!!sym(cat_name)))) |>
    group_by(wave) |>
    summarise(
      wt_mean = round(weighted.mean(val, family_weight, na.rm = TRUE), 0),
      wt_median = round(Hmisc::wtd.quantile(val, family_weight, probs = 0.5,
                                              na.rm = TRUE), 0),
      max_val = max(val, na.rm = TRUE),
      n_above_50k = sum(val > 50000, na.rm = TRUE),
      .groups = "drop"
    ) |>
    print(n = 12)
}

cat("\n=== Weighted mean TOTAL giving by wave ===\n")
p |>
  mutate(
    across(all_of(cats), ~ if_else(.x >= 999997, NA_real_, as.numeric(.x))),
    total = rowSums(across(all_of(cats),
                           ~ replace_na(pmax(.x, 0), 0)), na.rm = TRUE)
  ) |>
  group_by(wave) |>
  summarise(
    wt_mean = round(weighted.mean(total, family_weight, na.rm = TRUE), 0),
    p50 = round(Hmisc::wtd.quantile(total, family_weight, probs = 0.5,
                                     na.rm = TRUE), 0),
    p95 = round(Hmisc::wtd.quantile(total, family_weight, probs = 0.95,
                                     na.rm = TRUE), 0),
    p99 = round(Hmisc::wtd.quantile(total, family_weight, probs = 0.99,
                                     na.rm = TRUE), 0),
    max_val = max(total, na.rm = TRUE),
    .groups = "drop"
  ) |>
  print(n = 12)
