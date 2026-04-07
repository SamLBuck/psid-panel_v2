library(arrow); library(tidyverse)
p <- read_parquet("output/psid_family_year_2001_latest.parquet")
p <- p |> filter(!is.na(family_weight), family_weight > 0)

nonrelig_cols <- c(
  "DOLLAR AMT OF NEEDY DONATIONS",
  "DOLLAR AMT OF HEALTH DONATIONS",
  "DOLLAR AMT OF EDUCATION DONATIONS"
)
relig_col <- "DOLLAR AMT OF RELIGIOUS DONATIONS"

p2 <- p |>
  mutate(
    across(any_of(nonrelig_cols),
           ~ if_else(.x >= 99997, NA_real_, .x)),
    across(any_of(relig_col),
           ~ if_else(.x >= 999997, NA_real_, .x)),
    total = rowSums(
      across(c(any_of(relig_col), any_of(nonrelig_cols)),
             ~ replace_na(pmax(.x, 0), 0)),
      na.rm = TRUE
    )
  )

cat("=== Weighted mean TOTAL giving (fixed sentinels) ===\n")
p2 |>
  group_by(wave) |>
  summarise(
    wt_mean = round(weighted.mean(total, family_weight, na.rm = TRUE), 0),
    .groups = "drop"
  ) |>
  print(n = 12)

cat("\n=== Weighted mean by category (fixed) ===\n")
for (cn in c(relig_col, nonrelig_cols)) {
  cat("\n---", cn, "---\n")
  p2 |>
    group_by(wave) |>
    summarise(
      wt_mean = round(weighted.mean(!!sym(cn), family_weight, na.rm = TRUE), 0),
      max_val = max(!!sym(cn), na.rm = TRUE),
      .groups = "drop"
    ) |>
    print(n = 12)
}
