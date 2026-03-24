library(arrow)
library(tidyverse)
p <- read_parquet("output/psid_family_year_2001_latest.parquet")
cat("=== NEW PANEL ===\n")
cat("Rows:", nrow(p), "\n")
cat("Cols:", ncol(p), "\n")
cat("Has family_weight:", "family_weight" %in% names(p), "\n\n")

cat("Rows per wave:\n")
print(as.data.frame(table(p[["wave"]])))

cat("\nWeight summary:\n")
cat("  Non-zero:", sum(p[["family_weight"]] > 0, na.rm=TRUE), "\n")
cat("  Zero:", sum(p[["family_weight"]] == 0, na.rm=TRUE), "\n")
cat("  NA:", sum(is.na(p[["family_weight"]])), "\n")

cat("\nWeighted vs unweighted religious giving:\n")
p |>
  mutate(relig = if_else(`DOLLAR AMT OF RELIGIOUS DONATIONS` < 999997,
                          `DOLLAR AMT OF RELIGIOUS DONATIONS`, NA_real_)) |>
  group_by(wave) |>
  summarise(
    n = n(),
    unwt_mean = round(mean(relig, na.rm=TRUE), 0),
    wt_mean = round(weighted.mean(relig, family_weight, na.rm=TRUE), 0),
    n_donors = sum(relig > 0, na.rm=TRUE),
    pct_donors = round(100*n_donors/n, 1),
    .groups="drop"
  ) |> print(n=12)

