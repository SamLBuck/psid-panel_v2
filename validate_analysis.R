library(tidyverse); library(arrow); library(fixest); library(here)

cat("========== 1. DATA PIPELINE VALIDATION ==========\n\n")

panel <- read_parquet(here("output", "psid_family_year_2001_latest.parquet"))
cat("Raw panel:", nrow(panel), "rows,", ncol(panel), "cols\n")
cat("Waves:", paste(sort(unique(panel$wave)), collapse=", "), "\n")
cat("Unique id_1968:", n_distinct(panel$id_1968), "\n\n")

# Sentinel filtering check
nonrelig_cols <- c("DOLLAR AMT OF NEEDY DONATIONS",
                   "DOLLAR AMT OF HEALTH DONATIONS",
                   "DOLLAR AMT OF EDUCATION DONATIONS")
relig_col <- "DOLLAR AMT OF RELIGIOUS DONATIONS"

cat("--- Sentinel check: values >= 99997 in non-relig cols ---\n")
for (cn in nonrelig_cols) {
  bad <- sum(panel[[cn]] >= 99997, na.rm = TRUE)
  cat(cn, ":", bad, "sentinel values\n")
}
bad_rel <- sum(panel[[relig_col]] >= 999997, na.rm = TRUE)
cat(relig_col, ":", bad_rel, "sentinel values\n\n")

# Build analysis exactly as Rmd does
analysis <- panel |>
  mutate(
    across(any_of(nonrelig_cols), ~ if_else(.x >= 99997, NA_real_, .x)),
    across(any_of(relig_col), ~ if_else(.x >= 999997, NA_real_, .x))
  ) |>
  mutate(
    total_giving = rowSums(
      across(all_of(c(relig_col, nonrelig_cols)),
             ~ replace_na(pmax(.x, 0), 0)), na.rm = TRUE),
    log_giving = log1p(total_giving),
    moved = case_when(
      is.na(`WHY MOVED 1ST`) ~ 0L,
      `WHY MOVED 1ST` >= 97 ~ NA_integer_,
      `WHY MOVED 1ST` > 0 ~ 1L,
      TRUE ~ 0L),
    religious = case_when(
      is.na(Religious_Preference) ~ NA_integer_,
      Religious_Preference %in% c(0, 97, 98, 99) ~ 0L,
      TRUE ~ 1L),
    age = as.numeric(head_age), age2 = age^2
  ) |>
  mutate(
    birth_year = wave - age,
    family_key = paste(id_1968, birth_year, head_sex, sep = "_")
  ) |>
  filter(!is.na(id_1968), !is.na(religious),
         !is.na(family_weight), family_weight > 0)

# Dedup within family_key x wave
n_pre <- nrow(analysis)
analysis <- analysis |>
  group_by(family_key, wave) |>
  slice_max(age, n = 1, with_ties = FALSE) |>
  ungroup()
cat("After filters + dedup:", nrow(analysis), "of", n_pre, "rows\n")

# Check total_giving is reasonable after sentinel removal
cat("\n--- Weighted total giving by wave (should be ~1000-1150) ---\n")
analysis |>
  group_by(wave) |>
  summarise(wt_mean = round(weighted.mean(total_giving, family_weight,
                                           na.rm = TRUE), 0),
            max_val = max(total_giving, na.rm = TRUE),
            .groups = "drop") |>
  print(n = 12)

cat("\n========== 2. LAG VALIDATION ==========\n\n")

# Add lag using family_key (clean longitudinal ID)
analysis <- analysis |>
  arrange(family_key, wave) |>
  group_by(family_key) |>
  mutate(religious_lag = lag(religious, order_by = wave)) |>
  ungroup()

# Check: does lagged religiosity make sense?
# Within a lineage, religiosity should be VERY stable
cat("--- Religious stability within lineage ---\n")
stability <- analysis |>
  filter(!is.na(religious_lag)) |>
  mutate(same = (religious == religious_lag)) |>
  summarise(
    n = n(),
    pct_same = round(100 * mean(same), 1)
  )
cat("Religiosity matches prior wave:", stability$pct_same, "% of",
    stability$n, "obs\n")

# Check: any remaining duplicates within family_key x wave?
cat("\n--- Remaining duplicates within family_key x wave ---\n")
dups <- analysis |>
  count(family_key, wave) |>
  filter(n > 1)
cat("family_key x wave combos with >1 row:", nrow(dups), "\n")
cat("(Should be 0 after dedup)\n")

cat("\n========== 3. REGRESSION VALIDATION ==========\n\n")

analysis_lag <- analysis |>
  filter(!is.na(religious_lag), !is.na(moved))
cat("Regression sample:", nrow(analysis_lag), "obs\n")
cat("Unique id_1968:", n_distinct(analysis_lag$id_1968), "\n\n")

m1 <- feols(log_giving ~ moved | family_key + wave,
            data = analysis_lag, weights = ~family_weight)
m2 <- feols(log_giving ~ moved * religious_lag | family_key + wave,
            data = analysis_lag, weights = ~family_weight)
m3 <- feols(log_giving ~ moved * religious_lag + age + age2 |
              family_key + wave,
            data = analysis_lag, weights = ~family_weight)

cat("--- Model results ---\n")
etable(m1, m2, m3,
       headers = c("Baseline", "Interaction", "Controls"),
       vcov = "cluster", se.below = TRUE)

cat("\n--- Key coefficients (Model 2) ---\n")
coefs2 <- coef(m2)
cat("moved:", round(coefs2["moved"], 4), "\n")
cat("religious_lag:", round(coefs2["religious_lag"], 4), "\n")
cat("moved:religious_lag:", round(coefs2["moved:religious_lag"], 4), "\n")

cat("\n--- Sanity checks ---\n")
cat("moved coef should be NEGATIVE (moving reduces giving):",
    ifelse(coefs2["moved"] < 0, "PASS", "CHECK"), "\n")
cat("religious_lag coef should be POSITIVE (religious give more):",
    ifelse(coefs2["religious_lag"] > 0, "PASS", "CHECK"), "\n")
cat("N fixed effects (family_key):", m2$fixef_sizes["family_key"], "\n")
cat("N fixed effects (wave):", m2$fixef_sizes["wave"], "\n")
