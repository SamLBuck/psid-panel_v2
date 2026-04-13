library(arrow); library(tidyverse)
p <- read_parquet("output/psid_family_year_2001_latest.parquet")

# Compute head birth year
p <- p |>
  mutate(
    head_age_n = as.numeric(head_age),
    birth_year = wave - head_age_n,
    family_key = paste(id_1968, birth_year, sep = "_")
  )

cat("=== Birth year approach ===\n")
cat("Unique family_key values:", n_distinct(p$family_key), "\n")
cat("Unique id_1968:", n_distinct(p$id_1968), "\n\n")

# Check: how many family_keys appear in multiple waves?
key_waves <- p |>
  group_by(family_key) |>
  summarise(
    n_waves = n_distinct(wave),
    n_rows = n(),
    .groups = "drop"
  )
cat("family_keys by number of waves present:\n")
print(table(key_waves$n_waves))

# Check: within a family_key x wave, are there duplicates?
dups <- p |>
  count(family_key, wave) |>
  filter(n > 1)
cat("\nfamily_key x wave combos with >1 row:", nrow(dups), "\n")
cat("(These are cases where two heads in same lineage have",
    "same birth year)\n")

# Test: does age increase by exactly 2 across waves?
cat("\n=== Age consistency within family_key ===\n")
age_check <- p |>
  arrange(family_key, wave) |>
  group_by(family_key) |>
  mutate(
    prev_age = lag(head_age_n, order_by = wave),
    prev_wave = lag(wave, order_by = wave),
    age_diff = head_age_n - prev_age,
    wave_diff = wave - prev_wave
  ) |>
  filter(!is.na(prev_age))

cat("Age difference between consecutive waves:\n")
print(table(age_check$age_diff))

cat("\nExpected diff = 2 (biennial):",
    round(100 * mean(age_check$age_diff == 2), 1), "%\n")
cat("Within 1 of expected (1-3):",
    round(100 * mean(age_check$age_diff %in% 1:3), 1), "%\n")

# Birth year jitter: could be +-1 due to birthday timing
# Use rounded birth year to handle this
cat("\n=== Rounded birth year (nearest 2) ===\n")
p2 <- p |>
  mutate(
    birth_year_r = round(birth_year / 2) * 2,
    family_key_r = paste(id_1968, birth_year_r, sep = "_")
  )

cat("Unique family_key_r:", n_distinct(p2$family_key_r), "\n")
dups2 <- p2 |> count(family_key_r, wave) |> filter(n > 1)
cat("family_key_r x wave combos with >1 row:", nrow(dups2), "\n")

# Compare: how many unique families per wave with each method?
cat("\n=== Families per wave ===\n")
p2 |>
  group_by(wave) |>
  summarise(
    n_rows = n(),
    n_id1968 = n_distinct(id_1968),
    n_key = n_distinct(family_key),
    n_key_r = n_distinct(family_key_r),
    .groups = "drop"
  ) |>
  print(n = 12)
