library(arrow); library(tidyverse)
p <- read_parquet("output/psid_family_year_2001_latest.parquet")

# Key = id_1968 + birth_year + head_sex
p <- p |>
  mutate(
    head_age_n = as.numeric(head_age),
    birth_year = wave - head_age_n,
    family_key = paste(id_1968, birth_year, head_sex, sep = "_")
  )

cat("=== id_1968 + birth_year + sex ===\n")
cat("Unique family_key:", n_distinct(p$family_key), "\n")

# Check duplicates within wave
dups <- p |> count(family_key, wave) |> filter(n > 1)
cat("Duplicates (family_key x wave with >1 row):", nrow(dups), "\n\n")

# Families per wave
cat("Families per wave:\n")
p |>
  group_by(wave) |>
  summarise(
    n_rows = n(),
    n_keys = n_distinct(family_key),
    n_dups = n_rows - n_keys,
    pct_dup = round(100 * n_dups / n_rows, 1),
    .groups = "drop"
  ) |>
  print(n = 12)

# Now deduplicate: for any remaining dups, keep oldest head
p_clean <- p |>
  group_by(family_key, wave) |>
  slice_max(head_age_n, n = 1, with_ties = FALSE) |>
  ungroup()

cat("\nAfter dedup:", nrow(p_clean), "rows\n")
cat("Rows dropped:", nrow(p) - nrow(p_clean), "\n")

# Validate: age should increase by 2 across waves
age_check <- p_clean |>
  arrange(family_key, wave) |>
  group_by(family_key) |>
  mutate(
    prev_age = lag(head_age_n, order_by = wave),
    age_diff = head_age_n - prev_age
  ) |>
  filter(!is.na(prev_age))

cat("\nAge diff distribution (should be 2):\n")
print(table(age_check$age_diff))
cat("\nPct with age_diff == 2:",
    round(100 * mean(age_check$age_diff == 2), 1), "%\n")

# Check: how many family_keys span 5+ waves?
# (genuine panel families tracked over time)
key_span <- p_clean |>
  group_by(family_key) |>
  summarise(n_waves = n_distinct(wave), .groups = "drop")
cat("\nFamily keys by panel length:\n")
print(table(key_span$n_waves))
cat("\nKeys with 5+ waves:", sum(key_span$n_waves >= 5),
    "(", round(100 * mean(key_span$n_waves >= 5), 1), "%)\n")
