library(arrow); library(tidyverse)
p <- read_parquet("output/psid_family_year_2001_latest.parquet")

# Check: how many family_ids appear in multiple waves?
cat("=== family_id reuse across waves ===\n")
fid_waves <- p |>
  group_by(family_id) |>
  summarise(
    n_waves = n_distinct(wave),
    waves = paste(sort(unique(wave)), collapse = ","),
    n_lineages = n_distinct(id_1968),
    .groups = "drop"
  )

cat("family_ids appearing in 1 wave:", sum(fid_waves$n_waves == 1), "\n")
cat("family_ids appearing in 2+ waves:", sum(fid_waves$n_waves >= 2), "\n")
cat("family_ids appearing in ALL 11 waves:", sum(fid_waves$n_waves == 11), "\n\n")

# Key test: among family_ids in 2+ waves, how many map to MULTIPLE id_1968?
multi_wave <- fid_waves |> filter(n_waves >= 2)
cat("Of those in 2+ waves:\n")
cat("  Same id_1968 throughout:", sum(multi_wave$n_lineages == 1), "\n")
cat("  DIFFERENT id_1968 (recycled!):", sum(multi_wave$n_lineages > 1), "\n\n")

# Show examples of recycled IDs
recycled <- multi_wave |> filter(n_lineages > 1) |> head(5)
cat("Examples of recycled family_ids:\n")
for (fid in recycled$family_id) {
  cat("\nfamily_id =", fid, "\n")
  p |>
    filter(family_id == fid) |>
    select(family_id, id_1968, wave, `Current State`) |>
    arrange(wave) |>
    print()
}

# Test: state lag using id_1968 instead
cat("\n=== State change using id_1968 for lag ===\n")
state_check <- p |>
  arrange(id_1968, wave) |>
  group_by(id_1968) |>
  mutate(
    prev_state = lag(`Current State`, order_by = wave)
  ) |>
  ungroup() |>
  mutate(
    val = as.numeric(`WHY MOVED 1ST`),
    moved = case_when(
      is.na(val) ~ 0L,
      val >= 97  ~ NA_integer_,
      val > 0    ~ 1L,
      TRUE       ~ 0L
    ),
    state_changed = (!is.na(prev_state) &
                       `Current State` != prev_state)
  ) |>
  filter(!is.na(moved), !is.na(prev_state),
         !is.na(family_weight), family_weight > 0)

cat("Cross-tab: moved x state_changed (id_1968 lag)\n")
state_check |>
  count(moved, state_changed) |>
  mutate(pct = round(100 * n / sum(n), 1)) |>
  print()

cat("\nMove rate by wave: any move vs interstate\n")
state_check |>
  group_by(wave) |>
  summarise(
    n = n(),
    pct_any_move = round(100 * weighted.mean(moved, family_weight), 1),
    pct_interstate = round(100 * weighted.mean(
      as.integer(moved == 1 & state_changed), family_weight), 1),
    pct_intrastate = round(100 * weighted.mean(
      as.integer(moved == 1 & !state_changed), family_weight), 1),
    .groups = "drop"
  ) |>
  print(n = 12)
