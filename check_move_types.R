library(arrow); library(tidyverse)
p <- read_parquet("output/psid_family_year_2001_latest.parquet")

# 1. Check if splitoffs' first wave inflates the move rate
cat("=== Move rate: first appearance vs returning families ===\n")
p |>
  arrange(family_id, wave) |>
  group_by(family_id) |>
  mutate(first_wave = (row_number() == 1)) |>
  ungroup() |>
  mutate(
    val = as.numeric(`WHY MOVED 1ST`),
    moved = case_when(
      is.na(val) ~ 0L,
      val >= 97  ~ NA_integer_,
      val > 0    ~ 1L,
      TRUE       ~ 0L
    )
  ) |>
  filter(!is.na(moved), !is.na(family_weight), family_weight > 0) |>
  group_by(first_wave) |>
  summarise(
    n = n(),
    pct_moved = round(100 * mean(moved), 1),
    wt_pct_moved = round(100 * weighted.mean(moved, family_weight), 1),
    .groups = "drop"
  ) |>
  print()

# 2. State changes as proxy for long-distance moves
cat("\n=== State change vs WHY MOVED ===\n")
state_moves <- p |>
  arrange(family_id, wave) |>
  group_by(family_id) |>
  mutate(
    prev_state = lag(`Current State`, order_by = wave),
    state_changed = (!is.na(prev_state) & `Current State` != prev_state)
  ) |>
  ungroup() |>
  mutate(
    val = as.numeric(`WHY MOVED 1ST`),
    moved = case_when(
      is.na(val) ~ 0L,
      val >= 97  ~ NA_integer_,
      val > 0    ~ 1L,
      TRUE       ~ 0L
    )
  ) |>
  filter(!is.na(moved), !is.na(prev_state),
         !is.na(family_weight), family_weight > 0)

cat("Cross-tab: moved x state_changed\n")
state_moves |>
  count(moved, state_changed) |>
  mutate(pct = round(100 * n / sum(n), 1)) |>
  print()

cat("\n=== Move rate by wave: any move vs interstate ===\n")
state_moves |>
  group_by(wave) |>
  summarise(
    n = n(),
    pct_any_move = round(100 * weighted.mean(moved, family_weight), 1),
    pct_interstate = round(100 * weighted.mean(
      as.integer(moved == 1 & state_changed), family_weight), 1),
    .groups = "drop"
  ) |>
  print(n = 12)
