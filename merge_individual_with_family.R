################################################################################
# Merge Individual-Level IDs with Family-Level Panel
################################################################################
# This script merges the permanent person identifiers from the individual file
# with the existing family-level panel data to create stable person/couple IDs
################################################################################

library(tidyverse)
library(arrow)

cat("\n")
cat(strrep("=", 80), "\n", sep = "")
cat("MERGING INDIVIDUAL IDs WITH FAMILY PANEL\n")
cat(strrep("=", 80), "\n\n", sep = "")

# Load individual data
cat("Loading individual data...\n")
individual_data <- read_parquet("output/individual_data.parquet")
cat("  ✓ Loaded", nrow(individual_data), "individuals\n\n")

# Load existing family panel
cat("Loading family panel...\n")
family_panel <- read_parquet("output/panel_with_calculated_vars.parquet")
cat("  ✓ Loaded", nrow(family_panel), "family-year observations\n\n")

# Reshape individual data to long format (one row per person-year)
cat("Reshaping individual data to long format...\n")
individual_long <- individual_data |>
  select(
    person_id, id_1968, person_number, sex, sample_nonsample,
    starts_with("interview_num_"),
    starts_with("seq_num_"),
    starts_with("rel_to_head_")
  ) |>
  pivot_longer(
    cols = -c(person_id, id_1968, person_number, sex, sample_nonsample),
    names_to = c(".value", "year"),
    names_pattern = "(.+)_(\\d{4})"
  ) |>
  rename(
    interview_num = interview_num,
    seq_num = seq_num,
    rel_to_head = rel_to_head
  ) |>
  mutate(year = as.integer(year)) |>
  filter(interview_num > 0)  # Keep only years where person was interviewed

cat("  ✓ Reshaped to", nrow(individual_long), "person-year observations\n\n")

# Identify heads and spouses
cat("Identifying heads and spouses...\n")
heads_spouses <- individual_long |>
  filter(rel_to_head %in% c(10, 20, 22)) |>  # 10=Head, 20/22=Spouse
  mutate(
    role = case_when(
      rel_to_head == 10 ~ "head",
      rel_to_head %in% c(20, 22) ~ "spouse",
      TRUE ~ "other"
    )
  ) |>
  select(year, interview_num, role, person_id, id_1968, person_number, sex)

cat("  ✓ Found", sum(heads_spouses$role == "head"), "head observations\n")
cat("  ✓ Found", sum(heads_spouses$role == "spouse"), "spouse observations\n\n")

# Check for duplicates (multiple people with same role in same family-year)
cat("Checking for duplicate roles...\n")
duplicates <- heads_spouses |>
  group_by(year, interview_num, role) |>
  filter(n() > 1) |>
  ungroup()

if (nrow(duplicates) > 0) {
  cat("  ⚠ Warning: Found", nrow(duplicates), "duplicate role assignments\n")
  cat("  Keeping first occurrence for each family-year-role\n")

  heads_spouses <- heads_spouses |>
    group_by(year, interview_num, role) |>
    slice(1) |>
    ungroup()
}

# Pivot to get head and spouse in same row
cat("Creating head-spouse pairs...\n")
head_spouse_pairs <- heads_spouses |>
  pivot_wider(
    id_cols = c(year, interview_num),
    names_from = role,
    values_from = c(person_id, id_1968, person_number, sex),
    names_glue = "{role}_{.value}"
  )

cat("  ✓ Created", nrow(head_spouse_pairs), "family-year pairs\n\n")

# Merge with family panel
cat("Merging with family panel...\n")
cat("  Family panel has", ncol(family_panel), "columns\n")
cat("  Family panel columns:", paste(head(names(family_panel), 10), collapse = ", "), "...\n")

# Check what columns are available for merging
has_year <- "year" %in% names(family_panel)
has_interview_year <- "interview_year" %in% names(family_panel)
has_wave <- "wave" %in% names(family_panel)
has_family_id <- "family_id" %in% names(family_panel)
has_interview_num <- "interview_num" %in% names(family_panel)

cat("  Available columns: year=", has_year, ", interview_year=", has_interview_year,
    ", wave=", has_wave, ", family_id=", has_family_id, "\n")

# Determine merge strategy
if (has_family_id && has_interview_year) {
  cat("  Merging on: interview_year + family_id\n")
  panel_with_ids <- family_panel |>
    left_join(
      head_spouse_pairs,
      by = c("interview_year" = "year", "family_id" = "interview_num")
    )
} else if (has_family_id && has_year) {
  cat("  Merging on: year + family_id\n")
  panel_with_ids <- family_panel |>
    left_join(
      head_spouse_pairs,
      by = c("year" = "year", "family_id" = "interview_num")
    )
} else if (has_family_id && has_wave) {
  cat("  Merging on: wave (as year) + family_id\n")
  # Convert wave to year for matching
  panel_with_ids <- family_panel |>
    mutate(year = wave) |>
    left_join(
      head_spouse_pairs,
      by = c("year" = "year", "family_id" = "interview_num")
    ) |>
    select(-year)  # Remove temporary year column
} else if (has_interview_num && has_year) {
  cat("  Merging on: year + interview_num\n")
  panel_with_ids <- family_panel |>
    left_join(
      head_spouse_pairs,
      by = c("year", "interview_num")
    )
} else {
  stop("Cannot determine merge columns! Need (year or interview_year or wave) + (family_id or interview_num)")
}

cat("  ✓ Merged successfully!\n")
cat("  Result:", nrow(panel_with_ids), "rows x", ncol(panel_with_ids), "columns\n\n")

# Create stable couple ID
cat("Creating stable couple identifiers...\n")
panel_with_ids <- panel_with_ids |>
  mutate(
    # Sort person IDs so couples get same ID regardless of who is "head"
    couple_id = case_when(
      !is.na(head_person_id) & !is.na(spouse_person_id) ~ 
        paste(pmin(head_person_id, spouse_person_id), 
              pmax(head_person_id, spouse_person_id), sep = "_"),
      !is.na(head_person_id) ~ paste0(head_person_id, "_single"),
      TRUE ~ NA_character_
    )
  )

cat("  ✓ Created couple IDs\n\n")

# Summary statistics
cat("Summary:\n")
cat("  Total observations:", nrow(panel_with_ids), "\n")
cat("  Observations with head ID:", sum(!is.na(panel_with_ids$head_person_id)), "\n")
cat("  Observations with spouse ID:", sum(!is.na(panel_with_ids$spouse_person_id)), "\n")
cat("  Unique couple IDs:", n_distinct(panel_with_ids$couple_id, na.rm = TRUE), "\n\n")

# Save result
cat("Saving to output/panel_with_stable_ids.parquet...\n")
write_parquet(panel_with_ids, "output/panel_with_stable_ids.parquet")

cat("\n")
cat(strrep("=", 80), "\n", sep = "")
cat("✓ MERGE COMPLETE!\n")
cat(strrep("=", 80), "\n\n", sep = "")

cat("New columns added:\n")
cat("  - head_person_id: Permanent ID for household head\n")
cat("  - spouse_person_id: Permanent ID for spouse\n")
cat("  - couple_id: Stable couple identifier\n")
cat("  - head_id_1968, head_person_number: Components of head ID\n")
cat("  - spouse_id_1968, spouse_person_number: Components of spouse ID\n")
cat("  - head_sex, spouse_sex: Sex of head and spouse\n\n")

