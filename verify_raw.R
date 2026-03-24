# Verify raw data against codebook statistics
# This reads directly from the raw .txt files using the .sps dictionary
# and compares to known codebook values for 2001

library(tidyverse)
library(readr)
library(stringr)
library(glue)

DATA_DIR <- "data_raw"

parse_sps_positions <- function(dict_path) {
  lines <- read_lines(dict_path)
  dl <- which(str_detect(lines, regex("^\\s*DATA\\s+LIST", ignore_case = TRUE)))
  if (!length(dl)) return(tibble(var = character(), start = integer(), end = integer(), width = integer(), type = character()))
  after <- lines[seq(dl[1], length(lines))]
  stop_idx <- which(str_detect(after, regex("^\\s*VARIABLE\\s+LABELS", ignore_case = TRUE)))
  if (length(stop_idx)) after <- after[seq(1, stop_idx[1] - 1)]
  after <- gsub("\\t", " ", after)
  after <- gsub("\\*.*$", "", after)
  after <- str_squish(after)
  after <- after[after != ""]
  sl <- which(str_detect(after, "^/"))
  if (length(sl)) after <- after[seq(sl[1], length(after))]
  blob <- paste(after, collapse = " ")
  m <- str_match_all(blob, "(?i)\\b([A-Za-z0-9_]+)\\s+(\\d+)\\s*-\\s*(\\d+)\\s*(\\([aA]\\))?")[[1]]
  if (!nrow(m)) return(tibble(var = character(), start = integer(), end = integer(), width = integer(), type = character()))
  tibble(var = m[, 2], start = as.integer(m[, 3]), end = as.integer(m[, 4]),
         type = ifelse(!is.na(m[, 5]) & m[, 5] != "", "character", "numeric")) |>
    mutate(width = end - start + 1L) |> distinct(var, .keep_all = TRUE) |> arrange(start)
}

# --- Check 2001 wave against codebook ---
cat("=== Verifying 2001 raw data against codebook ===\n\n")

dict <- parse_sps_positions(file.path(DATA_DIR, "FAM2001ER.sps"))

# Read only the donation columns we care about
er_codes <- c("ER20047", "ER20059", "ER20065", "ER20071")
labels <- c("Religious", "Needy", "Health", "Education")

# Codebook expected values for 2001 (from screenshots)
expected <- tribble(
  ~er_code,   ~label,       ~N,   ~mean,    ~max,
  "ER20047",  "Religious",  3069, 1675.29,  67000,
  "ER20059",  "Needy",      1744, 418.57,   24500,
  "ER20065",  "Health",     1221, 691.21,   490000,
  "ER20071",  "Education",  901,  416.22,   50000
)

keep <- dict |> filter(var %in% er_codes)
cat("Column positions in .sps:\n")
print(keep)
cat("\n")

# Read just those columns from the raw file
df <- readr::read_fwf(
  file.path(DATA_DIR, "FAM2001ER.txt"),
  col_positions = readr::fwf_positions(keep$start, keep$end, keep$var),
  col_types = cols(.default = col_double()),
  progress = FALSE
)

cat("Total rows in raw file:", nrow(df), "\n\n")

for (i in seq_along(er_codes)) {
  ec <- er_codes[i]
  lb <- labels[i]
  vals <- df[[ec]]
  
  # Actual amount values (1 - 99,999,996)
  actual <- vals[vals >= 1 & vals <= 99999996]
  n_actual <- length(actual)
  mean_actual <- mean(actual)
  max_actual <- max(actual)
  
  # Compare to codebook
  exp_row <- expected |> filter(er_code == ec)
  
  cat(glue("{lb} ({ec}):\n"))
  cat(glue("  Raw N (1-99999996): {n_actual}  | Codebook N: {exp_row$N}\n"))
  cat(glue("  Raw Mean: {round(mean_actual, 2)}  | Codebook Mean: {exp_row$mean}\n"))
  cat(glue("  Raw Max:  {max_actual}  | Codebook Max: {exp_row$max}\n"))
  
  # Check zeros and sentinels
  n_zero <- sum(vals == 0, na.rm = TRUE)
  n_dk <- sum(vals == 99999998, na.rm = TRUE)
  n_ref <- sum(vals == 99999999, na.rm = TRUE)
  n_na <- sum(is.na(vals))
  cat(glue("  Zeros: {n_zero} | DK: {n_dk} | Refused: {n_ref} | NA: {n_na}\n"))
  
  match_ok <- n_actual == exp_row$N
  cat(glue("  MATCH: {ifelse(match_ok, 'YES', 'NO')}\n\n"))
}

# --- Now check what ends up in the parquet ---
cat("\n=== Comparing raw vs parquet for 2001 ===\n\n")
library(arrow)
panel <- read_parquet("output/psid_family_year_2001_latest.parquet")
p2001 <- panel |> filter(wave == 2001)

cat("Parquet rows for 2001:", nrow(p2001), "\n")
cat("Raw file rows:", nrow(df), "\n")
cat("Difference:", nrow(df) - nrow(p2001), "rows lost\n\n")

# Check donation values in parquet
for (i in seq_along(labels)) {
  col_name <- paste("DOLLAR AMT OF", toupper(labels[i]), "DONATIONS")
  if (col_name %in% names(p2001)) {
    vals <- p2001[[col_name]]
    actual <- vals[!is.na(vals) & vals >= 1 & vals < 999997]
    cat(glue("{col_name}:\n"))
    cat(glue("  Parquet N (valid >0): {length(actual)}\n"))
    cat(glue("  Parquet Mean: {round(mean(actual), 2)}\n\n"))
  }
}

