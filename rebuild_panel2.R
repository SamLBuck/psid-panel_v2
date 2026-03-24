# Direct extraction and execution of build_panel.Rmd chunks
cat("Starting rebuild...\n")

suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(arrow)
  library(glue)
  library(stringr)
  library(fs)
  library(here)
})

DATA_DIR  <- here("data_raw")
XWALK_DIR <- here("xwalk")
OUT_PATH  <- here("output", "psid_family_year_2001_latest.parquet")
WAVES     <- c(2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015,
               2017, 2019, 2021)

dir_create(path_dir(OUT_PATH))

# ---- Parse SPSS .sps dictionary for column positions ----
parse_sps_positions <- function(dict_path) {
  lines <- read_lines(dict_path)
  dl <- which(str_detect(lines, regex("^\\s*DATA\\s+LIST", ignore_case = TRUE)))
  if (!length(dl))
    return(tibble(var = character(), start = integer(),
                  end = integer(), width = integer(),
                  type = character()))
  after <- lines[seq(dl[1], length(lines))]
  stop_idx <- which(str_detect(
    after, regex("^\\s*VARIABLE\\s+LABELS", ignore_case = TRUE)
  ))
  if (length(stop_idx)) after <- after[seq(1, stop_idx[1] - 1)]
  after <- gsub("\\t", " ", after)
  after <- gsub("\\*.*$", "", after)
  after <- str_squish(after)
  after <- after[after != ""]
  sl <- which(str_detect(after, "^/"))
  if (length(sl)) after <- after[seq(sl[1], length(after))]
  blob <- paste(after, collapse = " ")
  m <- str_match_all(
    blob,
    "(?i)\\b([A-Za-z0-9_]+)\\s+(\\d+)\\s*-\\s*(\\d+)\\s*(\\([aA]\\))?"
  )[[1]]
  if (!nrow(m))
    return(tibble(var = character(), start = integer(),
                  end = integer(), width = integer(),
                  type = character()))
  tibble(
    var   = m[, 2],
    start = as.integer(m[, 3]),
    end   = as.integer(m[, 4]),
    type  = ifelse(!is.na(m[, 5]) & m[, 5] != "",
                   "character", "numeric")
  ) |>
    mutate(width = end - start + 1L) |>
    distinct(var, .keep_all = TRUE) |>
    arrange(start)
}

read_psid_fwf <- function(stem) {
  txt  <- path(DATA_DIR, paste0(stem, ".txt"))
  sps  <- path(DATA_DIR, paste0(stem, ".sps"))
  if (!file_exists(txt)) { warning(glue("Missing {stem}.txt")); return(tibble()) }
  if (!file_exists(sps)) { warning(glue("Missing {stem}.sps")); return(tibble()) }
  dict <- parse_sps_positions(sps)
  if (!nrow(dict)) { warning(glue("No positions in {stem}.sps")); return(tibble()) }
  col_types <- paste(ifelse(dict$type == "character", "c", "d"),
                     collapse = "")
  readr::read_fwf(
    file          = txt,
    col_positions = readr::fwf_positions(dict$start, dict$end, dict$var),
    col_types     = col_types,
    na            = c("", " ", ".", "NA")
  )
}

# ---- Load crosswalks ----
load_xwalk <- function(fname) {
  p <- path(XWALK_DIR, fname)
  if (!file_exists(p)) { warning(glue("Crosswalk not found: {fname}")); return(tibble(wave = integer(), varname = character(), source = character())) }
  out <- read_csv(p, show_col_types = FALSE)
  names(out) <- tolower(trimws(names(out)))
  out <- out |> select(any_of(c("wave", "varname", "source")))
  needed <- c("wave", "varname", "source")
  if (!all(needed %in% names(out))) { warning(glue("{fname}: missing columns")); return(tibble(wave = integer(), varname = character(), source = character())) }
  out <- out |> mutate(wave = as.integer(wave)) |> filter(!is.na(source), source != "")
  out <- out |> distinct(wave, varname, .keep_all = TRUE)
  out <- out |> distinct(wave, source, .keep_all = TRUE)
  out
}

family_xw <- load_xwalk("family_vars_crosswalk.csv")
hs_xw     <- load_xwalk("head_spouse_crosswalk.csv")
phil_xw   <- load_xwalk("philanthropy_crosswalk.csv")
mob_xw    <- load_xwalk("mobility_crosswalk.csv")
state_xw  <- load_xwalk("state_crosswalk.csv")
relig_xw  <- load_xwalk("religiosity_crosswalk.csv")
cat("Crosswalks loaded.\n")
cat("family_xw rows:", nrow(family_xw), "\n")
cat("family_weight entries:", sum(family_xw$varname == "family_weight"), "\n\n")

# ---- apply_xwalk ----
apply_xwalk <- function(df, xw, yr, label = "") {
  if (!nrow(df) || !nrow(xw)) return(tibble())
  xwy <- xw |> filter(wave == yr)
  if (!nrow(xwy)) return(tibble())
  found   <- xwy |> filter(source %in% names(df))
  missing <- xwy |> filter(!source %in% names(df))
  if (nrow(missing) && nchar(label))
    cat(glue("  {label} wave {yr}: missing {nrow(missing)} vars: ",
             "{paste(missing$source, collapse=', ')}\n\n"))
  if (!nrow(found)) return(tibble())
  out <- select(df, all_of(found$source))
  names(out) <- found$varname
  out
}

# ---- build_wave ----
build_wave <- function(yr) {
  cat(glue("=== Wave {yr} ===\n"))
  fam <- read_psid_fwf(glue("FAM{yr}ER"))
  if (!nrow(fam)) return(tibble())

  id68_src <- family_xw |> filter(wave == yr, varname == "id_1968") |> pull(source) |> first()
  fid_src  <- family_xw |> filter(wave == yr, varname == "family_id") |> pull(source) |> first()
  iyr_src  <- family_xw |> filter(wave == yr, varname == "interview_year") |> pull(source) |> first()
  wgt_src  <- family_xw |> filter(wave == yr, varname == "family_weight") |> pull(source) |> first()

  if (is.na(id68_src) || !id68_src %in% names(fam)) stop(glue("Wave {yr}: no id_1968 mapping"))
  if (is.na(fid_src)  || !fid_src  %in% names(fam)) stop(glue("Wave {yr}: no family_id mapping"))
  if (is.na(iyr_src)  || !iyr_src  %in% names(fam)) stop(glue("Wave {yr}: no interview_year mapping"))
  if (is.na(wgt_src)  || !wgt_src  %in% names(fam))
    warning(glue("Wave {yr}: no family_weight mapping"))

  keys <- tibble(
    id_1968        = as.integer(fam[[id68_src]]),
    family_id      = as.integer(fam[[fid_src]]),
    interview_year = as.integer(fam[[iyr_src]]),
    wave           = as.integer(yr),
    family_weight  = if (!is.na(wgt_src) && wgt_src %in% names(fam))
                       fam[[wgt_src]] else NA_real_
  )

  safe_add <- function(base, piece) {
    if (is.null(piece) || !ncol(piece)) return(base)
    dupes <- intersect(names(base), names(piece))
    if (length(dupes)) piece <- piece |> select(-all_of(dupes))
    bind_cols(base, piece)
  }

  out <- keys |>
    safe_add(apply_xwalk(fam, hs_xw,    yr, "head_spouse")) |>
    safe_add(apply_xwalk(fam, phil_xw,  yr, "philanthropy")) |>
    safe_add(apply_xwalk(fam, mob_xw,   yr, "mobility")) |>
    safe_add(apply_xwalk(fam, state_xw, yr, "state")) |>
    safe_add(apply_xwalk(fam, relig_xw, yr, "religiosity"))

  n_dup <- sum(duplicated(out$family_id))
  if (n_dup > 0) stop(glue("Wave {yr}: {n_dup} duplicate family_id rows"))

  cat(glue("  {nrow(out)} families, {ncol(out)} columns\n\n"))
  out
}

# ---- Build panel ----
panel <- map_dfr(WAVES, build_wave)

cat("\n=== PANEL SUMMARY (all families, including splitoffs) ===\n")
cat("Total rows:", nrow(panel), "\n")
cat("Total columns:", ncol(panel), "\n")
cat("Unique id_1968 values:", n_distinct(panel$id_1968), "\n")
cat("Unique family_id:", n_distinct(panel$family_id), "\n")
cat("Waves:", paste(sort(unique(panel$wave)), collapse = ", "), "\n\n")

cat("Family weight summary:\n")
cat("  Non-zero weights:", sum(panel$family_weight > 0, na.rm = TRUE), "\n")
cat("  Zero weights:", sum(panel$family_weight == 0, na.rm = TRUE), "\n")
cat("  NA weights:", sum(is.na(panel$family_weight)), "\n\n")

# ---- Write ----
panel <- panel |>
  filter(!is.na(id_1968), !is.na(wave)) |>
  arrange(id_1968, family_id, wave) |>
  relocate(id_1968, family_id, interview_year, wave, family_weight)

write_parquet(panel, OUT_PATH)
cat(glue("Wrote: {OUT_PATH}\n"))
cat(glue("  {nrow(panel)} rows x {ncol(panel)} columns\n"))
cat(glue("  Waves: {paste(sort(unique(panel$wave)), collapse=', ')}\n"))
cat("\nDone.\n")

