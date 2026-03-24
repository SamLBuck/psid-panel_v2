library(arrow)
p <- read_parquet("output/psid_family_year_2001_latest.parquet")
cat("Columns:\n")
cat(paste(names(p), collapse="\n"), "\n\n")
cat("Rows per wave:\n")
print(table(p$wave))
cat("\nHas id_1968:", "id_1968" %in% names(p), "\n")
cat("Has family_id_1968:", "family_id_1968" %in% names(p), "\n")
cat("Has Current State:", "Current State" %in% names(p), "\n")
cat("\nNA counts for Current State:\n")
if ("Current State" %in% names(p)) {
  cat("  Non-NA:", sum(!is.na(p[["Current State"]])), "\n")
  cat("  NA:", sum(is.na(p[["Current State"]])), "\n")
}

