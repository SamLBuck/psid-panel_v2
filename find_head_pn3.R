# Look at the first ~30 variable labels in each wave's .sps
# PSID groups identifying variables at the top of the file
years <- c(2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021)

for (yr in years) {
  sps_file <- file.path("data_raw", paste0("FAM", yr, "ER.sps"))
  lines <- readLines(sps_file, warn = FALSE)

  # Find VARIABLE LABELS section
  vl_start <- grep("^\\s*VARIABLE\\s+LABELS", lines, ignore.case = TRUE)
  if (length(vl_start)) {
    # Show first 30 label lines
    label_lines <- lines[seq(vl_start[1], min(vl_start[1] + 35, length(lines)))]
    cat("\n===", yr, "=== (first 30 labels)\n")
    cat(label_lines, sep = "\n")
  }
}
