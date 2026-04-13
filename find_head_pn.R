years <- c(2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021)

for (yr in years) {
  sps_file <- file.path("data_raw", paste0("FAM", yr, "ER.sps"))
  if (!file.exists(sps_file)) {
    cat(yr, ": FILE NOT FOUND\n")
    next
  }
  lines <- readLines(sps_file, warn = FALSE)
  # Search for sequence number / person number of head
  hits <- grep("SEQUENCE.*HEAD|PERSON.*HEAD|SEQ.*NO.*HEAD|PN.*HEAD|HEAD.*PERSON|HEAD.*SEQ",
               lines, value = TRUE, ignore.case = TRUE)
  cat("\n===", yr, "===\n")
  if (length(hits)) {
    cat(hits, sep = "\n")
  } else {
    # Try broader search
    hits2 <- grep("SEQUENCE NUM|PERSON NUM", lines, value = TRUE,
                  ignore.case = TRUE)
    cat("(broad search)\n")
    cat(head(hits2, 10), sep = "\n")
  }
}
