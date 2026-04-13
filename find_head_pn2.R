years <- c(2001, 2005, 2021)

for (yr in years) {
  sps_file <- file.path("data_raw", paste0("FAM", yr, "ER.sps"))
  lines <- readLines(sps_file, warn = FALSE)

  cat("\n===", yr, "===\n")

  # Try various patterns
  patterns <- c("HEAD", "PERSON", "SEQUENCE", "PN ", "REL.*HEAD",
                 "RELATION", "WHO.*HEAD", "HEAD.*ID", "REF.*PERSON")
  for (pat in patterns) {
    hits <- grep(pat, lines, value = TRUE, ignore.case = TRUE)
    if (length(hits) > 0) {
      cat("\n  Pattern:", pat, "(", length(hits), "hits)\n")
      cat(head(hits, 5), sep = "\n")
    }
  }
}
