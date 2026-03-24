# Extract and run all R chunks from build_panel.Rmd
lines <- readLines("build_panel.Rmd")
in_chunk <- FALSE
code <- character()
for (line in lines) {
  if (grepl("^```\\{r", line)) {
    in_chunk <- TRUE
    next
  }
  if (grepl("^```$", line) && in_chunk) {
    in_chunk <- FALSE
    next
  }
  if (in_chunk) code <- c(code, line)
}
# Write to temp file and source it
tmp <- tempfile(fileext = ".R")
writeLines(code, tmp)
source(tmp, echo = TRUE)

