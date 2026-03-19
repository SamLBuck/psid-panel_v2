# Rebuild panel dataset with religiosity variable
# This extracts the key parts from RStudio_Git_Setup.Rmd

cat("REBUILDING PANEL WITH RELIGIOSITY\\n")
cat(rep("=", 70), "\\n\\n", sep = "")

# Just run the Rmd file chunks in order
cat("Loading the Rmd file and executing chunks...\\n\\n")

# Read the Rmd file
rmd_content <- readLines("RStudio_Git_Setup.Rmd")

# Extract code from chunks
in_chunk <- FALSE
chunk_code <- character()
chunk_name <- ""

for (line in rmd_content) {
  # Check if starting a chunk
  if (grepl("^```\\{r", line)) {
    in_chunk <- TRUE
    chunk_name <- sub("^```\\{r\\s*(\\w*).*", "\\1", line)
    if (chunk_name == "") chunk_name <- "unnamed"
    next
  }
  
  # Check if ending a chunk
  if (grepl("^```\\s*$", line) && in_chunk) {
    if (length(chunk_code) > 0) {
      cat("\\nExecuting chunk:", chunk_name, "\\n")
      cat(rep("-", 50), "\\n", sep = "")
      
      # Execute the chunk code
      tryCatch({
        eval(parse(text = chunk_code))
      }, error = function(e) {
        cat("Warning in chunk", chunk_name, ":", conditionMessage(e), "\\n")
      })
    }
    in_chunk <- FALSE
    chunk_code <- character()
    next
  }
  
  # Collect code lines
  if (in_chunk) {
    chunk_code <- c(chunk_code, line)
  }
}

cat("\\n", rep("=", 70), "\\n", sep = "")
cat("REBUILD COMPLETE!\\n")
cat(rep("=", 70), "\\n\\n", sep = "")

# Verify the result
library(arrow)
library(here)
panel <- read_parquet(here("output", "psid_family_year_2001_latest.parquet"))

cat("Dataset summary:\\n")
cat("  Rows:", nrow(panel), "\\n")
cat("  Families:", length(unique(panel$family_id)), "\\n")
cat("  Waves:", paste(sort(unique(panel$wave)), collapse = ", "), "\\n\\n")

if ("Religious_Preference" %in% names(panel)) {
  cat("✓ Religious_Preference successfully added!\\n")
  cat("\\nReligious Preference distribution:\\n")
  print(table(panel$Religious_Preference, useNA = "always"))
} else {
  cat("✗ Religious_Preference NOT found\\n")
}

