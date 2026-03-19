################################################################################
# Load PSID Cross-Year Individual File (J359022)
################################################################################
# This file contains permanent person identifiers (ER30001 + ER30002) and
# relationship to head for all waves. These are the stable IDs needed to
# track individuals across waves.
################################################################################

library(tidyverse)
library(arrow)

cat("\n")
cat(strrep("=", 80), "\n", sep="")
cat("LOADING PSID CROSS-YEAR INDIVIDUAL FILE\n")
cat(strrep("=", 80), "\n\n", sep="")

# Define column positions from J359022.sas INPUT statement (lines 153-198)
# Based on the SAS file, we need these key variables for 2001-2021

col_positions <- fwf_positions(
  start = c(
    1,    # ER30000 - Release number
    2,    # ER30001 - 1968 Interview Number (permanent ID part 1)
    6,    # ER30002 - Person Number (permanent ID part 2)
    9,    # ER32000 - Sex
    10,   # ER32006 - Sample/nonsample
    # 2001 wave (ER33201, ER33202, ER33203)
    209, 214, 216,
    # 2003 wave (ER33301, ER33302, ER33303)
    218, 222, 224,
    # 2005 wave (ER33401, ER33402, ER33403)
    226, 231, 233,
    # 2007 wave (ER33501, ER33502, ER33503)
    235, 240, 242,
    # 2009 wave (ER33601, ER33602, ER33603)
    244, 248, 250,
    # 2011 wave (ER33701, ER33702, ER33703)
    252, 257, 259,
    # 2013 wave (ER34201, ER34202, ER34203)
    297, 302, 304,
    # 2015 wave (ER34301, ER34302, ER34303)
    306, 311, 313,
    # 2017 wave (ER34501, ER34502, ER34503)
    315, 320, 322,
    # 2019 wave (ER34701, ER34702, ER34703)
    324, 329, 331,
    # 2021 wave (ER34901, ER34902, ER34903)
    333, 338, 340
  ),
  end = c(
    1,    # ER30000
    5,    # ER30001
    8,    # ER30002
    9,    # ER32000
    10,   # ER32006
    # 2001
    213, 215, 217,
    # 2003
    221, 223, 225,
    # 2005
    230, 232, 234,
    # 2007
    239, 241, 243,
    # 2011
    247, 249, 251,
    # 2011
    256, 258, 260,
    # 2013
    301, 303, 305,
    # 2015
    310, 312, 314,
    # 2017
    319, 321, 323,
    # 2019
    328, 330, 332,
    # 2021
    337, 339, 341
  ),
  col_names = c(
    "release_number", "id_1968", "person_number", "sex", "sample_nonsample",
    # 2001
    "interview_num_2001", "seq_num_2001", "rel_to_head_2001",
    # 2003
    "interview_num_2003", "seq_num_2003", "rel_to_head_2003",
    # 2005
    "interview_num_2005", "seq_num_2005", "rel_to_head_2005",
    # 2007
    "interview_num_2007", "seq_num_2007", "rel_to_head_2007",
    # 2009
    "interview_num_2009", "seq_num_2009", "rel_to_head_2009",
    # 2011
    "interview_num_2011", "seq_num_2011", "rel_to_head_2011",
    # 2013
    "interview_num_2013", "seq_num_2013", "rel_to_head_2013",
    # 2015
    "interview_num_2015", "seq_num_2015", "rel_to_head_2015",
    # 2017
    "interview_num_2017", "seq_num_2017", "rel_to_head_2017",
    # 2019
    "interview_num_2019", "seq_num_2019", "rel_to_head_2019",
    # 2021
    "interview_num_2021", "seq_num_2021", "rel_to_head_2021"
  )
)

cat("Reading J359022.txt (85,536 individuals)...\n")
individual_data <- read_fwf(
  "J359022.txt",
  col_positions = col_positions,
  col_types = cols(.default = col_integer()),
  progress = TRUE
)

cat("\n✓ Data loaded successfully!\n")
cat("  Dimensions:", nrow(individual_data), "rows x", ncol(individual_data), "columns\n\n")

# Create unique person ID
individual_data <- individual_data %>%
  mutate(person_id = paste0(id_1968, "_", person_number))

# Display summary
cat("First 10 rows:\n")
print(head(individual_data, 10))

cat("\nUnique persons:", n_distinct(individual_data$person_id), "\n")
cat("Sex distribution:\n")
print(table(individual_data$sex, useNA = "ifany"))

# Save as parquet
cat("\nSaving to output/individual_data.parquet...\n")
write_parquet(individual_data, "output/individual_data.parquet")

cat("\n")
cat(strrep("=", 80), "\n", sep="")
cat("✓ INDIVIDUAL FILE LOADED AND SAVED!\n")
cat(strrep("=", 80), "\n\n", sep="")

