#===============================================================================
# Script 32: Filter maternal lab data
#
# Create a subset of the lab data for the drug codes
# MKD
# December 3, 2024
#===============================================================================

# Packages
if(!require("pacman")) install.packages("pacman")
library(pacman)
p_load(data.table, duckdb, DBI, stringr, here)
rm(list = ls())
source(here("code", "00_paths.r"))

# External data paths
input_lab_dir <- path_inpc_csv_dir

# Read in the LOINC codes from Script 31
loinc_drug_codes <- fread(here("data", "outputs", "loinc_drug_codes_moms.csv"))

# Clean up the LOINC codes - remove the dash if present
loinc_drug_codes[, loinc_clean := gsub("-", "", LoincNumber)]

# Append 0s to the LOINC codes to make them 10 digits long
loinc_drug_codes[, loinc_clean := str_pad(loinc_clean, width = 10, pad = "0")]

# Keep only relevant columns (update column names as needed)
loinc_drug_codes <- loinc_drug_codes[, .(loinc_clean, LongCommonName)]

# Create DuckDB connection for efficient filtering (reads and filters without loading full dataset)
con <- dbConnect(duckdb())

# Load LOINC codes into DuckDB
dbWriteTable(con, "loinc_codes", loinc_drug_codes, overwrite = TRUE)

# Process each year's data
for (year in 2019:2023) {
  # Single SQL query: read CSV, clean LOINC codes, and filter all in one operation
  # This avoids loading the full lab file into memory
  query <- paste0("
    SELECT *
    FROM read_csv_auto('", input_lab_dir, "/lab_", year, ".sas7bdat.csv')
    WHERE lpad(regexp_replace(LOINC, '-', ''), 10, '0')
          IN (SELECT loinc_clean FROM loinc_codes)
  ")

  # Execute query and convert result to data.table
  filtered_lab <- as.data.table(dbGetQuery(con, query))

  # Write out the filtered lab file
  fwrite(filtered_lab, here("data", "outputs", paste0("lab_Moms_", year, ".csv")))

  # Remove objects to free memory
  rm(filtered_lab)
}

# Close DuckDB connection
dbDisconnect(con)
