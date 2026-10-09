#===============================================================================
# Script 04: Filter lab files by meconium/umbilical LOINC codes
#
# Read in and then restrict the lab files to be only for the codes we want
# MKD
# January 15, 2024
# Updated February 8, 2024 to incorporate the umbilical cord LOINC codes
#===============================================================================

# Install and load packages
if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)}
p_load(data.table, ggplot2, lubridate, stringr, vroom, glue, here)
rm(list=ls())
source(here("code", "00_paths.r"))

# The LOINC codes are available in the data/inputs folder
# Read in the LOINC codes
loinc_meconium <- fread(here("data", "inputs", "Loinc_2.76_Export_20231116174148_Meconium.csv"))
loinc_umbilical <- fread(here("data", "inputs", "umbilical_cord_loinc.csv"))

# Clean up the LOINC codes - remove the dash
loinc_meconium[, loinc_clean := gsub("-", "", LOINC_NUM)]
loinc_umbilical[, loinc_clean := gsub("-", "", LOINC)]

# Append 0s to the LOINC codes to make them 10 digits long
loinc_meconium[, loinc_clean := str_pad(loinc_clean, width = 10, pad = "0")]
loinc_umbilical[, loinc_clean := str_pad(loinc_clean, width = 10, pad = "0")]

# Drop columns except for the LOINC codes and names
loinc_meconium <- loinc_meconium[, c("loinc_clean", "LONG_COMMON_NAME")]
loinc_umbilical <- loinc_umbilical[, c("loinc_clean", "Component Chart Name")]
setnames(loinc_umbilical, c("loinc_clean", "LONG_COMMON_NAME"))

# Create a variable for umbilical (dummy) and meconium (dummy)
loinc_meconium[, meconium := 1]
loinc_meconium[, umbilical := 0]
loinc_umbilical[, umbilical := 1]
loinc_umbilical[, meconium := 0]

# Merge the two loinc data.tables
loinc <- rbind(loinc_meconium, loinc_umbilical)

# Write out the loinc file
fwrite(loinc, here("data", "outputs", "loinc_umbilical_meconium_clean.csv"))

# Process lab files from 2016-2023 --------------------------------
years <- seq(2016, 2023)

# Files 
input_lab_dir <- path_inpc_csv_dir
lab_files_path <- list.files(input_lab_dir, pattern = "lab_[0-9]{4}.sas7bdat.csv", full.names = TRUE)

for (year in years) {
  # Read in lab file using vroom
  lab_file <- vroom(glue::glue("{input_lab_dir}/lab_{year}.sas7bdat.csv"))
  setDT(lab_file)

  # Clean up the lab file - remove the dash and pad LOINC codes to 10 digits
  lab_file[, LOINC_clean := gsub("-", "", LOINC)]
  lab_file[, LOINC_clean := str_pad(LOINC_clean, width = 10, pad = "0")]

  # Filter lab file based on LOINC codes in the loinc data.table
  filtered_lab <- merge(lab_file, loinc, by.x = "LOINC_clean", by.y = "loinc_clean")

  # Print counts by meconium status
  cat("Year", year, "- Counts by meconium:\n")
  print(filtered_lab[, .N, by = "meconium"])

  # Write out the filtered lab file
  fwrite(filtered_lab, here("data", "outputs", glue("lab_{year}_filtered.csv")))
} 
