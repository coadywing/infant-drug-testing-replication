#===============================================================================
# Script 05: Combine year-specific filtered lab files
#
# Combine all of the lab files into one large lab file
# MKD
# January 18 2024
# Updated on February 8, 2024 to incorporate the umbilical cord LOINC codes
#===============================================================================

# Install and load packages
if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)}
p_load(data.table, lubridate, stringr, vroom, here)
rm(list=ls())
gc()

# Input directory with year-specific lab files
input_lab_dir <- here("data", "outputs")

# Read in the lab files that are filtered for our codes
lab_files <- list.files(input_lab_dir, pattern = "lab_[0-9]{4}_filtered.csv", full.names = TRUE)

# Find the column names
colnames(lab_files[1])
colnames(lab_files[2])
colnames(lab_files[3])
colnames(lab_files[4])
colnames(lab_files[5])
colnames(lab_files[6])
colnames(lab_files[7])
colnames(lab_files[8])

# For the lab files that have a "VALUE TYPE" column, we will drop this to match the others
for (i in 1:length(lab_files)) {
  lab_file <- fread(lab_files[i])
  if ("VALUE_TYPE" %in% colnames(lab_file)) {
    lab_file[, VALUE_TYPE := NULL]
  }
  fwrite(lab_file, lab_files[i])
}

# Combine the lab files into one large lab file
labs <- rbindlist(lapply(lab_files, fread))
setDT(labs)

# Fix the lab date and create a clean variable
labs[, LAB_DATE_clean := as.Date(LAB_DATE, format = "%m/%d/%Y")]

# Flag the results that are either positive or negative
labs[, POSITIVE := ifelse(LAB_RESULT %ilike% "positive" | LAB_RESULT %ilike% "present", 1, 0)]
labs[, NEGATIVE := ifelse(LAB_RESULT %ilike% "negative" | LAB_RESULT %ilike% "not detected" | LAB_RESULT %ilike% "not present", 1, 0)]
labs[, unknown_result := ifelse(POSITIVE == 0 & NEGATIVE == 0, 1, 0)]

# Write out the labs file
fwrite(labs, here("data", "outputs", "all_labs.csv"))
