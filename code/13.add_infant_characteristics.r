#===============================================================================
# Script 13: Add birthweight, prematurity, NICU encounters
#
# Merge on infant characteristics -- low birthweight, NICU visits
# MKD
# June 20 2024
#===============================================================================

# Install packages
if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)}
p_load(data.table, lubridate, collapse, here)
rm(list=ls())
source(here("code", "00_paths.r"))

# External data paths
input_encounters <- file.path(path_inpc_csv_dir, "encounter.sas7bdat.csv")
input_diagnosis <- file.path(path_inpc_csv_dir, "dx.sas7bdat.csv")

# Read in the infant cohort
cohort <- fread(here("data", "outputs", "sample_with_maternal_characteristics.csv"))

# Read in the encounters data (full INPC)
enc <- fread(input_encounters)

  # Limit to infant's encounters
  baby_enc <- enc[STUDY_ID %in% cohort$BABY_STUDY_ID]

# Read in the dx data
dx <- fread(input_diagnosis)

  # Limit to infant's dx data
  baby_dx <- dx[STUDY_ID %in% cohort$BABY_STUDY_ID]

# Merge the encounters and dx data
dx_enc <- merge(baby_dx, baby_enc, by = c("STUDY_ID", "ENCOUNTER_ID_DE"), all = TRUE)

# Clean the dx code
dx_enc[, DX_CLEAN := gsub("\\.", "", DX_CODE)]

# Flag for low birthweight by icd 10 code
lbw <- c("P070", "P071")
lbw2 <- c("P0500", "P0501", "P0502", "P0503", "P0504", "P0505", "P0506", "P0507", "P0508",
            "P0510", "P0511", "P0512", "P0513", "P0514", "P0515", "P0516", "P0517", "P0518", "P0519")

# Flag for prematurity
premature <- c("P072", "P073")

# Flag in the dx_enc data for lbw and premature
dx_enc[, lbw := ifelse(substr(DX_CLEAN, 1, 4) %in% lbw | substr(DX_CLEAN, 1, 5) %in% lbw2, 1, 0)]
dx_enc[, premature := ifelse(substr(DX_CLEAN, 1, 4) %in% premature, 1, 0)]

# Create summary of infant characteristics (collapse to 1 row per infant)
dx_enc_summary <- dx_enc[, .(lbw = max(lbw, na.rm = TRUE), premature = max(premature, na.rm = TRUE)), by = STUDY_ID]

# Merge back onto the cohort
cohort <- merge(cohort, dx_enc_summary, by.x = "BABY_STUDY_ID", by.y = "STUDY_ID", all.x = TRUE)

# Replace NAs with 0
cohort[, lbw := fifelse(is.na(lbw), 0, lbw)]
cohort[, premature := fifelse(is.na(premature), 0, premature)]

# Merge on the date of birth from the cohort to baby_enc
dob <- cohort[, .(BABY_STUDY_ID, DOB)]
dob_enc <- merge(baby_enc, dob, by.x = "STUDY_ID", by.y = "BABY_STUDY_ID", all.x = TRUE)

# Limit to just 1 row per encounter_ID
dob_enc <- dob_enc[, .SD[1], by = ENCOUNTER_ID_DE]

# Create a DOB + 1 month variable
dob_enc[, DOB_1month := as.Date(DOB) + months(1)]

# Flag the encounters within 1 month of the DOB
dob_enc[, encounters_within_1month := ifelse(ADMIT_TIME >= DOB & ADMIT_TIME <= DOB_1month, 1, 0)]
dob_enc[is.na(encounters_within_1month), encounters_within_1month := ifelse(DISCHARGE_TIME >= DOB & DISCHARGE_TIME <= DOB_1month, 1, 0)]

# Collapse to be 1 row per study ID with the number of encounters within 1 month of the DOB
dob_enc_summary <- dob_enc[, .(encounters_within_1month = sum(encounters_within_1month)), by = STUDY_ID]
dob_enc_summary[is.na(encounters_within_1month), encounters_within_1month := 0] # Set NAs to 0
dob_enc_summary[, .N, by = encounters_within_1month][order(encounters_within_1month)]

# Merge the number of encounters within 1 month of the DOB back onto the cohort
cohort <- merge(cohort, dob_enc_summary, by.x = "BABY_STUDY_ID", by.y = "STUDY_ID", all.x = TRUE)

# Save the cohort
fwrite(cohort, here("data", "outputs", "sample_with_maternal_infant_characteristics.csv"))