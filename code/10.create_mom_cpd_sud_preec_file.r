#===============================================================================
# Script 10: Extract maternal diagnoses (SUD, CPD, hypertension, etc.)
#
# Initial exploration of the utilization of care by moms
# MKD
# April 2 2024
#===============================================================================

# Install and load packages.
if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)}
p_load(data.table, lubridate, collapse, here)
rm(list=ls())
source(here("code", "00_paths.r"))

# External data paths
input_encounters <- file.path(path_inpc_csv_dir, "encounter.sas7bdat.csv")
input_diagnosis <- file.path(path_inpc_csv_dir, "dx.sas7bdat.csv")

# Read in the linked cohort data
cohort <- fread(here("data", "outputs", "labs_natl_cohort_mort.csv"))

cohort[, DELIVERY_DATE := as.Date(DELIVERY_DATE)]
mom_births <- cohort[, .(BABY_STUDY_ID, MOM_STUDY_ID, DELIVERY_DATE)][order(MOM_STUDY_ID, DELIVERY_DATE)]
mom_births[, birth_number := seq_len(.N), by = MOM_STUDY_ID]

# Read in the encounters data
enc <- fread(input_encounters)

  # Limit to mom's encounters
  mom_enc <- enc[STUDY_ID %in% mom_births$MOM_STUDY_ID]

# Read in the dx data
dx <- fread(input_diagnosis)

  # Limit to mom's dx data
  mom_dx <- dx[STUDY_ID %in% mom_births$MOM_STUDY_ID]

# Merge the encounters and dx data
dx_enc <- merge(mom_enc, mom_dx, by = c("STUDY_ID", "ENCOUNTER_ID_DE"), all = TRUE)

# Clean the diagnosis codes
dx_enc[, DX_CLEAN := gsub("\\.", "", DX_CODE)]

# Define diagnosis code sets with their column names and character widths
diagnosis_conditions <- list(
  SUD = list(codes = c("F10", "F11", "F12", "F13", "F14", "F15", "F16", "F17", "F18", "F19"), width = 3),
  CPD = list(codes = c("G89"), width = 3),
  HYPT = list(codes = c("O10", "O11", "O13", "O14", "O15", "O16"), width = 3),  # Hypertensive disorders of pregnancy
  GEST_DIAB = list(codes = c("O244"), width = 4),
  MOOD = list(codes = c("F30", "F31", "F32", "F33", "F34", "F39"), width = 3),
  ANXIETY = list(codes = c("F40", "F41", "F42", "F43", "F44", "F45", "F48"), width = 3),
  SCHIZ = list(codes = c("F20", "F21", "F22", "F23", "F24", "F25", "F28", "F29"), width = 3),
  PRENATAL_CARE = list(codes = c("Z34"), width = 3)
)

# Create diagnosis flags
for (condition in names(diagnosis_conditions)) {
  codes <- diagnosis_conditions[[condition]]$codes
  width <- diagnosis_conditions[[condition]]$width
  dx_enc[, (condition) := ifelse(substr(DX_CLEAN, 1, width) %in% codes, 1, 0)]
}

# Combine mental health indicators
dx_enc[, MENTAL_HEALTH := ifelse(MOOD == 1 | ANXIETY == 1 | SCHIZ == 1, 1, 0)]

# Summarize
dx_enc[, .(n = .N,
            n_sud = sum(SUD),
            n_cpd = sum(CPD),
            n_hypt = sum(HYPT),
            n_gest_diab = sum(GEST_DIAB),
            n_mood = sum(MOOD),
            n_anxiety = sum(ANXIETY),
            n_schiz = sum(SCHIZ),
            n_mental_health = sum(MENTAL_HEALTH),
            n_prenatal_care = sum(PRENATAL_CARE))]

# Merge the dx_enc file with the mom births file
mom_dx_enc <- merge(dx_enc, mom_births, by.x = "STUDY_ID", by.y = "MOM_STUDY_ID", allow.cartesian = TRUE)

# Create a flag for whether the encounter was before or after the delivery date
mom_dx_enc[, before_deliv := as.numeric(ADMIT_TIME <= DELIVERY_DATE)]
mom_dx_enc[is.na(before_deliv)] # if NA, means ADMIT_TIME missing. Recode to be based on DISCHARGE_TIME for NAs
mom_dx_enc[is.na(before_deliv), before_deliv := as.numeric(DISCHARGE_TIME <= DELIVERY_DATE)]
mom_dx_enc[is.na(before_deliv)] # No NAs remaining

# Create flags for encounters before delivery for each diagnosis type
conditions_to_flag <- c("SUD", "CPD", "HYPT", "GEST_DIAB", "MOOD", "ANXIETY", "SCHIZ", "MENTAL_HEALTH", "PRENATAL_CARE")
for (condition in conditions_to_flag) {
  new_col_name <- paste0("before_", tolower(condition))
  mom_dx_enc[, (new_col_name) := ifelse(before_deliv == 1 & get(condition) == 1, 1, 0)]
}

# Now, for each DELIVERY_DATE, MOM_STUDY_ID, BABY_STUDY_ID, and birth_number,
# calculate the number of encounters before delivery for SUD, CPD, and preeclampsia. Collapse the data to one row per group.
mom_dx_enc_collapse <- mom_dx_enc[, .(n_before_sud = sum(before_sud),
                                      n_before_cpd = sum(before_cpd),
                                      n_before_hypt = sum(before_hypt),
                                      n_before_gest_diab = sum(before_gest_diab),
                                      n_before_mood = sum(before_mood),
                                      n_before_anxiety = sum(before_anxiety),
                                      n_before_schiz = sum(before_schiz),
                                      n_before_mental_health = sum(before_mental_health),
                                      n_before_prenatal_care = sum(before_prenatal_care)
                                      ),
                                      by = .(DELIVERY_DATE, STUDY_ID, BABY_STUDY_ID, birth_number)]

# Save the data
fwrite(mom_dx_enc_collapse, here("data", "outputs", "mom_dx_enc_collapse.csv"))
