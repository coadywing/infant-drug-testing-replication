#===============================================================================
# Script 33: Consolidate maternal drug testing records
#
# Create a single file of drug testing for moms in the sample
# MKD
# December 4, 2024
#===============================================================================

# Packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, lubridate, here)
rm(list = ls())

# Input directory with year-specific lab files
input_lab_dir <- here("data", "outputs")

# Read in the 2019-2023 lab files for moms from Script 32
lab_2019 <- fread(file.path(input_lab_dir, "lab_Moms_2019.csv"))
lab_2020 <- fread(file.path(input_lab_dir, "lab_Moms_2020.csv"))
lab_2021 <- fread(file.path(input_lab_dir, "lab_Moms_2021.csv"))
lab_2022 <- fread(file.path(input_lab_dir, "lab_Moms_2022.csv"))
lab_2023 <- fread(file.path(input_lab_dir, "lab_Moms_2023.csv"))

# Read in the analytic sample to get list of Mom STUDYIDs from Script 15
dt <- fread(here("data", "outputs", "final_analytic_sample_with_infant_char_for_reg.csv"))

# Rbind the lab files
lab_moms <- rbind(lab_2019, lab_2020, lab_2021, lab_2022, lab_2023)

# Limit the lab_moms to just STUDY_IDs of moms in the sample
lab_moms_limited <- lab_moms[STUDY_ID %in% dt$STUDY_ID]

# Create a flag for positive test results
    # First put LAB_RESULT to all lower case
    lab_moms_limited[, LAB_RESULT := tolower(LAB_RESULT)]
    lab_moms_limited[, NORMAL_RANGE := tolower(NORMAL_RANGE)]
    # Create a list of positive test result values
    pos <- c("positive", "pos", "present", "positive result.", "positive (qualifier value)", "detected", "present","pos-unconf")
    lab_moms_limited[, positive_test := ifelse(LAB_RESULT %in% pos, 1, 0)]

# Collapse to be a single row per test date per person with a variable for the number of tests and then a flag for any positive tests
lab_moms_collapse <- lab_moms_limited[, .(positive_test = max(positive_test), total_tests = .N), by = .(STUDY_ID, LAB_DATE)]

# Write out the lab_moms_collapse file
fwrite(lab_moms_collapse, here("data", "outputs", "lab_moms_collapse.csv"))
