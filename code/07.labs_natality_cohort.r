#===============================================================================
# Script 07: Merge labs with natality cohort
#
# Merge the linked files with the lab files
# MKD
# March 29 2024
#===============================================================================

# Install and load packages
if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)}
p_load(data.table, collapse, lubridate, here)
rm(list = ls())

# Read in the demographic file
cohort <- fread(here("data", "outputs", "linked_infant_mom_demographics.csv"))

# Read in the lab files (filtered, all years)
labs <- fread(here("data", "outputs", "all_labs_opioids.csv"))

# Create a flag for positive opioids lab
labs[, positive_opioids := ifelse(POSITIVE == 1 & opioids == 1, 1, 0)]

# Create a flag for negative opioids lab
labs[, negative_opioids := ifelse(NEGATIVE == 1 & opioids == 1, 1, 0)]

# Create a flag for unknown result opioids lab
labs[, unknown_res_opioids := ifelse(unknown_result == 1 & opioids == 1, 1, 0)]

# Collapse the labs file
collapse_labs <- labs[, .(total_tests = .N,
             total_pos = sum(POSITIVE),
             total_neg = sum(NEGATIVE),
             total_no_result = sum(unknown_result),
             any_opioids = sum(opioids),
             total_positive_opioids = sum(positive_opioids),
             total_negative_opioids = sum(negative_opioids),
             total_unknown_opioids = sum(unknown_res_opioids),
             total_meconium = sum(meconium),
             total_umbilical = sum(umbilical),
             date_first_test = min(LAB_DATE_clean),
             date_last_test = max(LAB_DATE_clean)),
        by = STUDY_ID]
nrow(collapse_labs)

labs_cohort <- merge(cohort, collapse_labs, by.x = "BABY_STUDY_ID", by.y = "STUDY_ID", all.x = TRUE)
labs_cohort[!is.na(total_tests), .N]

# Create a flag for no labs
labs_cohort[, F_NO_LABS := ifelse(is.na(total_tests), 1, 0)]

# Create a flag for positive labs
labs_cohort[, F_POS_LABS := ifelse(total_pos > 0, 1, 0)]

# Create a flag for only negative labs
labs_cohort[, F_NEG_LABS := ifelse(total_neg > 0 & total_pos == 0, 1, 0)]

# Create a flag for no result labs
labs_cohort[, F_NO_RESULT_LABS := ifelse(total_no_result > 0 & total_pos == 0 & total_neg == 0, 1, 0)]

# Save the file
fwrite(labs_cohort, here("data", "outputs", "cohort_with_lab.csv"))
