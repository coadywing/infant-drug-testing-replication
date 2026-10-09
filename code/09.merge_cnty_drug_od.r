#===============================================================================
# Script 09: Merge county drug overdose mortality
#
# Merge on drug OD data
# MKD
# April 16 2024
#===============================================================================

# Packages
if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)}
p_load(data.table, lubridate, collapse, dplyr, here)
rm(list=ls())

# Read in the analytic sample
cohort <- fread(here("data", "outputs", "labs_natl_cohort.csv"))

# Store county codes as character
cohort[, BABY_COUNTY := as.character(BABY_COUNTY)]
cohort[, MOM_COUNTY := as.character(MOM_COUNTY)]

# Check to see if the child and mother ZIP are the same
cohort[, same_zip := ifelse(BABY_ZIP == MOM_ZIP, 1, 0)]

# Check to see if the county is the same
cohort[, same_county := ifelse(BABY_COUNTY == MOM_COUNTY, 1, 0)]

# Method to merge:
# If the county code is the same for mom/infant, then merge on that county code
# If the code is different and neither are weird, go with the mother's zip code
# If the code is different and the mom is NA, go with the baby
# If the code is different and the baby is NA, go with the mom
# Otherwise, the county will stay NA

cohort[, county_to_merge := case_when(
  same_county == 1 ~ MOM_COUNTY,
  same_county == 0 & weird_zip_code_mom == 0 & weird_zip_code_baby == 0 ~ MOM_COUNTY,
  same_county == 0 & is.na(MOM_COUNTY) ~ BABY_COUNTY,
  same_county == 0 & is.na(BABY_COUNTY) ~ MOM_COUNTY,
  same_county == 0 & weird_zip_code_baby == 1 & weird_zip_code_mom == 0 ~ MOM_COUNTY,
  same_county == 0 & weird_zip_code_mom == 1 & weird_zip_code_baby == 0 ~ BABY_COUNTY,
  same_zip == 0 & weird_zip_code_mom == 1 & weird_zip_code_baby == 0 ~ BABY_COUNTY,
  same_zip == 0 & weird_zip_code_baby == 1 & weird_zip_code_mom == 0 ~ MOM_COUNTY,
  TRUE ~ NA_character_)]
cohort[, .N, by = county_to_merge][order(-N)]

# Get the year for the cohort (DOB year)
cohort[, year := year(DOB)]

# We don't have mortality from 2022 so the moving average is missing for 2023
# Will assign it the same values as the 2022 data
cohort[year == 2023, year := 2022]

# Just save cohort without mortality merge for now
mort_cohort <- cohort
mort_cohort[, COUNTY := county_to_merge]

# Flag all obs where either missing the county or the baby/mom state is not IN
mort_cohort[, F_nonIN_missCNTY := ifelse(is.na(county_to_merge) | MOM_STATE != "IN" | BABY_STATE != "IN", 1, 0)]

# Save the mortality merged file
fwrite(mort_cohort, here("data", "outputs", "labs_natl_cohort_mort.csv"))

