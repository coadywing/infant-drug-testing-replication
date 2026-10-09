#===============================================================================
# Script 08: Add county info via ZIP crosswalk
#
# Merge on the county crosswalk data
# MKD
# March 29 2024
#===============================================================================

# Install and load packages.
if (!require("pacman")) {
  install.packages("pacman",)
  library(pacman)}
p_load(data.table, ggplot2, readxl, zctaCrosswalk, here)
rm(list=ls()) # Remove all objects from the environment
gc() # Clean up memory

# Read in the county crosswalk file: this is available from the US Census Bureau and links ZIP codes to counties
county_crosswalk <- fread(here("data", "inputs", "ZIP_COUNTY_122023.csv"))
zcta_crosswalk <- zcta_crosswalk
setDT(zcta_crosswalk)

# Drop USPS_ZIP_PREF_CITY, RES_RATIO, BUS_RATIO, OTH_RATIO, TOT_RATIO
county_crosswalk[, c("RES_RATIO", "BUS_RATIO", "OTH_RATIO", "TOT_RATIO", "USPS_ZIP_PREF_CITY") := NULL]

county_crosswalk[duplicated(ZIP) & USPS_ZIP_PREF_STATE == "IN", .N]
county_crosswalk[duplicated(ZIP) & USPS_ZIP_PREF_STATE == "IN"]

zcta_crosswalk[duplicated(zcta) & state_usps == "IN"]

# Drop the duplicate ZIP codes in IN
county_crosswalk <- county_crosswalk[!duplicated(county_crosswalk[, c("ZIP", "USPS_ZIP_PREF_STATE", "USPS_ZIP_PREF_STATE")]), ]
setnames(county_crosswalk, old = "USPS_ZIP_PREF_STATE", new = "STATE")

# Read in the cohort file
cohort <- fread(here("data", "outputs", "cohort_with_lab.csv"))

# Change the data type of the ZIP for the cohort and crosswalk files
cohort[, ZIP := as.character(ZIP)]
county_crosswalk[, ZIP := as.character(ZIP)]

# Merge in the county crosswalk data
cohort2 <- merge(cohort, county_crosswalk, by = "ZIP", all.x = TRUE, all.y = FALSE)

# Flag weird zip codes (less than five digits or character/letter in ZIP code)
cohort2[, weird_zip_code_baby := ifelse(nchar(ZIP) < 5 | grepl("[a-zA-Z]", ZIP), 1, 0)][weird_zip_code_baby == 1, .N]
setnames(cohort2, old = c("COUNTY", "STATE", "ZIP"), new = c("BABY_COUNTY", "BABY_STATE", "BABY_ZIP"))

# Merge on the county data by the mother's zip code
cohort3 <- merge(cohort2, county_crosswalk, by.x = "MOM_ZIP", by.y = "ZIP", all.x = TRUE, all.y = FALSE)
setnames(cohort3, old = c("COUNTY", "STATE"), new = c("MOM_COUNTY", "MOM_STATE"))

cohort3[, weird_zip_code_mom := ifelse(nchar(MOM_ZIP) < 5 | grepl("[a-zA-Z]", MOM_ZIP), 1, 0)][weird_zip_code_mom == 1, .N]

# Save the file
fwrite(cohort3, here("data", "outputs", "labs_natl_cohort.csv"))
