#===============================================================================
# Script 03: Build linked mother-infant cohort
#
# Create a file with the mom's and children linked to identify births in INPC
# MKD
# March 28, 2024
#===============================================================================

# Install and load packages.
if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)}
p_load(data.table, lubridate, collapse, here)
rm(list=ls())
source(here("code", "00_paths.r"))

# External data paths
input_demographics <- file.path(path_inpc_csv_dir, "demo.sas7bdat.csv")

# Read in the demographics files and the mom and baby matched files
mom_matched <- fread(here("data", "inputs", "mom_matched.csv"))
baby_matched <- fread(here("data", "inputs", "baby_matched.csv"))
demographics <- fread(input_demographics)

# Deduplicate the baby matched file
baby_matched <- baby_matched[!duplicated(STUDY_ID)][order(DATE_OF_BIRTH)]

# Drop duplicated demographics 
demographics <- demographics[!duplicated(STUDY_ID)]

# Merge the baby and mom matched files
setnames(mom_matched, old = "STUDY_ID", new = "MOM_STUDY_ID")
setnames(baby_matched, old = "STUDY_ID", new = "BABY_STUDY_ID")
baby_matched[, .N, by = c("BIRTH_ID_DE", "BABY_STUDY_ID")][order(-N)] 
births <- merge(mom_matched, baby_matched, by = "BIRTH_ID_DE", all = TRUE)

# Drop the NAs of the babys
births <- births[!is.na(BABY_STUDY_ID)]

# Drop the duplicates. Order by DOB. Will take the first MOM_STUDY_ID for now
births <- births[!duplicated(BABY_STUDY_ID)][order(DATE_OF_BIRTH)]

# Save the file
fwrite(births, here("data", "outputs", "births_linked_dedup.csv"))

# Merge the demographics file with the births file (baby only and mom only)
# Create a demo file for infants
demo_inf <- demographics[STUDY_ID %in% births$BABY_STUDY_ID]
demo_inf <- merge(demo_inf, births, by.x = "STUDY_ID", by.y = "BABY_STUDY_ID", all.y = TRUE)
setnames(demo_inf, old = "STUDY_ID", new = "BABY_STUDY_ID")

# Create a demo file for moms
demo_mom <- demographics[STUDY_ID %in% births$MOM_STUDY_ID]
demo_mom <- merge(demo_mom, births, by.x = "STUDY_ID", by.y = "MOM_STUDY_ID", all.y = TRUE)
setnames(demo_mom, old = "STUDY_ID", new = "MOM_STUDY_ID")

# Calculate age at delivery for moms
demo_mom[, age_at_deliv := year(DATE_OF_BIRTH) - year(DOB)]
# demo_mom[, .N, by = age_at_deliv][order(age_at_deliv)]
# demo_mom[age_at_deliv < 0, .N]
# demo_mom[age_at_deliv < 12, .N]
# demo_mom[age_at_deliv >= 55, .N]
# demo_mom[age_at_deliv < 55 & age_at_deliv >= 12, .N]
# demo_mom[GENDER == "M", .N]

# Rename some of the mother variables
setnames(demo_mom, old = c("STUDY_ID", "GENDER", "DOB", "DOD", "RACE", "RACE_SUBCATEGORY", "ETHNICITY",
                            "ZIP", "CENSUS_TRACT_2010", "CENSUS_TRACT_2020", "DATE_OF_BIRTH"),
                            new = c("MOM_STUDY_ID", "MOM_GENDER", "MOM_DOB", "MOM_DOD", "MOM_RACE", "MOM_RACE_SUBCATEGORY",
                            "MOM_ETHNICITY", "MOM_ZIP", "MOM_CENSUS_TRACT_2010", "MOM_CENSUS_TRACT_2020", "DELIVERY_DATE"), skip_absent = TRUE)
# Drop all other columns
demo_mom <- demo_mom[, c("MOM_STUDY_ID", "MOM_GENDER", "MOM_DOB", "MOM_DOD", "MOM_RACE", "MOM_RACE_SUBCATEGORY",
                            "MOM_ETHNICITY", "MOM_ZIP", "MOM_CENSUS_TRACT_2010", "MOM_CENSUS_TRACT_2020", "DELIVERY_DATE",
                            "age_at_deliv", "BIRTH_ID_DE")]

# Merge mom onto infant demographics
all_demo <- merge(demo_inf, demo_mom, by = c("BIRTH_ID_DE", "MOM_STUDY_ID"))

# Check missingness
# Should have 1 study_id per row
all_demo <- all_demo[!duplicated(BABY_STUDY_ID)] # drop the duplicate STUDY_IDs

# Save the file
fwrite(all_demo, here("data", "outputs", "linked_infant_mom_demographics.csv"))