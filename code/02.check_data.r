#===============================================================================
# Script 02: Validate and deduplicate data
#
# Validity/checking data
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

# Read in the mom and baby matched files
mom_matched <- fread(here("data", "inputs", "mom_matched.csv"))
baby_matched <- fread(here("data", "inputs", "baby_matched.csv"))

# Deduplicate the baby matched file
baby_matched <- baby_matched[!duplicated(STUDY_ID)][order(DATE_OF_BIRTH)] 

# Create a year variable in the baby matched file
baby_matched[, year := year(DATE_OF_BIRTH)]

# Deduplicate the baby matched file
baby_matched <- baby_matched[!duplicated(STUDY_ID)][order(DATE_OF_BIRTH)] 

# Read demo file. Drop duplicates.
demographics <- fread(input_demographics)
demographics <- demographics[!duplicated(STUDY_ID)]

# Merge the baby and mom matched files
setnames(mom_matched, old = "STUDY_ID", new = "MOM_STUDY_ID")
setnames(baby_matched, old = "STUDY_ID", new = "BABY_STUDY_ID")
births <- merge(mom_matched, baby_matched, by = "BIRTH_ID_DE", all = TRUE)