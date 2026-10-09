#===============================================================================
# Script 01: Convert SAS natality linked data to CSV
#
# Convert natality linked data to csv from sas7bdat
# MKD
# March 19 2024
#===============================================================================

# Install packages
if (!require("pacman")) {
  install.packages("pacman",)
  library(pacman)}
p_load(data.table, haven, here)

# External data paths
input_mom_sas <- file.path(path_inpc_sas_dir, "rdrp5665_mom_matched_fnl.sas7bdat")
input_baby_sas <- file.path(path_inpc_sas_dir, "rdrp5665_baby_matched_fnl.sas7bdat")

# Read in SAS files
mom_matched <- read_sas(input_mom_sas)
baby_matched <- read_sas(input_baby_sas)

# Write out the files to csv
fwrite(mom_matched, here("data", "inputs", "mom_matched.csv"))
fwrite(baby_matched, here("data", "inputs", "baby_matched.csv"))
