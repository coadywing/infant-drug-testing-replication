#===============================================================================
# Script 14: Merge NAS incidence by county
#
# Add in neonatal abstinence syndrome
# MKD
# July 16, 2024
#===============================================================================

# Install packages
if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(data.table, collapse, lubridate, ggplot2, here)
rm(list = ls())

# Filter criteria
valid_state <- "IN"

# Read in the data
cohort <- fread(here("data", "outputs", "sample_with_maternal_infant_characteristics.csv"))

# Read in the NAS data
nas <- fread(here("data", "inputs", "nas_counts_by_zip.csv"))

# Read in the county crosswalk
county_crosswalk <- fread(here("data", "inputs", "ZIP_COUNTY_122023.csv"))

# Clean up the county crosswalk
# Drop unnecessary columns
county_crosswalk[, c("RES_RATIO", "BUS_RATIO", "OTH_RATIO", "TOT_RATIO", "USPS_ZIP_PREF_CITY") := NULL]

# Rename state column
setnames(county_crosswalk, old = "USPS_ZIP_PREF_STATE", new = "STATE")

# Drop duplicate ZIP codes (keep first occurrence)
county_crosswalk <- county_crosswalk[!duplicated(county_crosswalk[, c("ZIP", "STATE")]), ]

# Filter to valid state
county_crosswalk <- county_crosswalk[STATE == valid_state]

# Change the data type of the ZIP for the NAS and crosswalk files
nas[, ZIP := as.character(ZIP)]
county_crosswalk[, ZIP := as.character(ZIP)]

# Drop the ZIP codes where there are 0 for NAS1, NAS2, or births
nas <- nas[NAS1 > 0 | NAS2 > 0 | births > 0]

# Merge the NAS with the county crosswalk
nas2 <- merge(nas, county_crosswalk, by = "ZIP", all.x = TRUE, all.y = FALSE)

# Limit to valid state
nas_in <- nas2[STATE == valid_state]

# Count the NAS1, NAS2, and births by county-state
nas_in_cty <- nas_in[, .(NAS1 = sum(NAS1), NAS2 = sum(NAS2), births = sum(births)), by = .(COUNTY, STATE)][order(-NAS1)]

# Calculate the NAS rate
nas_in_cty[, NAS1_rate := NAS1 / births]
nas_in_cty[, NAS2_rate := NAS2 / births]

# Merge the NAS rates with the cohort data
cohort2 <- merge(cohort, nas_in_cty, by = "COUNTY", all.x = TRUE, all.y = FALSE)

# Find the average NAS rate and create a dummy that is 0/1 above average or not
cohort2[, above_avg_nas1 := as.numeric(NAS1_rate > mean(nas_in_cty$NAS1_rate, na.rm = TRUE))]
mean(cohort2$above_avg_nas1, na.rm = TRUE) # 0.2034303

# Save the data
fwrite(cohort2, here("data", "outputs", "sample_with_maternal_infant_nas_characteristics.csv"))
