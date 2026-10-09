#===============================================================================
# Script 06: Add opioid classification flags
#
# Create a variable for the opioid only labs
# MKD
# January 19, 2024
# Updated on February 8 to incorporate the umbilical cord LOINC codes
#===============================================================================

# Install and load packages
if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)}
p_load(data.table, ggplot2, lubridate, stringr, vroom, here)

rm(list=ls()) # Remove all objects from the environment

# Read in the labs file
labs <- fread(here("data", "outputs", "all_labs.csv"))

# Read in the LOINC codes cleaned
loinc <- fread(here("data", "outputs", "loinc_umbilical_meconium_clean.csv"))

# Append 0s to the LOINC codes to make them 10 digits long
loinc[, loinc_clean := str_pad(loinc_clean, width = 10, pad = "0")]

# What are the unique values in the LONG_COMMON_NAME column?
loinc[, .N, by = LONG_COMMON_NAME]

# Create a list of opioids terms
opioids_components <- c("6-Monoacetylmorphine",
                        "Buprenorphine",
                        "Codeine",
                        "HYDROcodone",
                        "Hydrocodone",
                        "HYDROmorphone",
                        "Hydromorphone",
                        "Methadone",
                        "Methadone+Metabolite",
                        "Morphine",
                        "Opiates",
                        "Opiates cutoff",
                        "Opiates panel",
                        "oxyCODONE",
                        "oxyCODONE+Oxymorphone",
                        "Oxycodone",
                        "oxyMORphone",
                        "Oxymorphone",
                        "O-Nortramadol",
                        "traMADol",
                        "Tramadol",
                        "Acetylmorphine",
                        "Dihydrocodeine",
                        "Fentanyl",
                        "Meperidine",
                        "Naloxone",
                        "Propoxyphene",
                        "Tapentadol",
                        "N-desmethyltramadol",
                        "O-desmethyltramadol",
                        "Norbuprenorphine",
                        "Norhydrocodone",
                        "Noroxycodone",
                        "Noroxymorphone"
                        )

# Create an opioid varaible in loinc
loinc[, opioids := ifelse(grepl(paste(opioids_components, collapse = "|"), LONG_COMMON_NAME), 1, 0)]

# Drop all variables except for loinc_clean, meconium, umbilical, opioids
loinc <- loinc[, .(loinc_clean, opioids)]

# Append 0s to loinc clean in labs
labs[, loinc_clean := str_pad(LOINC_clean, width = 10, pad = "0")]

# Merge this onto the labs file
labs <- merge(labs, loinc, by = "loinc_clean", all.x = TRUE)

# Write the new labs file with the opioids variable
fwrite(labs, here("data", "outputs", "all_labs_opioids.csv"))
