#===============================================================================
# Script 27: Compare INPC to NVSS (Table A6)
#
# Summary comparison of births in INPC analytic sample to entire NVSS data
# MKD
# December 6, 2024
#===============================================================================

# Packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, lubridate, here)

# Load the data
dt <- fread(here("data", "outputs", "final_analytic_sample_with_infant_char_for_reg.csv"))

# Summarize the data for all infants 
# Average delivery age, age bins of 15-19, 20-24, 25-29, 30-34, 35-39, 40-44, 45+ 
# Prental care received, more than 12 visits 
# Mom black, white, or other 
# Medicaid, other insurance, private insurance 
# Gestational diabetes, gestational hypertension 
# Premature birth 
# low birth weight
summary <- dt[, .(
    source = "INPC",
    age = mean(age_at_deliv),
    age_15_19 = mean(age_bin15), 
    age_20_24 = mean(age_bin20),
    age_25_29 = mean(age_bin25),
    age_30_34 = mean(age_bin30),
    age_35_39 = mean(age_bin35),
    age_40_44 = mean(age_bin40),
    age_45_plus = mean(age_bin45),
    prenatal_care = mean(any_prenatal),
    prenatal_care_12 = mean(twelve_prenatal_plus),
    mom_black = mean(mom_black),
    mom_white = mean(mom_white),
    mom_other = mean(mom_other),
    medicaid = mean(medicaid),
    other_insurance = mean(other_ins),
    private_insurance = mean(private_ins),
    gestational_diabetes = mean(any_gest_diab),
    gestational_hypertension = mean(any_hypt), 
    premature_birth = mean(premature),
    lbw = mean(lbw), 
    N = .N)]

# Read in the NVSS summary data
nvss <- fread(here("data", "outputs", "infant_summary_in.csv"))
nvss[, source := "NVSS"]

# Rbind the data 
summary <- rbind(summary, nvss)

# Save 
fwrite(summary, here("results", "compare_nvss.csv"))