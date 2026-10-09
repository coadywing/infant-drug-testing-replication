# Read in the 2019 natality data, limit to Indiana, and average characteristics for the full sample compared to INPC 
# Average delivery age, age bins of 15-19, 20-24, 25-29, 30-34, 35-39, 40-44, 45+ 
# Prental care received, more than 12 visits 
# Mom black, white, or other 
# Medicaid, other insurance, private insurance 
# Gestational diabetes, gestational hypertension 
# Premature birth 

# Packages 
if(!require(pacman)) install.packages("pacman")
pacman::p_load(data.table, dplyr, readr, stringr, ggplot2, here)
rm(list = ls())
source(here("code", "00_paths.r"))

list.files(path_nvss_natality_dir)

# Read in the 2019-2022 data
dt.all <- rbindlist(lapply(2019:2022, function(year) {
  fread(sprintf("%s/Natality_%d.csv", path_nvss_natality_dir, year))
}), fill = TRUE)

# Limit to Indiana
dt.all <- dt.all[OSTATE == "IN"]
dt.all[, .N, by = DOB_YY]

# Create a new age variable
dt.all[, age := MAGE_R]
dt.all[is.na(age), age := MAGER]
dt.all[, .N, by = age]

# Create dummy variables for age bins
age_bins <- list(c(15, 19), c(20, 24), c(25, 29), c(30, 34), c(35, 39), c(40, 44))
for (bin in age_bins) {
  var_name <- sprintf("age_%d_%d", bin[1], bin[2])
  dt.all[, (var_name) := ifelse(age >= bin[1] & age <= bin[2], 1, 0)]
}
dt.all[, age_45_plus := ifelse(age >= 45, 1, 0)]

# Create a variable for prenatal care received 
dt.all[, prenatal_care := ifelse(PRECARE != 0 & PRECARE != 99, 1, 0)]
dt.all[, .N, by = prenatal_care]

# At least 12 prenatal care visits dummy variable 
dt.all[, prenatal_care_12 := ifelse(PNCVIS >= 12, 1, 0)]
dt.all[is.na(prenatal_care_12), prenatal_care_12 := ifelse(PREVIS >= 12, 1, 0)]
dt.all[, .N, by = prenatal_care_12]

# Create dummy variables for mom race
dt.all[, mom_white := coalesce(MRACE_R6 == 1, MRACE6 == 1)]
dt.all[, mom_black := coalesce(MRACE_R6 == 2, MRACE6 == 2)]
dt.all[, mom_other := !(coalesce(MRACE_R6 == 1, MRACE6 == 1) | coalesce(MRACE_R6 == 2, MRACE6 == 2))]
dt.all[, c("mom_white", "mom_black", "mom_other") := lapply(.SD, as.integer), .SDcols = c("mom_white", "mom_black", "mom_other")]

# Create a dummy variable for Medicaid, other insurance, private insurance
dt.all[, medicaid := ifelse(PAY == 1, 1, 0)]
dt.all[, private_insurance := ifelse(PAY == 2, 1, 0)]
dt.all[, other_insurance := ifelse(PAY != 1 & PAY != 2, 1, 0)]

# Create dummy variables for health conditions
dt.all[, gestational_diabetes := as.integer(RF_GDIAB == "Y")]
dt.all[, gestational_hypertension := as.integer(RF_GHYPE == "Y")]
dt.all[, premature_birth := as.integer(coalesce(OEGEST < 37, COMBGEST < 37))]
dt.all[, lbw := as.integer(BWTR4 <= 2)]

# Summarize the data for the variables created by taking the mean
summary_vars <- c("age",
                  grep("^age_", names(dt.all), value = TRUE),
                  "prenatal_care", "prenatal_care_12",
                  "mom_white", "mom_black", "mom_other",
                  "medicaid", "private_insurance", "other_insurance",
                  "gestational_diabetes", "gestational_hypertension",
                  "premature_birth", "lbw")
summary <- dt.all[, c(lapply(.SD, mean), list(N = .N)), .SDcols = summary_vars]

# Save the summary data 
fwrite(summary, here("data", "outputs", "infant_summary_in.csv"))
