#===============================================================================
# Script 12: Apply exclusions, create final analytic sample
#
# Merge the mom characteristics with the infant characteristics. Create and clean the analytic file
# MKD
# May 14, 2024
#===============================================================================

# Install packages
if(!require(pacman)) install.packages("pacman")
library(pacman)
p_load(data.table, lubridate, fixest, modelsummary, collapse, ggplot2, dplyr, gridExtra, here)
rm(list = ls())

# Exclusion criteria
min_age_at_delivery <- 12
max_age_at_delivery <- 55
min_delivery_date <- as.Date("2019-01-01")
valid_mom_state <- "IN"
valid_baby_state <- "IN"
mortality_suppression_threshold <- 10
mortality_suppression_value <- 5

# Read in data and harmonize formats
moms <- fread(here("data", "outputs", "mom_sud_cpd_preec_enc_ins.csv"))
moms[, DELIVERY_DATE := as.Date(DELIVERY_DATE)]
moms[, STUDY_ID := as.character(STUDY_ID)]
moms[, BABY_STUDY_ID := as.character(BABY_STUDY_ID)]

cohort <- fread(here("data", "outputs", "labs_natl_cohort_mort.csv"))
cohort[, DELIVERY_DATE := as.Date(DELIVERY_DATE)]
cohort[, MOM_STUDY_ID := as.character(MOM_STUDY_ID)]
cohort[, BABY_STUDY_ID := as.character(BABY_STUDY_ID)]

# Merge the moms with the cohort
moms_cohort <- merge(moms, cohort, by.x = c("BABY_STUDY_ID", "STUDY_ID", "DELIVERY_DATE"), by.y = c("BABY_STUDY_ID", "MOM_STUDY_ID", "DELIVERY_DATE"), all = TRUE)

# Read in the new mortality data
mort <- fread(here("data", "inputs", "IN_mortality_summary_201118_201418.csv"))
mort <- mort[FIPS != 18000]
mort[all_od_deaths_201418 < 10] # 15
mort[all_od_deaths_201118 < 10] # 10
# If less than 10 in the cell, replace with 5
mort[all_od_deaths_201418 < 10, all_od_deaths_201118 := 5] # 10
mort[all_od_deaths_201118 < 10, all_od_deaths_201118 := 5] # 5

# Rename 2011:2018 with pop2011 etc
setnames(mort, old = c("2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018"), new = c("pop2011", "pop2012", "pop2013", "pop2014", "pop2015", "pop2016", "pop2017", "pop2018"))

# Calculate a crude mortality rate
mort[, pop_201118 := pop2011 + pop2012 + pop2013 + pop2014 + pop2015 + pop2016 + pop2017 + pop2018]
mort[, pop_201418 := pop2014 + pop2015 + pop2016 + pop2017 + pop2018]
mort[, crude_mort_201118_10k := all_od_deaths_201118 / pop_201118 * 10000]
mort[, crude_mort_201418_10k := all_od_deaths_201418 / pop_201418 * 10000]

# Merge the mortality data with the moms_cohort
moms_cohort <- merge(moms_cohort, mort, by.x = "COUNTY", by.y = "FIPS", all.x = TRUE)

# Now, create an analytic sample that excludes weird issues (moms with gender as male, moms with age < 12,
# moms with age > 60, moms with delivery date < 2019)
# Also exclude moms with states that aren't IN or with weird ZIP codes
# Where the DELIVERY_DATE is different than the child's DOB, also exclude
moms_cohort[MOM_GENDER != "F", .N, by = MOM_GENDER]
moms_cohort[age_at_deliv < 12, .N]
moms_cohort[age_at_deliv > 55, .N]
moms_cohort[DELIVERY_DATE < as.Date("2019-01-01"), .N]
moms_cohort[MOM_STATE != "IN", .N]
moms_cohort[MOM_STATE != "IN", .N, by = MOM_STATE]
moms_cohort[BABY_STATE != "IN", .N]
moms_cohort[is.na(MOM_COUNTY), .N]
moms_cohort[is.na(BABY_COUNTY), .N]
moms_cohort[is.na(COUNTY)]
moms_cohort[DELIVERY_DATE != DOB, .N]
nrow(moms_cohort)

# Exclude moms
moms_cohort2 <- moms_cohort[MOM_GENDER == "F" & age_at_deliv >= 12 & age_at_deliv <= 55 & DELIVERY_DATE >= as.Date("2019-01-01") & MOM_STATE == "IN" & DELIVERY_DATE == DOB &
                            !is.na(COUNTY)]
nrow(moms_cohort2)

# Now create a variable that is: child was tested, child had a positive test.
moms_cohort2[, tested := as.numeric(total_tests > 0)]
moms_cohort2[is.na(tested), tested := 0]

moms_cohort2[, positive_test := as.numeric(total_pos > 0)]
moms_cohort2[is.na(positive_test), positive_test := 0]

# Summarize the testing rates
moms_cohort2[, .N, by = c("tested", "positive_test")]

# Create binary indicators for maternal diagnoses
diagnosis_indicators <- list(
  list(source = "n_before_sud", target = "any_sud", threshold = 0),
  list(source = "n_before_cpd", target = "any_cpd", threshold = 0),
  list(source = "n_before_hypt", target = "any_hypt", threshold = 0),
  list(source = "n_before_gest_diab", target = "any_gest_diab", threshold = 0),
  list(source = "n_before_mood", target = "any_mood_dis", threshold = 0),
  list(source = "n_before_anxiety", target = "any_anxiety", threshold = 0),
  list(source = "n_before_schiz", target = "any_schiz", threshold = 0),
  list(source = "n_before_mental_health", target = "any_ment_hlth", threshold = 0),
  list(source = "n_before_prenatal_care", target = "any_prenatal", threshold = 0),
  list(source = "n_before_prenatal_care", target = "twelve_prenatal_plus", threshold = 11)
)

for (indicator in diagnosis_indicators) {
  source_col <- indicator$source
  target_col <- indicator$target
  threshold <- indicator$threshold
  moms_cohort2[, (target_col) := as.numeric(get(source_col) > threshold)]
  moms_cohort2[is.na(get(target_col)), (target_col) := 0]
}

# Year is the year of the delivery date
moms_cohort2[, year := year(DELIVERY_DATE)]

# Save the file
fwrite(moms_cohort2, here("data", "outputs", "sample_with_maternal_characteristics.csv"))
