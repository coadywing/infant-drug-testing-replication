#===============================================================================
# Script 15: Data preparation and propensity score reweighting
#
# Prepare data and calculate propensity scores for Oaxaca decomposition
# MKD
# June 11, 2024
#===============================================================================

# Packages
if(!require("pacman")) install.packages("pacman")
library(pacman)
p_load(data.table, collapse, tidyverse, lubridate, survey, here)

# Read in the data
dt <- fread(here("data", "outputs", "sample_with_maternal_infant_nas_characteristics.csv"))

# Create the infant race_recode variable
dt[, race_recode := case_when(
  RACE == "WHITE" ~ "White",
  RACE == "BLACK OR AFRICAN AMERICAN" ~ "Black",
  RACE == "ASIAN/PACIFIC ISLANDER" ~ "Asian",
  TRUE ~ "other"
)]

print(dt[, .N, by = race_recode])

# Calculate age at delivery
dt[, age_deliv := floor(interval(MOM_DOB, DELIVERY_DATE) / years(1))]

# Create age bins
dt[, age_deliv_bin := case_when(
  age_deliv < 15 ~ "Under 15",
  age_deliv >= 15 & age_deliv < 20 ~ "15-19",
  age_deliv >= 20 & age_deliv < 25 ~ "20-24",
  age_deliv >= 25 & age_deliv < 30 ~ "25-29",
  age_deliv >= 30 & age_deliv < 35 ~ "30-34",
  age_deliv >= 35 & age_deliv < 40 ~ "35-39",
  age_deliv >= 40 ~ "40+",
  TRUE ~ "other"
)]

# Create dummy variables for age bins
age_bin_names <- c("age_bin15", "age_bin20", "age_bin25", "age_bin30", "age_bin35", "age_bin40", "age_bin45")
age_bin_labels <- c("Under 15", "15-19", "20-24", "25-29", "30-34", "35-39", "40+")

for (i in seq_along(age_bin_names)) {
  dt[, (age_bin_names[i]) := as.numeric(age_deliv_bin == age_bin_labels[i])]
}

# Create dummy variables for insurance type
dt[, medicaid := as.numeric(insurance == "Medicaid")]
dt[, private_ins := as.numeric(insurance == "Private")]
dt[, other_ins := as.numeric(medicaid == 0 & private_ins == 0)]

# Create maternal race categories
dt[, mom_race_recode := case_when(
  MOM_RACE == "WHITE" ~ "White",
  MOM_RACE == "BLACK OR AFRICAN AMERICAN" ~ "Black",
  MOM_RACE == "ASIAN/PACIFIC ISLANDER" ~ "Asian",
  TRUE ~ "other"
)]

# Create infant race dummy variables
dt[, infant_black := as.numeric(race_recode == "Black")]
dt[, infant_white := as.numeric(race_recode == "White")]

# Create maternal race dummy variables
dt[, mom_white := as.numeric(mom_race_recode == "White")]
dt[, mom_black := as.numeric(mom_race_recode == "Black")]
dt[, mom_other := as.numeric(mom_race_recode != "White" & mom_race_recode != "Black")]

# Create above average mortality indicator
dt[, above_avg_mort := as.numeric(crude_mort_201418_10k > mean(crude_mort_201418_10k, na.rm = TRUE))]

# Create four_plus_encs indicator if it doesn't exist
if (!("four_plus_encs" %in% names(dt))) {
  dt[, four_plus_encs := as.numeric(encounters_within_1month >= 4)]
}

# Create above_avg_nas1 indicator if it doesn't exist
if (!("above_avg_nas1" %in% names(dt))) {
  dt[, above_avg_nas1 := as.numeric(NAS1_rate > mean(NAS1_rate, na.rm = TRUE))]
}

# Exclude Asian/other race infants - keep only Black and White
dt <- dt[race_recode == "White" | race_recode == "Black"]

# Create black and white indicators
dt[, black := infant_black]
dt[, white := as.numeric(black == 0)]

# Fit propensity score models for black and white infants
propensity_model <- glm(
  black ~ age_bin20 + age_bin25 + age_bin30 + age_bin35 + age_bin40 + age_bin45 +
    above_avg_nas1 + any_sud + any_prenatal + above_avg_mort + any_ment_hlth +
    any_hypt + medicaid + other_ins + lbw + premature + four_plus_encs + mom_black + mom_other,
  data = dt,
  family = "binomial"
)

propensity_model_white <- glm(
  white ~ age_bin20 + age_bin25 + age_bin30 + age_bin35 + age_bin40 + age_bin45 +
    above_avg_nas1 + any_sud + any_prenatal + above_avg_mort + any_ment_hlth +
    any_hypt + medicaid + other_ins + lbw + premature + four_plus_encs + mom_black + mom_other,
  data = dt,
  family = "binomial"
)

# Calculate propensity scores
dt[, propensity_score := predict(propensity_model, type = "response")]
dt[, propensity_score_white := predict(propensity_model_white, type = "response")]

# Calculate weights for average treatment effect on the treated (ATT)
dt[, ATT_weight := ifelse(black == 1, 1, propensity_score / (1 - propensity_score))]
dt[, ATT_weight_white := ifelse(white == 1, 1, propensity_score_white / (1 - propensity_score_white))]
dt[, ATT_weight_white2 := ifelse(white == 1, 1, (1 - propensity_score) / propensity_score)]

# Save the file 
fwrite(dt, here("data", "outputs", "final_analytic_sample_with_infant_char_for_reg.csv"))

# ============================================================================
# Calculate reweighted means by race
# ============================================================================
# Limit the data to white and black infants
dt_to_reweight <- dt[race_recode == "White" | race_recode == "Black",
  .(black, white, race_recode, age_deliv, age_bin15, age_bin20, age_bin25, age_bin30, age_bin35, age_bin40, age_bin45,
    medicaid, private_ins, other_ins, any_sud, any_cpd, any_gest_diab, any_hypt, any_ment_hlth,
    any_prenatal, twelve_prenatal_plus, tested, positive_test, crude_mort_201418_10k, mom_black, mom_white, mom_other, lbw, premature, NAS1_rate, above_avg_nas1,
    encounters_within_1month, ATT_weight_white2)]

# Create survey design for reweighting
design_black <- svydesign(ids = ~1, data = dt_to_reweight[black == 1], weights = ~ATT_weight_white2)

# Calculate reweighted means for all variables
reweighted_means_black <- sapply(names(dt_to_reweight)[-1], function(var) {
  tryCatch({
    mean_val <- svymean(as.formula(paste0("~", var)), design_black)
    return(mean_val[1])
  }, error = function(e) {
    return(NA)
  })
})

# Convert to data.table
reweighted_means_black <- data.table(t(reweighted_means_black))

# Save results
fwrite(reweighted_means_black, here("data", "outputs", "temp_reweighted_means.csv"))
