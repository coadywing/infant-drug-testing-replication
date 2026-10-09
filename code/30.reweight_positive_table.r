#===============================================================================
# Script 30: Positive Testing Rate - Reweighting Analysis
# Creates the appendix table A7
# Create tables showing positive testing rates among white infants vs black infants
# using propensity score reweighting to assess disparities.
# Sensitivity analysis: compares reweighting using full sample propensity scores
# vs propensity scores estimated only on the tested subsample
# MKD
# December 30, 2024
#===============================================================================

# Packages
if(!require(pacman)) {install.packages("pacman")}
library(pacman)
p_load(data.table, fixest, boot, pbapply, xtable, survey, here)
rm(list = ls())

# Load data from Script 15
dt <- fread(here("data", "outputs", "final_analytic_sample_with_infant_char_for_reg.csv"))

# Create a propensity score model 
dt[, white := as.numeric(black == 0)]
propensity_model <- glm(black ~ age_bin20 + age_bin25 + age_bin30 + age_bin35 + age_bin40 + age_bin45 + 
                          above_avg_nas1 + any_sud + any_prenatal + above_avg_mort + any_ment_hlth + 
                          any_hypt + medicaid + other_ins + lbw + premature + four_plus_encs + mom_black + mom_other, data = dt, family = "binomial")

# make a table showing the positive testing rate among white infants vs black infants 
# reweighted to match the white infants using our regular p-scores. 
# This would be using the propensity score weights to recalculate the positive testing rates 
dt[, propensity_score := predict(propensity_model, type = "response")]
dt_tested <- dt[tested == 1]
dt_tested[, ATT_weight := ifelse(black == 1, 1, propensity_score / (1 - propensity_score))]
dt_tested[, ATT_weight_white2 := ifelse(black == 0, 1, (1 - propensity_score)/propensity_score)]

# White observed mean
white_observed_mean <- dt_tested[black == 0, mean(positive_test)]

# Black observed mean
black_observed_mean <- dt_tested[black == 1, mean(positive_test)]

# Black reweighted mean
design_black <- svydesign(ids = ~1, data = dt_tested[black == 1], weights = ~ATT_weight_white2)
black_reweighted_mean <- svymean(~positive_test, design_black)

# White reweighted mean
design_white <- svydesign(ids = ~1, data = dt_tested[black == 0], weights = ~ATT_weight)
white_reweighted_mean <- svymean(~positive_test, design_white)

#------------------------------------------------
# Sensitivity analysis: Repeat with propensity scores estimated on tested sample only
rm(dt)
dt <- fread(here("data", "outputs", "final_analytic_sample_with_infant_char_for_reg.csv"))
dt_tested <- dt[tested == 1]

# Create a propensity score model-new for only the tested sample
propensity_model_tested <- glm(black ~ age_bin20 + age_bin25 + age_bin30 + age_bin35 + age_bin40 + age_bin45 + 
                          above_avg_nas1 + any_sud + any_prenatal + above_avg_mort + any_ment_hlth + 
                          any_hypt + medicaid + other_ins + lbw + premature + four_plus_encs + mom_black + mom_other, data = dt_tested, family = "binomial")

# limit to only the tested sample and calculate the propensity score
dt_tested[, propensity_score_tested := predict(propensity_model_tested, type = "response")]

# Calculate the weights
dt_tested[, ATT_weight_tested := ifelse(black == 1, 1, propensity_score_tested / (1 - propensity_score_tested))]
dt_tested[, ATT_weight_white2_tested := ifelse(black == 0, 1, (1 - propensity_score_tested)/propensity_score_tested)]

# White observed mean
white_observed_mean_tested <- dt_tested[black == 0, mean(positive_test)]

# Black observed mean
black_observed_mean_tested <- dt_tested[black == 1, mean(positive_test)]

# Black reweighted mean
design_black_tested <- svydesign(ids = ~1, data = dt_tested[black == 1], weights = ~ATT_weight_white2_tested)
black_reweighted_mean_tested <- svymean(~positive_test, design_black_tested)

# White reweighted mean
design_white_tested <- svydesign(ids = ~1, data = dt_tested[black == 0], weights = ~ATT_weight_tested)
white_reweighted_mean_tested <- svymean(~positive_test, design_white_tested)

