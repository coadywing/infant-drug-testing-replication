#===============================================================================
# Script 24: Appendix Figure A2 - Propensity Score Model Table
# Create an appendix table with the propensity score model
# MKD
# October 2024
#===============================================================================

# Packages
if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(data.table, fixest, lubridate, ggplot2, boot, pbapply, xtable, modelsummary, here)
rm(list = ls())

# Load data
dt <- fread(here("data", "outputs", "final_analytic_sample_with_infant_char_for_reg.csv"))

# Create a propensity score model 
propensity_model <- glm(black ~ age_bin20 + age_bin25 + age_bin30 + age_bin35 + age_bin40 + age_bin45 + 
                          above_avg_nas1 + any_sud + any_prenatal + above_avg_mort + any_ment_hlth + 
                          any_hypt + medicaid + other_ins + lbw + premature + four_plus_encs + mom_black + mom_other, data = dt, family = "binomial")

# Calculate the propensity score
dt[, propensity_score := predict(propensity_model, type = "response")]

# Create a table with the propensity score model
modelsummary(propensity_model, 
                stars = TRUE, 
                gof_map = c("nobs", "r.squared"),
                coef_rename = c("age_bin20" = "Age 20-24", "age_bin25" = "Age 25-29", 
                                "age_bin30" = "Age 30-34", "age_bin35" = "Age 35-39",
                                "age_bin40" = "Age 40-44", "age_bin45" = "Age 45-49",
                                "above_avg_nas1" = "Above Average NAS in County", "any_sud" = "Any Maternal SUD",
                                "any_prenatal" = "Any Prenatal Care", "above_avg_mort" = "Above Average Overdose Mortality in County",
                                "any_ment_hlth" = "Any Maternal Mental Health Diagnosis", "any_hypt" = "Any Maternal Hypertension",
                                "medicaid" = "Medicaid ", "other_ins" = "Other Insurance", "lbw" = "Low Birthweight",
                                "premature" = "Premature Birth", "four_plus_encs" = "Four or More Encounters",
                                "mom_black" = "Mom Black", "mom_other" = "Mom Other", "(Intercept)" = "Intercept"),
                title = "Propensity Score Model", 
                output = here("results", "appendix_table_propensity_score_model.tex"), 
                fmt = 4)
