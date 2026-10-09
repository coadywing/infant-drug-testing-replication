#===============================================================================
# Script 21: Propensity Score Weighted Decomposition (Table 2)
#
# Inverse probability weighting decomposition of racial gap in infant drug testing
# MKD
# August 6, 2024
#===============================================================================

if(!require(pacman)) install.packages("pacman")
library(pacman)
p_load(data.table, fixest, boot, pbapply, xtable, survey, here)
rm(list = ls())

# Output directory
output_dir <- here("results")

# Read in data
dt <- fread(here("data", "outputs", "final_analytic_sample_with_infant_char_for_reg.csv"))

### Propensity Score, Inverse Probability Weighting ### 
# Create a propensity score model 
dt[, white := as.numeric(black == 0)]
propensity_model <- glm(black ~ age_bin20 + age_bin25 + age_bin30 + age_bin35 + age_bin40 + age_bin45 + 
                          above_avg_nas1 + any_sud + any_prenatal + above_avg_mort + any_ment_hlth + 
                          any_hypt + medicaid + other_ins + lbw + premature + four_plus_encs, data = dt, family = "binomial")
propensity_model_white <- glm(white ~ age_bin20 + age_bin25 + age_bin30 + age_bin35 + age_bin40 + age_bin45 + 
                          above_avg_nas1 + any_sud + any_prenatal + above_avg_mort + any_ment_hlth + 
                          any_hypt + medicaid + other_ins + lbw + premature + four_plus_encs + mom_black + mom_other, data = dt, family = "binomial")

# Calculate the propensity score
dt[, propensity_score := predict(propensity_model, type = "response")]
dt[, propensity_score_white := predict(propensity_model_white, type = "response")]

# Calculate the weights 
# If white, ATT weight is 1
# If black, ATT weight is ps / (1 - ps)
dt[, ATT_weight := ifelse(black == 1, 1, propensity_score / (1 - propensity_score))]
# dt[, ATT_weight_white := ifelse(white == 1, 1, propensity_score_white / (1 - propensity_score_white))]
dt[, ATT_weight_white2 := ifelse(white == 1, 1, (1 - propensity_score)/propensity_score)]

# White observed mean 
white_observed_mean <- dt[black == 0, mean(tested)]

# Black observed mean
black_observed_mean <- dt[black == 1, mean(tested)]

# Black reweighted mean
design_black <- svydesign(ids = ~1, data = dt[black == 1], weights = ~ATT_weight_white2)
black_reweighted_mean <- svymean(~tested, design_black)

# White reweighted mean
design_white <- svydesign(ids = ~1, data = dt[black == 0], weights = ~ATT_weight)
white_reweighted_mean <- svymean(~tested, design_white)

print(white_observed_mean)
print(black_observed_mean)
print(black_reweighted_mean)

# Raw gap 
a <- black_observed_mean - white_observed_mean

# Decomposition
b <- black_reweighted_mean - white_observed_mean

c <- black_observed_mean - black_reweighted_mean

d <- white_reweighted_mean - white_observed_mean 

e <- black_observed_mean - white_reweighted_mean

# Bootstrap the standard errors
calc_gap <- function(data, indices) {
  dt_boot <- data[indices]
  
  design_black_boot <- svydesign(ids = ~1, data = dt_boot[black == 1], weights = ~ATT_weight_white2)
  black_reweighted_mean_boot <- svymean(~tested, design_black_boot)
  
  design_white_boot <- svydesign(ids = ~1, data = dt_boot[black == 0], weights = ~ATT_weight)
  white_reweighted_mean_boot <- svymean(~tested, design_white_boot)
  
  a_boot <- dt_boot[black == 1, mean(tested)] - dt_boot[black == 0, mean(tested)]
  b_boot <- black_reweighted_mean_boot - dt_boot[black == 0, mean(tested)]
  c_boot <- dt_boot[black == 1, mean(tested)] - black_reweighted_mean_boot
  d_boot <- white_reweighted_mean_boot - dt_boot[black == 0, mean(tested)]
  e_boot <- dt_boot[black == 1, mean(tested)] - white_reweighted_mean_boot
  
  return(c(a_boot, b_boot, c_boot, d_boot, e_boot))
}

# Perform bootstrapping
set.seed(123)
boot_results <- boot(data = dt, statistic = calc_gap, R = 1000)

# Calculate standard errors
boot_se <- apply(boot_results$t, 2, sd)

# Create a table that is Panel A: observed outcome, reweighted outcome for rows white and black infants 
# Then Panel B is the decomposition 

# Panel A
panel_a <- data.table(
  group = c("Observed", "Reweighted"),
  White = c(white_observed_mean, white_reweighted_mean),
  Black = c(black_observed_mean, black_reweighted_mean)
)

# Panel B 
result_table <- data.table(
  "Component" = c("Raw Gap", "B re - W obs", "B obs - B re"),
  "Gap" = c(a, b, c),
  "Standard Errors" = c(boot_se[1], boot_se[2], boot_se[3]), 
  "Share of Raw Gap" = c(1, b / a, c / a)
)

result_table2 <- data.table(
  "Component" = c("Raw Gap", "W re - W obs", "B obs - W re"),
  "Gap" = c(a, d, e),
  "Standard Errors" = c(boot_se[1], boot_se[4], boot_se[5]), 
  "Share of Raw Gap" = c(1, d / a, e / a)
)

# Export the panels as 1 table to latex
print(xtable(panel_a, digits = c(0, 0, 4, 4)), type = "latex", file = file.path(output_dir, "panel_a.tex"))
print(xtable(result_table, digits = c(0, 0, 4, 4, 4)), type = "latex", file = file.path(output_dir, "panel_b.tex"))
print(xtable(result_table2, digits = c(0, 0, 4, 4, 4)), type = "latex", file = file.path(output_dir, "panel_c.tex"))
