# Oaxaca-Blinder Decomposition - Appendix Table A4
# MKD
# July 2024

if(!require(pacman)) install.packages("pacman")
library(pacman)
p_load(data.table, fixest, boot, pbapply, xtable, here)
rm(list = ls())

# Output directory
output_dir <- here("results")

# Read in the data
dt <- fread(here("data", "outputs", "final_analytic_sample_with_infant_char_for_reg.csv"))

# Create a dummy variable for Black
dt1 <- dt[race_recode == "White" | race_recode == "Black"]
dt1[, black := as.numeric(race_recode == "Black")]

# Create a dummy variable for above average crude mortality rate
dt1[, above_avg_mort := as.numeric(crude_mort_201418_10k > mean(crude_mort_201418_10k, na.rm = TRUE))]

# Create a dummy variable for 3 or more encounters within 1 month 
dt1[, three_plus_encs := as.numeric(encounters_within_1month >= 3)]

# Create a dummy variable for 4 or more encounters within 1 month
dt1[, four_plus_encs := as.numeric(encounters_within_1month >= 4)]

dt1[, mean(three_plus_encs), by = black]

# Save the output as oaxaca
fwrite(dt1, here("data", "outputs", "oaxaca.csv"))

rm(dt, dt1)

# Run a regression
dt <- fread(here("data", "outputs", "oaxaca.csv"))
model_white <- feols(tested ~ age_bin20 + age_bin25 + age_bin30 + age_bin35 + age_bin40 + age_bin45 +
                above_avg_nas1 + any_sud + any_prenatal + above_avg_mort + any_ment_hlth +
                any_hypt + medicaid + other_ins + lbw + premature, data = dt[black == 0])
model_black <- feols(tested ~ age_bin20 + age_bin25 + age_bin30 + age_bin35 + age_bin40 + age_bin45 +
                above_avg_nas1 + any_sud + any_prenatal + above_avg_mort + any_ment_hlth +
                any_hypt + medicaid + other_ins + lbw + premature, data = dt[black == 1])

# Oaxaca Blinder Decomposition
x_bar_black <- c(1, colMeans(dt[black == 1, .SD, .SDcols = c("age_bin20", "age_bin25", "age_bin30", "age_bin35", "age_bin40", "age_bin45",
                                                          "above_avg_nas1", "any_sud", "any_prenatal", "above_avg_mort", "any_ment_hlth",
                                                          "any_hypt", "medicaid", "other_ins", "lbw", "premature")]))
x_bar_white <- c(1, colMeans(dt[black == 0, .SD, .SDcols = c("age_bin20", "age_bin25", "age_bin30", "age_bin35", "age_bin40", "age_bin45",
                                                            "above_avg_nas1", "any_sud", "any_prenatal", "above_avg_mort", "any_ment_hlth",
                                                            "any_hypt", "medicaid", "other_ins", "lbw", "premature")]))

beta_black <- coef(model_black)
beta_white <- coef(model_white)

delta_x <-  x_bar_black - x_bar_white
delta_b <- beta_black - beta_white

# # Delta_X Beta_Black  + Delta_Beta X_Black + Delta_X Delta_Beta 
# delta_x_beta_black <- sum(delta_x * beta_black)
# delta_beta_x_black <- sum(delta_b * x_bar_black)
# delta_x_delta_beta <- sum(delta_x * delta_b) 

# Delta_X Beta_Black + Delta_beta X_White 
delta_x_beta_black <- sum(delta_x * beta_black)
delta_beta_x_white <- sum(delta_b * x_bar_white)

delta_x_beta_black + delta_beta_x_white

# Or Delta_X Beta_White + Delta_Beta X_Black 
delta_x_beta_white <- sum(delta_x * beta_white)
delta_beta_x_black <- sum(delta_b * x_bar_black)

delta_x_beta_white + delta_beta_x_black

# Calculate the raw gap 
raw_gap <- dt[black == 1, mean(tested)] - dt[black == 0, mean(tested)]

# Function to calculate Oaxaca-Blinder decomposition for bootstrap sample
oaxaca_bootstrap <- function(data, indices) {
  data_boot <- data[indices, ]
  
  # Run regressions for White and Black groups
  model_white <- feols(tested ~ age_bin20 + age_bin25 + age_bin30 + age_bin35 + age_bin40 + age_bin45 +
                  above_avg_nas1 + any_sud + any_prenatal + above_avg_mort + any_ment_hlth +
                  any_hypt + medicaid + other_ins + lbw + premature, data = data_boot[black == 0])
  model_black <- feols(tested ~ age_bin20 + age_bin25 + age_bin30 + age_bin35 + age_bin40 + age_bin45 +
                  above_avg_nas1 + any_sud + any_prenatal + above_avg_mort + any_ment_hlth +
                  any_hypt + medicaid + other_ins + lbw + premature, data = data_boot[black == 1])
  
  # Calculate means for the groups
  x_bar_black <- c(1, colMeans(data_boot[black == 1, .SD, .SDcols = c("age_bin20", "age_bin25", "age_bin30", "age_bin35", "age_bin40", "age_bin45",
                                                                  "above_avg_nas1", "any_sud", "any_prenatal", "above_avg_mort", "any_ment_hlth",
                                                                  "any_hypt", "medicaid", "other_ins", "lbw", "premature")]))
  x_bar_white <- c(1, colMeans(data_boot[black == 0, .SD, .SDcols = c("age_bin20", "age_bin25", "age_bin30", "age_bin35", "age_bin40", "age_bin45",
                                                                  "above_avg_nas1", "any_sud", "any_prenatal", "above_avg_mort", "any_ment_hlth",
                                                                  "any_hypt", "medicaid", "other_ins", "lbw", "premature")]))
  
  beta_black <- coef(model_black)
  beta_white <- coef(model_white)

  delta_x <-  x_bar_black - x_bar_white
  delta_b <- beta_black - beta_white
  
  # Calculate components
  delta_x_beta_black <- sum(delta_x * beta_black)
  delta_beta_x_white <- sum(delta_b * x_bar_white)  

  delta_x_beta_white <- sum(delta_x * beta_white)
  delta_beta_x_black <- sum(delta_b * x_bar_black)

  return(c(delta_x_beta_black, delta_beta_x_white, delta_x_beta_white, delta_beta_x_black))
}

# Apply bootstrapping
set.seed(123)  # For reproducibility
bootstrap_results <- boot(data = dt, statistic = oaxaca_bootstrap, R = 1000)

# Extract bootstrap standard errors
bootstrap_se <- apply(bootstrap_results$t, 2, sd)

# Save to a table 
oaxaca_table <- data.table(
  "Component" = c("Raw Gap", "Endowments", "Coefficients"),
  "Gap" = c(raw_gap, delta_x_beta_black, delta_beta_x_white),
  "Standard Errors" = c(0, bootstrap_se[1], bootstrap_se[2]), 
  "Share of Raw Gap" = c(1, delta_x_beta_black / raw_gap, delta_beta_x_white / raw_gap)
)

oaxaca_table2 <- data.table(
  "Component" = c("Raw Gap", "Endowments", "Coefficients"),
  "Gap" = c(raw_gap, delta_x_beta_white, delta_beta_x_black),
  "Standard Errors" = c(0, bootstrap_se[3], bootstrap_se[4]), 
  "Share of Raw Gap" = c(1, delta_x_beta_white / raw_gap, delta_beta_x_black / raw_gap)
)

# Save the table as Latex
print(xtable(oaxaca_table, caption = "Oaxaca-Blinder Decomposition of the Racial Gap in Infant Drug Testing", label = "tab:oaxaca_blinder", digits = 5), type = "latex", file = file.path(output_dir, "oaxaca_blinder1.tex"))
print(xtable(oaxaca_table2, caption = "Oaxaca-Blinder Decomposition of the Racial Gap in Infant Drug Testing", label = "tab:oaxaca_blinder2", digits = 5), type = "latex", file = file.path(output_dir, "oaxaca_blinder2.tex"))