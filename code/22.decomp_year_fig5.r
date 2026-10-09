#===============================================================================
# Script 22: Decomposition by Year (Figure 5)
#
# Year-by-year propensity score weighted decomposition with visualizations
# MKD
# August 9, 2024
#===============================================================================

if(!require(pacman)) install.packages("pacman")
library(pacman)
p_load(data.table, fixest, boot, pbapply, xtable, survey, ggplot2, here)
rm(list = ls())

# Load data
dt <- fread(here("data", "outputs", "final_analytic_sample_with_infant_char_for_reg.csv"))

# Calculate the propensity scores and weights for the entire dataset
dt[, white := as.numeric(black == 0)]
propensity_model <- glm(black ~ age_bin20 + age_bin25 + age_bin30 + age_bin35 + age_bin40 + age_bin45 + 
                          above_avg_nas1 + any_sud + any_prenatal + above_avg_mort + any_ment_hlth + 
                          any_hypt + medicaid + other_ins + lbw + premature + four_plus_encs, data = dt, family = "binomial")
propensity_model_white <- glm(white ~ age_bin20 + age_bin25 + age_bin30 + age_bin35 + age_bin40 + age_bin45 + 
                          above_avg_nas1 + any_sud + any_prenatal + above_avg_mort + any_ment_hlth + 
                          any_hypt + medicaid + other_ins + lbw + premature + four_plus_encs, data = dt, family = "binomial")

# Calculate the propensity scores
dt[, propensity_score := predict(propensity_model, type = "response")]
dt[, propensity_score_white := predict(propensity_model_white, type = "response")]

# Calculate the weights once
dt[, ATT_weight := ifelse(black == 1, 1, propensity_score / (1 - propensity_score))]
dt[, ATT_weight_white := ifelse(white == 1, 1, propensity_score_white / (1 - propensity_score_white))]
dt[, ATT_weight_white2 := ifelse(white == 1, 1, (1 - propensity_score)/propensity_score)]

# Initialize an empty data table to store the results
means_all <- data.table()

# Function for performing decomposition for each year
perform_decomposition <- function(year_var) {
  
  dt_year <- dt[year == year_var]
  
  # Calculate observed and reweighted means
  white_observed_mean <- dt_year[black == 0, mean(tested)]
  black_observed_mean <- dt_year[black == 1, mean(tested)]
  
  design_black <- svydesign(ids = ~1, data = dt_year[black == 1], weights = ~ATT_weight_white2)
  black_reweighted_mean <- svymean(~tested, design_black)
  
  design_white <- svydesign(ids = ~1, data = dt_year[black == 0], weights = ~ATT_weight)
  white_reweighted_mean <- svymean(~tested, design_white)

  n_white <- nrow(dt_year[black == 0])
  n_black <- nrow(dt_year[black == 1])

  # Bootstrap standard errors 
  black_boot <- boot(data = dt_year[black == 1], statistic = function(data, indices) {
    mean(data[indices, tested])
  }, R = 1000)
  white_boot <- boot(data = dt_year[black == 0], statistic = function(data, indices) {
    mean(data[indices, tested])
  }, R = 1000)
  black_reweighted_boot <- boot(data = dt_year[black == 1], statistic = function(data, indices) {
    mean(data[indices, tested], weights = dt_year[black == 1, ATT_weight_white2])
  }, R = 1000)
  
  # Save the black reweighted mean, white reweighted mean, white observed mean, and black observed mean
  means <- data.table(
    year = year_var,
    black_reweighted_mean = as.numeric(black_reweighted_mean),
    white_reweighted_mean = as.numeric(white_reweighted_mean),
    white_observed_mean = white_observed_mean,
    black_observed_mean = black_observed_mean, 
    n_white = n_white,
    n_black = n_black, 
    se_black = sd(black_boot$t),
    se_white = sd(white_boot$t),
    se_black_reweighted = sd(black_reweighted_boot$t)
  )


  # Append to the global means_all table
  means_all <<- rbindlist(list(means_all, means))
}

# Loop over years and apply the function
years <- 2019:2023
lapply(years, perform_decomposition)

# Display the means_all data table
means_all

# Calculate the confidence intervals for the reweighted means and observed means 
means_all[, ci_black_observed := 1.96 * se_black]
means_all[, ci_white_observed := 1.96 * se_white]
means_all[, ci_black_reweighted := 1.96 * se_black_reweighted]
means_all[, ci_upper_black_obs := black_observed_mean + ci_black_observed]
means_all[, ci_lower_black_obs := black_observed_mean - ci_black_observed]
means_all[, ci_upper_white_obs := white_observed_mean + ci_white_observed]
means_all[, ci_lower_white_obs := white_observed_mean - ci_white_observed]
means_all[, ci_upper_black_rew := black_reweighted_mean + ci_black_reweighted]
means_all[, ci_lower_black_rew := black_reweighted_mean - ci_black_reweighted]

# Create a figure that is a line for each mean (black observed, white observed, black reweighted)
# Plot the testing
# Assuming means_all contains the means for each year
# Adding labels at the end of the lines for each mean
means_all$label_black_observed <- ifelse(means_all$year == max(means_all$year), "Black Observed", NA)
means_all$label_white_observed <- ifelse(means_all$year == max(means_all$year), "White Observed", NA)
means_all$label_black_reweighted <- ifelse(means_all$year == max(means_all$year), "Black Reweighted", NA)

# Create the plot
plot <- 
  ggplot() + 
  geom_line(data = means_all, aes(x = year, y = black_observed_mean, color = "Black Observed")) +
  geom_point(data = means_all, aes(x = year, y = black_observed_mean, color = "Black Observed")) +
  geom_line(data = means_all, aes(x = year, y = white_observed_mean, color = "White Observed")) + 
  geom_point(data = means_all, aes(x = year, y = white_observed_mean, color = "White Observed")) +
  geom_line(data = means_all, aes(x = year, y = black_reweighted_mean, color = "Black Reweighted"), linetype = "dashed") +
  geom_point(data = means_all, aes(x = year, y = black_reweighted_mean, color = "Black Reweighted")) +
  geom_ribbon(data = means_all, aes(x = year, ymin = ci_lower_black_obs, ymax = ci_upper_black_obs), fill = "midnightblue", alpha = 0.2) +
  geom_ribbon(data = means_all, aes(x = year, ymin = ci_lower_white_obs, ymax = ci_upper_white_obs), fill = "mediumvioletred", alpha = 0.2) +
  geom_ribbon(data = means_all, aes(x = year, ymin = ci_lower_black_rew, ymax = ci_upper_black_rew), fill = "grey7", alpha = 0.2) +
  geom_text(data = means_all, aes(x = year, y = black_observed_mean, label = label_black_observed, color = "Black Observed"), 
            hjust = -0.1, size = 4) +
  geom_text(data = means_all, aes(x = year, y = white_observed_mean, label = label_white_observed, color = "White Observed"), 
            hjust = -0.1, size = 4) +
  geom_text(data = means_all, aes(x = year, y = black_reweighted_mean, label = label_black_reweighted, color = "Black Reweighted"), 
            hjust = -0.1, size = 4) +
  labs(title = "Observed and Reweighted Means by Year", 
       x = "Year", 
       y = "Mean Testing Rate") +
  scale_color_manual(values = c("Black Observed" = "midnightblue", 
                                "White Observed" = "mediumvioletred", 
                                "Black Reweighted" = "grey7")) +
  theme_minimal() + 
  ylim(0, 0.3) + 
  xlim(2018.9, 2023.75) +  # Extended x-axis limit to allow space for labels
  theme(legend.position = "none", 
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        plot.title = element_text(size = 18, face = "bold"),
        panel.grid = element_blank(),
        axis.line = element_line())

# Save the plot
ggsave(here("results", "observed_reweighted_means_labeled.pdf"), plot, width = 8, height = 6, units = "in", dpi = 300)
