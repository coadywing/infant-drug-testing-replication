#===============================================================================
# Script 28: Propensity Score Overlap (Appendix Figures A6-A7)
#
# Check overlap in propensity score weights between Black and White infants
# MKD
# August 2024
#===============================================================================

# Packages
if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(data.table, fixest, lubridate, ggplot2, boot, pbapply, xtable, here)
rm(list = ls())

# Output directory
output_dir <- here("results")

# Load data
dt <- fread(here("data", "outputs", "final_analytic_sample_with_infant_char_for_reg.csv"))

# Calculate the positive test rate for balck and white 
dt[black == 1, sum(positive_test)/sum(tested)]
dt[black == 0, sum(positive_test)/sum(tested)]

### Propensity Score, Inverse Probability Weighting ### 
# Create a propensity score model 
propensity_model <- glm(black ~ age_bin20 + age_bin25 + age_bin30 + age_bin35 + age_bin40 + age_bin45 + 
                          above_avg_nas1 + any_sud + any_prenatal + above_avg_mort + any_ment_hlth + 
                          any_hypt + medicaid + other_ins + lbw + premature + four_plus_encs + mom_black + mom_other, data = dt, family = "binomial")

# Calculate the propensity score
dt[, propensity_score := predict(propensity_model, type = "response")]

# Calculate the weights 
# If white, ATT weight is 1
# If black, ATT weight is ps / (1 - ps)
dt[, ATT_weight := ifelse(black == 1, 1, propensity_score / (1 - propensity_score))]
dt[, ATT_weight_white := ifelse(black == 0, 1, (1 - propensity_score)/propensity_score)]

# Sum up the weights for blacks only 
total_black_weight <- dt[black == 1, sum(ATT_weight_white)]

# Calculate the total fraction of the weights that each person contributes
dt[black == 1, frac_black_weight := ATT_weight_white / total_black_weight]

# Plot the distribution of the fraction of the weights
ggplot() +
  geom_histogram(data = dt[black == 1], aes(x = frac_black_weight), fill = "navy") +
  theme_minimal() +
  labs(title = "Distribution of Fraction of Total Weight: Black Infants Reweighted to Be Like White", x = "Fraction of Total Weight", y = "Count") +
  theme(legend.position = "none")
ggsave(file.path(output_dir, "weight_distribution.pdf"), width = 8, height = 6, units = "in")

# # Count the number of individuals by ATT_weight_white_frac
# dt[, .N, by = frac_black_weight][order(-frac_black_weight)]
# # Max weight fraction is 0.002880558
# max_frac_weight <- dt[black == 1, max(frac_black_weight)]
# min_frac_weight <- dt[black == 1, min(frac_black_weight)]
# median_frac_weight <- dt[black == 1, median(frac_black_weight)]
# mean_frac_weight <- dt[black == 1, mean(frac_black_weight)]
# # Get the associated weights 
# max_weight <- dt[frac_black_weight == max_frac_weight, ATT_weight_white][1]
# min_weight <- dt[frac_black_weight == min_frac_weight, ATT_weight_white][1]

# table_weights <- data.table(
#   Measure = c("Frac", "Weight"),
#   Minimum = c(round(min_frac_weight, 6), round(min_weight, 6)),
#   Maximum = c(round(max_frac_weight, 6), round(max_weight, 6)),
#   Median = c(round(median_frac_weight, 6), NA),
#   Mean = c(round(mean_frac_weight, 6), NA)
# )


# # Set Measure as the row names and remove it as a column
# setnames(table_weights, "Measure", " ")
# table_weights_latex <- xtable(table_weights, 
#                               caption = "Summary of Fraction Weights and Associated ATT Weights", 
#                               label = "tab:weights_summary")

# # Print the LaTeX code for the table
# print(table_weights_latex, include.rownames = FALSE, file = "infant_drugs/results/table_weights.tex")

# # Look for overlap in the propensity scores 
# ggplot(dt, aes(x = propensity_score, fill = factor(black))) + 
#   geom_density(alpha = 0.5) + 
#   theme_minimal() + 
#   labs(title = "Distribution of Propensity Scores",
#        x = "Propensity Score",
#        y = "Density",
#        fill = "") +
#   scale_fill_manual(values = c("mediumvioletred", "midnightblue"), labels = c("White", "Black"))
# ggsave("infant_drugs/results/propensity_score_distribution.pdf", width = 6, height = 4, units = "in")

# Bin the propensity scores in increments of 0.1 and look for overlap
dt[, propensity_bin01 := cut(propensity_score, breaks = seq(0, 1, by = 0.01))]
overlap01 <- dt[, .N, by = c("propensity_bin01", "black")][order(-propensity_bin01)]
dt[, propensity_bin02 := cut(propensity_score, 
                                  breaks = seq(0, 1, by = 0.02), 
                                  include.lowest = TRUE, 
                                  right = TRUE, 
                                  labels = seq(0, 0.98, by = 0.02))]
overlap02 <- dt[, .N, by = c("propensity_bin02", "black")][order(-propensity_bin02)]


# calculate the proportion in each bin by race 
white_total <- sum(overlap01[black == 0, N])
black_total <- sum(overlap01[black == 1, N])
overlap01[black == 1, prop := N / black_total]
overlap01[black == 0, prop := N / white_total]

overlap02[black == 1, prop := N / black_total]
overlap02[black == 0, prop := N / white_total]

overlap01[, black := factor(black, labels = c("White", "Black"))]
overlap02[, black := factor(black, labels = c("White", "Black"))]

# sum the proportions by race 
overlap02[, .(prop = sum(prop)), by = black]

overlap02[, propensity_bin02 := as.numeric(as.character(propensity_bin02))]

# Plot the proportion with different sizes for solid and hollow dots
# Adjust point sizes for solid (White) and hollow (Black) dots
ggplot() + 
  geom_point(data = overlap02[black == "White"], 
             aes(x = propensity_bin02, y = prop, col = black), 
             size = 2, shape = 19) +   # Smaller size for solid dots (White)
  geom_point(data = overlap02[black == "Black"], 
             aes(x = propensity_bin02, y = prop, shape = black, col = black), 
             size = 4, shape = 1) +   # Larger size for hollow dots (Black)
  theme_bw() + 
  labs(title = "Overlap in Propensity Scores",
       x = "Propensity Score Bin",
       y = "Proportion",
       col = "Race",
       shape = "Race") +  
  scale_color_manual(values = c("mediumvioletred", "midnightblue")) +  # Pink for white, blue for black
  theme(legend.title = element_blank())
ggsave(file.path(output_dir, "propensity_score_overlap_proportion_bin02.pdf"), width = 6, height = 4, units = "in")

# Save the overlap data
write.csv(overlap02, file.path(output_dir, "propensity_score_overlap_proportion_bin02.csv"), row.names = FALSE)

# Plot the overlap
ggplot(overlap02, aes(x = propensity_bin02, y = N, fill = factor(black))) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Overlap in Propensity Scores",
       x = "Propensity Score Bin",
       y = "Count",
       fill = "Black") +
  scale_fill_manual(values = c("mediumvioletred", "midnightblue"), labels = c("White", "Black"))
ggsave(file.path(output_dir, "propensity_score_overlap.pdf"), width = 6, height = 4, units = "in")

