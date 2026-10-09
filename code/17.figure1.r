#===============================================================================
# Script 17: Figures 1 (panels)
#
# Figure 1: Race test rate and positive test rate by year
# MKD
#===============================================================================

# Packages
if(!require(pacman)) install.packages("pacman")
library(pacman)
p_load(data.table, lubridate, fixest, modelsummary, collapse, ggplot2, dplyr, gridExtra, here)

rm(list = ls())

# Output directory
output_dir <- here("results")

# Color schemes for different race combinations
colors_2race <- c("White" = "mediumvioletred", "Black" = "midnightblue")
colors_3race <- c("White" = "#374E66FF", "Black" = "#00A1D5FF", "Asian" = "#DF8F44FF")

# Plot theme
plot_theme_2race <- theme(
  legend.position = "none",
  legend.text = element_text(size = 16),
  axis.title = element_text(size = 16),
  axis.text = element_text(size = 14),
  plot.title = element_text(size = 18, face = "bold"),
  panel.grid = element_blank(),
  panel.border = element_blank(),
  axis.line = element_line()
)

plot_theme_3race <- theme(
  legend.position = "none",
  axis.title = element_text(size = 14),
  axis.text = element_text(size = 12),
  plot.title = element_text(size = 14, face = "bold"),
  panel.grid = element_blank(),
  panel.border = element_blank(),
  axis.line = element_line()
)

# ============================================================================
# Read in the data
# ============================================================================
dt <- fread(here("data", "outputs", "final_analytic_sample_with_infant_char_for_reg.csv"))
fnunique(dt)

# Helper function to calculate confidence intervals
calculate_ci <- function(dt, rate_col, n_col) {
  z_score <- 1.96
  ci <- z_score * sqrt(dt[[rate_col]] * (1 - dt[[rate_col]]) / dt[[n_col]])
  upper <- dt[[rate_col]] + ci
  lower <- dt[[rate_col]] - ci
  list(ci = ci, upper = upper, lower = lower)
}

# ============================================================================
# Overall testing rates by year
# ============================================================================
year_collapse <- dt[, .(.N, tested = sum(tested), positive_test = sum(positive_test)), by = year]
year_collapse[, testing_rate := tested / N]
year_collapse[, positive_rate := positive_test / tested]

# Calculate confidence intervals
ci_testing <- calculate_ci(year_collapse, "testing_rate", "N")
year_collapse[, `:=`(ci_testing = ci_testing$ci,
                     ci_upper_bound_testing = ci_testing$upper,
                     ci_lower_bound_testing = ci_testing$lower)]

ci_positive <- calculate_ci(year_collapse, "positive_rate", "N")
year_collapse[, `:=`(ci_positive = ci_positive$ci,
                     ci_upper_bound_positive = ci_positive$upper,
                     ci_lower_bound_positive = ci_positive$lower)]

# Plot the testing rates by year with confidence intervals
plot_year <- ggplot() +
    geom_line(data = year_collapse, aes(x = year, y = testing_rate, color = "Testing Rate")) +
    geom_line(data = year_collapse, aes(x = year, y = positive_rate, color = "Positive Test Rate")) +
    geom_ribbon(data = year_collapse, aes(x = year, ymin = ci_lower_bound_testing, ymax = ci_upper_bound_testing, fill = "Testing Rate"), alpha = 0.3) +
    geom_ribbon(data = year_collapse, aes(x = year, ymin = ci_lower_bound_positive, ymax = ci_upper_bound_positive, fill = "Positive Test Rate"), alpha = 0.3) +
    labs(title = "Testing Rates ", x = "Year", y = "Rate") +
    theme_bw() +
    ylim(0, 0.5) +
    scale_fill_manual(values = c("Testing Rate" = "navy", "Positive Test Rate" = "red"),
                      guide = guide_legend(title = "Rate Type")) +
    scale_color_manual(values = c("Testing Rate" = "navy", "Positive Test Rate" = "red"),
                       guide = guide_legend(title = "Rate Type"))
ggsave(file.path(output_dir, "test_rate_by_year.pdf"))

# Plot the frequency of testing by year and race
# Recode race to be black, white, other
dt[, race_recode := case_when(RACE == "WHITE" ~ "White",
                                        RACE == "BLACK OR AFRICAN AMERICAN" ~ "Black",
                                        RACE == "ASIAN/PACIFIC ISLANDER" ~ "Asian",
                                        TRUE ~ "other")]

# Frequency by race, year
year_race_collapse <- dt[, .(.N, tested = sum(tested), positive_test = sum(positive_test)), by = c("year", "race_recode")][order(year)]
year_race_collapse[, testing_rate := tested/N]
year_race_collapse[, positive_rate := positive_test/tested]

# Confidence intervals
year_race_collapse[, ci_testing := 1.96 * sqrt(testing_rate * (1 - testing_rate)/N)]
year_race_collapse[, ci_positive := 1.96 * sqrt(positive_rate * (1 - positive_rate)/N)]

# Bounds
year_race_collapse[, ci_upper_bound_testing := testing_rate + ci_testing]
year_race_collapse[, ci_lower_bound_testing := testing_rate - ci_testing]
year_race_collapse[, ci_upper_bound_positive := positive_rate + ci_positive]
year_race_collapse[, ci_lower_bound_positive := positive_rate - ci_positive]

colors2 <- c("White" = "brown2", "Black" = "navy")

# Plotting testing rates by year and race with confidence intervals
  # Create a subset of the data with the last data point for each race group
  last_points <- year_race_collapse %>%
    filter(race_recode %in% c("White", "Black")) %>%
    group_by(race_recode) %>%
    filter(year == max(year))

# Plot the testing
plot_test_race <-
  ggplot() +
  geom_point(data = year_race_collapse[year_race_collapse$race_recode %in% c("White", "Black")],
             aes(x = year, y = testing_rate, color = race_recode, group = race_recode)) +
  geom_line(data = year_race_collapse[year_race_collapse$race_recode %in% c("White", "Black")],
            aes(x = year, y = testing_rate, color = race_recode, group = race_recode)) +
  geom_ribbon(data = year_race_collapse[year_race_collapse$race_recode %in% c("White", "Black")],
              aes(x = year, ymin = ci_lower_bound_testing, ymax = ci_upper_bound_testing, fill = race_recode),
              alpha = 0.2) +  # Add confidence intervals
  geom_text(data = last_points, aes(x = year, y = testing_rate, label = race_recode, color = race_recode),
            hjust = -0.1, size = 5) +  # Adjust hjust to position the label
  theme_minimal() +
  ylim(0, 0.3) +
  xlim(2018.9, 2023.25) +
  labs(title = "Testing Rates by Racial Group of Infant",
       x = "",
       y = "Testing Rate",
       color = "Race",
       fill = "Race") +
  theme(legend.position = "none",
        legend.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        plot.title = element_text(size = 18, face = "bold"),
        panel.grid = element_blank(),  # Remove gridlines
        panel.border = element_blank(),  # Remove panel border
        axis.line = element_line()) +  # Add axis lines
  scale_color_manual(values = c("White" = "mediumvioletred", "Black" = "midnightblue")) +
  scale_fill_manual(values = c("White" = "mediumvioletred", "Black" = "midnightblue"))
ggsave(file.path(output_dir, "test_rate_by_race2.pdf"), plot = plot_test_race, width = 8, height = 5)

# Plot the positive testing rates by year and race
  # Create a subset of the data with the last data point for each race group
  last_points2 <- year_race_collapse %>%
    filter(race_recode %in% c("White", "Black")) %>%
    group_by(race_recode) %>%
    filter(year == max(year))
    # Add a small offset to the y-values for the labels to prevent overlap
    last_points2 <- last_points2 %>%
    mutate(positive_rate = ifelse(race_recode == "White", positive_rate + 0.02, positive_rate - 0.01))


plot_pos_race <-
  ggplot() +
  geom_point(data = year_race_collapse[year_race_collapse$race_recode %in% c("White", "Black")], aes(x = year, y = positive_rate, color = race_recode, group = race_recode)) +
  geom_line(data = year_race_collapse[year_race_collapse$race_recode %in% c("White", "Black")], aes(x = year, y = positive_rate, color = race_recode, group = race_recode)) +
  geom_ribbon(data = year_race_collapse[year_race_collapse$race_recode %in% c("White", "Black")], aes(x = year, ymin = ci_lower_bound_positive, ymax = ci_upper_bound_positive, fill = race_recode), alpha = 0.2) +  # Add confidence intervals
  geom_text(data = last_points2, aes(x = year, y = positive_rate, label = race_recode, color = race_recode),
            hjust = -0.1, size = 5) +  # Adjust hjust to position the label
  theme_bw() +
  ylim(0, 0.3) +
  xlim(2018.9, 2023.25) +
  labs(title = "Positive Testing Rates by Racial Group of Infant",
       x = "",
       y = "Positive Testing Rate",
       color = "Race",
       fill = "Race") +
  theme(legend.position = "none",  # Remove legend
        legend.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        plot.title = element_text(size = 18, face = "bold"),
        panel.grid = element_blank(),  # Remove gridlines
        panel.border = element_blank(),  # Remove panel border
        axis.line = element_line()) +  # Add axis lines
  scale_color_manual(values = c("White" = "mediumvioletred", "Black" = "midnightblue")) +
  scale_fill_manual(values = c("White" = "mediumvioletred", "Black" = "midnightblue"))
ggsave(file.path(output_dir, "positive_test_rate_by_race2.pdf"), width = 8, height = 5)

grid_race <- grid.arrange(plot_test_race, plot_pos_race, ncol = 2)
ggsave(file.path(output_dir, "testing_race_year_race2.pdf"), plot = grid_race, width = 16, height = 5)



