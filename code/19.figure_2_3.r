#===============================================================================
# Figures 2 & 3: Testing by mother's history of SUD
# MKD
# May 30 2024

#===============================================================================

# Packages
if(!require(pacman)) install.packages("pacman")
library(pacman)
p_load(data.table, lubridate, fixest, modelsummary, collapse, ggplot2, dplyr, gridExtra, grid, here)
rm(list = ls())

# Output directory
output_dir <- here("results")

# Read in the data
dt <- fread(here("data", "outputs", "final_analytic_sample_with_infant_char_for_reg.csv"))

dt[, race_recode := case_when(RACE == "WHITE" ~ "White",
                                        RACE == "BLACK OR AFRICAN AMERICAN" ~ "Black",
                                        RACE == "ASIAN/PACIFIC ISLANDER" ~ "Asian",
                                        TRUE ~ "other")]

# Testing rates by prior SUD
sud_collapse <- dt[, .(.N, tested = sum(tested), positive_test = sum(positive_test)), by = c("any_sud", "year", "race_recode")][order(year)]
sud_collapse[, testing_rate := tested/N]
sud_collapse[, positive_rate := positive_test/tested]
sud_collapse[, any_sud := as.factor(any_sud)]

# Confidence intervals 
sud_collapse[, ci_testing := 1.96 * sqrt(testing_rate * (1 - testing_rate) / N), by = c("any_sud", "year", "race_recode")]
sud_collapse[, ci_positive := 1.96 * sqrt(positive_rate * (1 - positive_rate) / N), by = c("any_sud", "year", "race_recode")]

# Bounds 
sud_collapse[, ci_upper_bound_testing := testing_rate + ci_testing]
sud_collapse[, ci_lower_bound_testing := testing_rate - ci_testing]
sud_collapse[, ci_upper_bound_positive := positive_rate + ci_positive]
sud_collapse[, ci_lower_bound_positive := positive_rate - ci_positive]

# Plot the testing rates by race for SUD=0 with confidence intervals
  # Create a subset of the data with the last data point for each race group
last_points <- sud_collapse %>%
  filter(race_recode %in% c("White", "Black"), any_sud == 0) %>%
  group_by(race_recode) %>%
  filter(year == max(year))
  
last_points <- last_points %>%
  mutate(testing_rate = case_when(
    race_recode == "White" ~ testing_rate + 0.01,
    race_recode == "Black" ~ testing_rate  ))

# Filter the data for plotting
data_filtered <- sud_collapse %>%
  filter(any_sud == 0, race_recode %in% c("White", "Black"))

# Create the plot
plot_test_sud0 <- 
  ggplot() + 
  geom_point(data = data_filtered, aes(x = year, y = testing_rate, color = race_recode, group = race_recode)) +
  geom_line(data = data_filtered, aes(x = year, y = testing_rate, color = race_recode, group = race_recode)) +
  geom_ribbon(data = data_filtered, aes(x = year, ymin = ci_lower_bound_testing, ymax = ci_upper_bound_testing, fill = race_recode), alpha = 0.2) + 
  geom_text(data = last_points, aes(x = year, y = testing_rate, label = race_recode, color = race_recode), 
            hjust = -0.1, size = 5) +  # Adjust hjust to position the label
  theme_minimal() +
  ylim(0, 0.6) + 
  xlim(2018.9, 2023.40) +
  labs(title = "No Maternal Substance Use History", x = "", y = "Testing Rate") + 
  theme(legend.position = "none", 
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        plot.title = element_text(size = 18, face = "bold"),
        panel.grid = element_blank(),  # Remove gridlines
        panel.border = element_blank(),  # Remove panel border
        axis.line = element_line()) +  # Add axis lines
  scale_color_manual(values = c("White" = "mediumvioletred", "Black" = "midnightblue")) +
  scale_fill_manual(values = c("White" = "mediumvioletred", "Black" = "midnightblue"))

ggsave(file.path(output_dir, "test_rate_by_race_sud0.pdf"), plot = plot_test_sud0, height = 5, width = 8)

# Plot the testing rates by race for SUD=1 with confidence intervals
  # Create a subset of the data with the last data point for each race group
  last_points2 <- sud_collapse %>%
    filter(race_recode %in% c("White", "Black"), any_sud == 1) %>%
    group_by(race_recode) %>%
    filter(year == max(year))
  
  last_points <- last_points %>%
    mutate(testing_rate = case_when(
      race_recode == "White" ~ testing_rate + 0.01,
      race_recode == "Black" ~ testing_rate,
    ))

# Filter the data for plotting
  data_filtered2 <- sud_collapse %>%
    filter(any_sud == 1, race_recode %in% c("White", "Black"))

# Create the plot
plot_test_sud1 <- 
  ggplot() + 
  geom_point(data = data_filtered2, aes(x = year, y = testing_rate, color = race_recode, group = race_recode)) +
  geom_line(data = data_filtered2, aes(x = year, y = testing_rate, color = race_recode, group = race_recode)) +
  geom_ribbon(data = data_filtered2, aes(x = year, ymin = ci_lower_bound_testing, ymax = ci_upper_bound_testing, fill = race_recode), alpha = 0.2) + 
  geom_text(data = last_points2, aes(x = year, y = testing_rate, label = race_recode, color = race_recode), 
            hjust = -0.1, size = 5) +  # Adjust hjust to position the label
  theme_minimal() +
  ylim(0, 0.6) + 
  xlim(2018.9, 2023.40) +
  labs(title = "Maternal Substance Use History", x = "", y = "Testing Rate") + 
  theme(legend.position = "none", 
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        plot.title = element_text(size = 18, face = "bold"),
        panel.grid = element_blank(),  # Remove gridlines
        panel.border = element_blank(),  # Remove panel border
        axis.line = element_line()) +  # Add axis lines
  scale_color_manual(values = c("White" = "mediumvioletred", "Black" = "midnightblue")) +
  scale_fill_manual(values = c("White" = "mediumvioletred", "Black" = "midnightblue"))
ggsave(file.path(output_dir, "test_rate_by_race_sud1.pdf"), plot = plot_test_sud1, height = 5, width = 8)

title_test <- textGrob("Testing Rates by Racial Group of Infant and Maternal Substance Use History", gp = gpar(fontsize = 20, fontface = "bold"))
testing_sud <- grid.arrange(plot_test_sud0, plot_test_sud1, ncol = 2, top = title_test)
ggsave(file.path(output_dir, "testing_rate_sud_history.pdf"), plot = testing_sud, height = 5, width = 16)

# Plot the positive testing rates by race and SUD=0 with confidence intervals
# Plot the testing rates by race for SUD=0 with confidence intervals
  # Create a subset of the data with the last data point for each race group
last_points <- sud_collapse %>%
  filter(race_recode %in% c("White", "Black"), any_sud == 0) %>%
  group_by(race_recode) %>%
  filter(year == max(year))
  
last_points <- last_points %>%
  mutate(positive_rate = case_when(
    race_recode == "White" ~ positive_rate + 0.01,
    race_recode == "Black" ~ positive_rate - 0.01))

# Filter the data for plotting
data_filtered <- sud_collapse %>%
  filter(any_sud == 0, race_recode %in% c("White", "Black"))

# Create the plot
plot_pos_sud0 <- 
  ggplot() + 
  geom_point(data = data_filtered, aes(x = year, y = positive_rate, color = race_recode, group = race_recode)) +
  geom_line(data = data_filtered, aes(x = year, y = positive_rate, color = race_recode, group = race_recode)) +
  geom_ribbon(data = data_filtered, aes(x = year, ymin = ci_lower_bound_positive, ymax = ci_upper_bound_positive, fill = race_recode), alpha = 0.2) + 
  geom_text(data = last_points, aes(x = year, y = positive_rate, label = race_recode, color = race_recode), 
            hjust = -0.1, size = 5) +  # Adjust hjust to position the label
  theme_minimal() +
  ylim(0, 0.6) + 
  xlim(2018.9, 2023.40) +
  labs(title = "No Maternal Substance Use History", x = "", y = "Positive Testing Rate") + 
  theme(legend.position = "none", 
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        plot.title = element_text(size = 18, face = "bold"),
        panel.grid = element_blank(),  # Remove gridlines
        panel.border = element_blank(),  # Remove panel border
        axis.line = element_line()) +  # Add axis lines
  scale_color_manual(values = c("White" = "mediumvioletred", "Black" = "midnightblue")) +
  scale_fill_manual(values = c("White" = "mediumvioletred", "Black" = "midnightblue"))
ggsave(file.path(output_dir, "positive_rate_by_race_sud0.pdf"), plot = plot_pos_sud0, height = 5, width = 8)

# Plot the positive testing rates for SUD 1 
  # Create a subset of the data with the last data point
  last_points <- sud_collapse %>%
  filter(race_recode %in% c("White", "Black"), any_sud == 1) %>%
  group_by(race_recode) %>%
  filter(year == max(year))
  
last_points <- last_points %>%
  mutate(positive_rate = case_when(
    race_recode == "White" ~ positive_rate - 0.02,
    race_recode == "Black" ~ positive_rate + 0.01))

# Filter the data for plotting
data_filtered <- sud_collapse %>%
  filter(any_sud == 1, race_recode %in% c("White", "Black"))

# Create the plot
plot_pos_sud1 <- 
  ggplot() + 
  geom_point(data = data_filtered, aes(x = year, y = positive_rate, color = race_recode, group = race_recode)) +
  geom_line(data = data_filtered, aes(x = year, y = positive_rate, color = race_recode, group = race_recode)) +
  geom_ribbon(data = data_filtered, aes(x = year, ymin = ci_lower_bound_positive, ymax = ci_upper_bound_positive, fill = race_recode), alpha = 0.2) + 
  geom_text(data = last_points, aes(x = year, y = positive_rate, label = race_recode, color = race_recode), 
            hjust = -0.1, size = 5) +  # Adjust hjust to position the label
  theme_minimal() +
  ylim(0, 0.6) + 
  xlim(2018.9, 2023.40) +
  labs(title = "Maternal Substance Use History", x = "", y = "Positive Testing Rate") + 
  theme(legend.position = "none", 
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        plot.title = element_text(size = 18, face = "bold"),
        panel.grid = element_blank(),  # Remove gridlines
        panel.border = element_blank(),  # Remove panel border
        axis.line = element_line()) +  # Add axis lines
  scale_color_manual(values = c("White" = "mediumvioletred", "Black" = "midnightblue")) +
  scale_fill_manual(values = c("White" = "mediumvioletred", "Black" = "midnightblue"))
ggsave(file.path(output_dir, "positive_rate_by_race_sud1.pdf"), plot = plot_pos_sud1, height = 5, width = 8)

title_pos <- textGrob("Positive Testing Rates by Racial Group of Infant and Maternal Substance Use History", gp = gpar(fontsize = 20, fontface = "bold"))
positive_sud <- grid.arrange(plot_pos_sud0, plot_pos_sud1, ncol = 2, top = title_pos)
ggsave(file.path(output_dir, "positive_rate_sud_history.pdf"), plot = positive_sud, height = 5, width = 16)

# Create titles as text grobs
# Arrange the plots with titles
grid_sud_test <- grid.arrange(
  title_test,
  grid.arrange(plot_test_sud0, plot_test_sud1, ncol = 2),
  nrow = 2,
  heights = c(0.1, 0.9)  # Allocate 10% of height for title and 90% for plots
)

grid_sud_pos <- grid.arrange(
  title_pos,
  grid.arrange(plot_pos_sud0, plot_pos_sud1, ncol = 2),
  nrow = 2,
  heights = c(0.1, 0.9)  # Allocate 10% of height for title and 90% for plots
)

grid_sud <- grid.arrange(grid_sud_test, grid_sud_pos, nrow = 2)

ggsave(file.path(output_dir, "testing_sud_race_year.pdf"), plot = grid_sud, height = 15, width = 20)
