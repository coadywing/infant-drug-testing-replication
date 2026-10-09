#===============================================================================
# Figure 4: Geographic variation and testing (NAS incidence in 2019)
# Figure 4 shows the relationship between county NAS incidence and testing by county 
# This will also create the appendix figure to show the relationship between NAS 
# And positive test rates which is Figure A1
# MKD
# May 30, 2024
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

dt[, NAS_rate_100 := NAS1_rate * 100]

# Create the race_recode variable
dt[, race_recode := case_when(RACE == "WHITE" ~ "White",
                                        RACE == "BLACK OR AFRICAN AMERICAN" ~ "Black",
                                        RACE == "ASIAN/PACIFIC ISLANDER" ~ "Asian",
                                        TRUE ~ "other")]

# Create a table that is the number of births by county by year 
births_by_cty_yr <- dt[, .N, by = .(COUNTY, year)][order(-N)]
births_by_cty_yr_avg <- dt[, .N, by = .(COUNTY, year)][order(-N)][, .(avg_births = mean(N)), by = COUNTY]

# Check the geographic variation in NAS rates and testing 
dt[, tested := ifelse(total_tests > 0, 1, 0)]
dt[is.na(tested), tested := 0]

dt2 <- dt[, .(tested = sum(tested), total_infants = .N, positive_tests = sum(positive_test)), by = .(COUNTY, race_recode, NAS_rate_100, pop_201418)]
dt2[, avg_pop := round(pop_201418/5)]

dt2 <- merge(dt2, births_by_cty_yr_avg, by = "COUNTY", all.x = TRUE)

# County level testing rates 
dt2[, ":=" (county_tested = tested/total_infants, county_positive = positive_tests/tested)]
dt2[is.na(county_tested), county_tested := 0]
dt2[is.na(county_positive), county_positive := 0]

# Reshape wide 
dt2_wide <- dcast(dt2, COUNTY + NAS_rate_100 + avg_pop + avg_births ~ race_recode, value.var = c("county_tested", "county_positive"))
dt2_wide[is.na(dt2_wide)] <- 0

# Plot the relationship between NAS and testing 
# Fit a weighted linear model
fit <- lm(county_tested_White ~ NAS_rate_100, data = dt2_wide, weights = avg_births)

# Extract the coefficients
intercept <- coef(fit)[1]
slope <- coef(fit)[2]

# Plot the relationship between NAS and testing
white_test <- ggplot(data = dt2_wide, aes(x = NAS_rate_100, y = county_tested_White, size = avg_births)) + 
    geom_point(col = "mediumvioletred") + 
    theme_minimal() +
    labs(title = "White", x = "NAS Incidence per 100 Births", y = "Testing Rate") +
    geom_abline(intercept = intercept, slope = slope, col = "mediumvioletred") +  # Add the regression line
    ylim(0, 1) + 
    annotate("text", x = 4.5, y = 0.75, label = paste("Coefficient: ", round(slope, 3))) + 
    theme(legend.position = "none", 
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          plot.title = element_text(size = 18, face = "bold"),
          panel.grid = element_blank(),  # Remove gridlines
          panel.border = element_blank(),  # Remove panel border
          axis.line = element_line())  # Add axis lines
ggsave(file.path(output_dir, "NAS_county_tested_white_weightbirths.pdf"), plot = white_test, height = 5, width = 6)

# Black 
# Fit the model
fit <- lm(county_tested_Black ~ NAS_rate_100, data = dt2_wide, weights = avg_births)

# Extract the coefficients
intercept <- coef(fit)[1]
slope <- coef(fit)[2]

# Plot the relationship between NAS and testing
black_test <- ggplot() + 
    geom_point(data = dt2_wide, aes(x = NAS_rate_100, y = county_tested_Black, size = avg_births), col = "midnightblue") +
    theme_minimal()+
    labs(title = "Black", x = "NAS Incidence per 100 Births", y = "Testing Rate") +
    geom_abline(intercept = intercept, slope = slope, col = "midnightblue") +  # Add the regression line
    ylim(0, 1) +
    annotate("text", x = 4.5, y = 0.75, label = paste("Coefficient: ", round(slope, 3))) + 
    theme(legend.position = "none", 
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          plot.title = element_text(size = 18, face = "bold"),
          panel.grid = element_blank(),  # Remove gridlines
          panel.border = element_blank(),  # Remove panel border
          axis.line = element_line())  # Add axis lines
ggsave(file.path(output_dir, "NAS_county_tested_black_weightbirths.pdf"), plot = black_test, height = 5, width = 6)

# Grid arrange and add a title that is "Relationship between County Testing Rates by Race and NAS Incidence"
title_test <- textGrob("Relationship between County Testing Rates by Racial Group of Infant and NAS Incidence", gp = gpar(fontsize = 20, fontface = "bold"))
all_test <- grid.arrange(white_test, black_test, ncol = 2, top = title_test)
ggsave(file.path(output_dir, "NAS_county_tested_all_weightbirths.pdf"), plot = all_test, height = 5, width = 14)

# Positive test rates
# Fit a weighted linear model
fit <- lm(county_positive_White ~ NAS_rate_100, data = dt2_wide, weights = avg_births)

# Extract the coefficients
intercept <- coef(fit)[1]
slope <- coef(fit)[2]

# Plot the relationship between mortality and positive testing
white_positive <- ggplot() + 
    geom_point(data = dt2_wide, aes(x = NAS_rate_100, y = county_positive_White, size = avg_births), col = "mediumvioletred") +
    theme_minimal()+
    labs(title = "White", x = "NAS Incidence per 100 Births", y = "Positive Test Rate") +
    geom_abline(intercept = intercept, slope = slope, col = "mediumvioletred") +  # Add the regression line
    ylim(0, 1) +
    annotate("text", x = 4.5, y = 0.75, label = paste("Coefficient: ", round(slope, 3))) + 
    theme(legend.position = "none", 
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          plot.title = element_text(size = 18, face = "bold"),
          panel.grid = element_blank(),  # Remove gridlines
          panel.border = element_blank(),  # Remove panel border
          axis.line = element_line())  # Add axis lines
ggsave(file.path(output_dir, "NAS_county_positive_white_weightbirths.pdf"), plot = white_positive, height = 5, width = 6)

# Black
# Fit the model
fit <- lm(county_positive_Black ~ NAS_rate_100, data = dt2_wide, weights = avg_births)

# Extract the coefficients
intercept <- coef(fit)[1]
slope <- coef(fit)[2]

# Plot the relationship between NAS and positive testing
black_positive <- ggplot() + 
    geom_point(data = dt2_wide, aes(x = NAS_rate_100, y = county_positive_Black, size = avg_births), col = "midnightblue") +
    theme_minimal()+
    labs(title = "Black", x = "NAS Incidence per 100 Births", y = "Positive Test Rate") +
    geom_abline(intercept = intercept, slope = slope, col = "midnightblue") +  # Add the regression line
    ylim(0, 1) +
    annotate("text", x = 4.5, y = 0.75, label = paste("Coefficient: ", round(slope, 3))) + 
    theme(legend.position = "none", 
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          plot.title = element_text(size = 18, face = "bold"),
          panel.grid = element_blank(),  # Remove gridlines
          panel.border = element_blank(),  # Remove panel border
          axis.line = element_line())  # Add axis lines
ggsave(file.path(output_dir, "NAS_county_positive_black_weightbirths.pdf"), plot = black_positive, height = 5, width = 6)

# Grid arrange and add a title that is "Relationship between County Positive Test Rate by Race and NAS Incidence"
title_pos <- textGrob("Relationship between County Positive Test Rate by Racial Group of Infant and NAS Incidence", gp = gpar(fontsize = 20, fontface = "bold"))
all_positive <- grid.arrange(white_positive, black_positive, ncol = 2, top = title_pos)
ggsave(file.path(output_dir, "NAS_county_positive_all_weightbirths.pdf"), plot = all_positive, height = 5, width = 14)

