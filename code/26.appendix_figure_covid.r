#===============================================================================
# Script 26: COVID-19 Positive Test Rate and Infant Drug Testing (Appendix Figures A4-5)
#
# Infant drug testing rate / positive rate by race correlated with
# geographic COVID-19 positive test rate level
# MKD
#===============================================================================

# Packages
if(!require(pacman)) install.packages("pacman")
library(pacman)
p_load(data.table, lubridate, fixest, modelsummary, collapse, ggplot2, dplyr, gridExtra, grid, here)
rm(list = ls())
source(here("code", "00_paths.r"))

# External data paths
input_covid_labs <- file.path(path_inpc_csv_dir, "covid_labs.sas7bdat.csv")
input_demographics <- file.path(path_inpc_csv_dir, "demo.sas7bdat.csv")
# Output directory
output_dir <- here("results")

# Files in convert_sas/data_csv
list.files(dirname(input_covid_labs))

# Read in the COVID lab data
covid <- fread(input_covid_labs)

# Convert all LAB_RESULT to lowercase 
covid[, LAB_RESULT := tolower(LAB_RESULT)]

# Positive lab results are 
pos <- c("detected", "yes", "positive", "detected (qualifier value)", "symptomatic or exposure", "detected\ndetected", "positive for detection of sars-cov-2 virus", 
            "positive\npositive", "abnormal", "detected(qualifier value)", "positive (qualifier value)", "positive sars-cov-2", "yes\nyes")

covid[, positive := ifelse(LAB_RESULT %in% pos, 1, 0)]
covid[positive == 0, .N, by = LAB_RESULT][order(-N)][120:180]
covid[, test := 1]

# What are the dates for the data ? 2020-2023
covid[, .N, by = LAB_DATE][order(LAB_DATE)]

# Limit to STUDY ID, test, positive 
covid <- covid[, .(STUDY_ID, test, positive)]

# Read in the demographics data
demo <- fread(input_demographics)

# Limit to one row per study ID in demo 
demo <- demo[!duplicated(STUDY_ID)]

# Limit to STUDY ID and ZIP CODE 
demo <- demo[, .(STUDY_ID, ZIP)]

# Drop NA ZIP codes
demo <- demo[!is.na(ZIP)]

# Merge the COVID data with the demographics data
covid <- merge(covid, demo, by = "STUDY_ID", all.x = TRUE)

# Sum the number of tests and positive tests by ZIP code
covid_zip <- covid[, .(tested = sum(test), positive_test = sum(positive)), by = ZIP]

# Create the rates by ZIP code
covid_zip[, positive_rate := positive_test/tested]

# Read in the county crosswalk
county_crosswalk <- fread(here("data", "inputs", "ZIP_COUNTY_122023.csv"))

# Drop USPS_ZIP_PREF_CITY, RES_RATIO, BUS_RATIO, OTH_RATIO, TOT_RATIO
county_crosswalk[, c("RES_RATIO", "BUS_RATIO", "OTH_RATIO", "TOT_RATIO", "USPS_ZIP_PREF_CITY") := NULL]

county_crosswalk[duplicated(ZIP) & USPS_ZIP_PREF_STATE == "IN", .N] # There are 601 duplicate ZIP codes in IN (WHY?)
county_crosswalk[duplicated(ZIP) & USPS_ZIP_PREF_STATE == "IN"]

# Drop the duplicate ZIP codes in IN
county_crosswalk <- county_crosswalk[!duplicated(county_crosswalk[, c("ZIP", "USPS_ZIP_PREF_STATE", "USPS_ZIP_PREF_STATE")]), ]
setnames(county_crosswalk, old = "USPS_ZIP_PREF_STATE", new = "STATE")

# Drop the ZIP codes outside IN 
county_crosswalk <- county_crosswalk[STATE == "IN"]

# Change the data type of the ZIP for the NAS and crosswalk files
covid_zip[, ZIP := as.character(ZIP)]
county_crosswalk[, ZIP := as.character(ZIP)]

# Merge the COVID rates with the county crosswalk
covid_zip2 <- merge(covid_zip, county_crosswalk, by = "ZIP")

# Now collapse to be the test and positive by county counts 
covid_cty <- covid_zip2[, .(covid_tested = sum(tested), covid_positive_test = sum(positive_test)), by = .(COUNTY, STATE)]

# Positive test rate 
covid_cty[, covid_positive_rate := covid_positive_test/covid_tested]

# Read in the data
dt <- fread(here("data", "outputs", "final_analytic_sample_with_infant_char_for_reg.csv"))

# Create a floor_month variable for DELIVERY_DATE
dt[, floor_month := floor_date(DELIVERY_DATE, "month")]

# Race recode
dt[, race_recode := case_when(RACE == "WHITE" ~ "White",
                                        RACE == "BLACK OR AFRICAN AMERICAN" ~ "Black",
                                        RACE == "ASIAN/PACIFIC ISLANDER" ~ "Asian",
                                        TRUE ~ "other")]

# Limit to black and white, create a dummy for black 
dt <- dt[race_recode == "White" | race_recode == "Black"]

# Everything in March 2020 and later is in the post period
dt[, post := ifelse(floor_month >= "2020-03-01", 1, 0)]

# Limit to just the post period
dt <- dt[post == 1]

# Merge the COVID rates with the data
dt <- merge(dt, covid_cty, by = "COUNTY")

# Create the avg births by county
births_by_cty_yr <- dt[, .N, by = .(COUNTY, year)][order(-N)]
births_by_cty_yr_avg <- dt[, .N, by = .(COUNTY, year)][order(-N)][, .(avg_births = mean(N)), by = COUNTY]

dt2 <- dt[, .(tested = sum(tested), total_infants = .N, positive_tests = sum(positive_test)), by = .(COUNTY, race_recode, covid_positive_rate)]

# Merge the data with the avg births by county
dt2 <- merge(dt2, births_by_cty_yr_avg, by = "COUNTY", all.x = TRUE)

# County level testing rates 
dt2[, ":=" (county_tested = tested/total_infants, county_positive = positive_tests/tested)]
dt2[is.na(county_tested), county_tested := 0]
dt2[is.na(county_positive), county_positive := 0]

# White subset 
dt2_white <- dt2[race_recode == "White"]

# Black subset
dt2_black <- dt2[race_recode == "Black"]

# Fit a weighted linear model
fit_white <- lm(county_tested ~ covid_positive_rate, data = dt2_white, weights = avg_births)
fit_black <- lm(county_tested ~ covid_positive_rate, data = dt2_black, weights = avg_births)

# Extract the coefficients
intercept_white <- coef(fit_white)[1]
slope_white <- coef(fit_white)[2]

intercept_black <- coef(fit_black)[1]
slope_black <- coef(fit_black)[2]

# Plot the relationship between Covid Positive Test Rate and testing for White
white_Covidpos_Test <- ggplot(data = dt2_white, aes(x = covid_positive_rate, y = county_tested, size = avg_births)) + 
    geom_point(col = "mediumvioletred") + 
    theme_minimal() +
    labs(title = "White", x = "COVID-19 Positive Test Rate", y = "Testing Rate") +
    geom_abline(intercept = intercept_white, slope = slope_white, col = "mediumvioletred") +  # Add the regression line
    ylim(0, 1) + 
    xlim(0, 0.25) +
    annotate("text", x = 0.20, y = 0.4, label = paste("Coefficient: ", round(slope_white, 3))) + 
    theme(legend.position = "none", 
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          plot.title = element_text(size = 18, face = "bold"),
          panel.grid = element_blank(),  # Remove gridlines
          panel.border = element_blank(),  # Remove panel border
          axis.line = element_line())  # Add axis lines
ggsave(file.path(output_dir, "COVID19_county_tested_white_weightbirths.pdf"), plot = white_Covidpos_Test, height = 5, width = 6)


# Plot the relationship between Covid Positive Test Rate and testing for Black
black_Covidpos_Test <- ggplot(data = dt2_black, aes(x = covid_positive_rate, y = county_tested, size = avg_births)) + 
    geom_point(col = "midnightblue") + 
    theme_minimal() +
    labs(title = "Black", x = "COVID-19 Positive Test Rate", y = "Testing Rate") +
    geom_abline(intercept = intercept_black, slope = slope_black, col = "midnightblue") +  # Add the regression line
    ylim(0, 1) + 
    xlim(0, 0.25) +
    annotate("text", x = 0.20, y = 0.4, label = paste("Coefficient: ", round(slope_black, 3))) + 
    theme(legend.position = "none", 
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          plot.title = element_text(size = 18, face = "bold"),
          panel.grid = element_blank(),  # Remove gridlines
          panel.border = element_blank(),  # Remove panel border
          axis.line = element_line())  # Add axis lines
ggsave(file.path(output_dir, "COVID19_county_tested_black_weightbirths.pdf"), plot = black_Covidpos_Test, height = 5, width = 6)

# Grid arrange and add a title that is "Relationship between County Positive Test Rate by Race and Covid-19 Positive Test Rate"
title_pos <- textGrob("Relationship between County Test Rate by Race and Covid-19 Positive Test Rate", gp = gpar(fontsize = 20, fontface = "bold"))
covid_tested <- grid.arrange(white_Covidpos_Test, black_Covidpos_Test, ncol = 2, top = title_pos)
ggsave(file.path(output_dir, "Covid_Tested_County_tested_all_weightbirths.pdf"), plot = covid_tested, height = 5, width = 14)


# Fit a weighted linear model
fit_white <- lm(county_positive ~ covid_positive_rate, data = dt2_white, weights = avg_births)
fit_black <- lm(county_positive ~ covid_positive_rate, data = dt2_black, weights = avg_births)

# Extract the coefficients
intercept_white <- coef(fit_white)[1]
slope_white <- coef(fit_white)[2]

intercept_black <- coef(fit_black)[1]
slope_black <- coef(fit_black)[2]


# Plot the relationship between Covid Positive Test Rate and positive testing for White
white_Covidpos_Pos <- ggplot(data = dt2_white, aes(x = covid_positive_rate, y = county_positive, size = avg_births))+ 
    geom_point(col = "mediumvioletred") + 
    theme_minimal() +
    labs(title = "White", x = "COVID-19 Positive Test Rate", y = "Positive Test Rate") +
    geom_abline(intercept = intercept_white, slope = slope_white, linetype = "dashed", col = "mediumvioletred") +  # Add the 45 degree line
    ylim(0, 1) + 
    xlim(0, 0.25) +
    annotate("text", x = 0.20, y = 0.4, label = paste("Coefficient: ", round(slope_white, 3))) + 
    theme(legend.position = "none", 
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          plot.title = element_text(size = 18, face = "bold"),
          panel.grid = element_blank(),  # Remove gridlines
          panel.border = element_blank(),  # Remove panel border
          axis.line = element_line())  # Add axis lines
ggsave(file.path(output_dir, "COVID19_county_positive_white_weightbirths.pdf"), plot = white_Covidpos_Pos, height = 5, width = 6)

# Plot the relationship between Covid Positive Test Rate and positive testing for Black
black_Covidpos_Pos <- ggplot(data = dt2_black, aes(x = covid_positive_rate, y = county_positive, size = avg_births)) + 
    geom_point(col = "midnightblue") + 
    theme_minimal() +
    labs(title = "Black", x = "COVID-19 Positive Test Rate", y = "Positive Test Rate") +
    geom_abline(intercept = intercept_black, slope = slope_black, linetype = "dashed", col = "midnightblue") +  # Add the 45 degree line
    ylim(0, 1) + 
    xlim(0, 0.25) +
    annotate("text", x = 0.20, y = 0.4, label = paste("Coefficient: ", round(slope_black, 3))) + 
    theme(legend.position = "none", 
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          plot.title = element_text(size = 18, face = "bold"),
          panel.grid = element_blank(),  # Remove gridlines
          panel.border = element_blank(),  # Remove panel border
          axis.line = element_line())  # Add axis lines
ggsave(file.path(output_dir, "COVID19_county_positive_black_weightbirths.pdf"), plot = black_Covidpos_Pos, height = 5, width = 6)

# Grid arrange and add a title that is "Relationship between County Positive Test Rate by Race and Covid-19 Positive Test Rate"
title_pos <- textGrob("Relationship between County Positive Test Rate by Race and Covid-19 Positive Test Rate", gp = gpar(fontsize = 20, fontface = "bold"))
covid_tested_pos <- grid.arrange(white_Covidpos_Pos, black_Covidpos_Pos, ncol = 2, top = title_pos)
ggsave(file.path(output_dir, "Covid_Tested_County_Positive_all_weightbirths.pdf"), plot = covid_tested_pos, height = 5, width = 14)

