#===============================================================================
# Script 34: Maternal vs infant testing correlations (Appendix Figure A8 and other not in paper)
#
# Look at the correlation between mom drug testing before pregnancy /
# during pregnancy / after pregnancy and Infant Drug Testing
# Creates appendix figure showing relationships across years and racial groups
# MKD
# Dec 4, 2024
#===============================================================================

# Packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, ggplot2, lubridate, collapse, dplyr, here)
rm(list=ls())

# Output directory
output_dir <- here("results")

# Read in the mom drug testing data from Script 33
lab_moms_collapse <- fread(here("data", "outputs", "lab_moms_collapse.csv"))
# Read in the analytic sample from Script 15
oaxaca <- fread(here("data", "outputs", "final_analytic_sample_with_infant_char_for_reg.csv"))

# Merge the mom drug testing data with the oaxaca data -- but only with the Mom Study ID and the date of birth
lab_moms_collapse <- merge(lab_moms_collapse, oaxaca[, .(STUDY_ID, DOB)], by = "STUDY_ID")

# Want to create variables for 1) before pregnancy, 2) during pregnancy, 3) after pregnancy
# Before pregnancy is any test date before 9 months before the DOB
# During pregnancy is any test date between 9 months before the DOB and the DOB
# After pregnancy is any test date after the DOB
lab_moms_collapse[, DOB := as.Date(DOB, format = "%m/%d/%Y")]
lab_moms_collapse[, LAB_DATE := as.Date(LAB_DATE, format = "%m/%d/%Y")]
lab_moms_collapse[, before_preg := ifelse(LAB_DATE < (DOB - months(9)), 1, 0)]
lab_moms_collapse[, during_preg := ifelse(LAB_DATE >= (DOB - months(9)) & LAB_DATE <= DOB, 1, 0)]
lab_moms_collapse[, after_preg := ifelse(LAB_DATE > DOB, 1, 0)]

# Create variables to flag the positive tests before, during, and after pregnancy
lab_moms_collapse[, ':=' (positive_before_preg = ifelse(before_preg == 1 & positive_test == 1, 1, 0),
                      positive_during_preg = ifelse(during_preg == 1 & positive_test == 1, 1, 0),
                      positive_after_preg = ifelse(after_preg == 1 & positive_test == 1, 1, 0))]
fmean(lab_moms_collapse)

# Create a variable for the number of tests before, during, and after pregnancy
# And if any of the tests are positive before, during, and after pregnancy
lab_mom_summary <- lab_moms_collapse[, .(total_tests_mom = sum(total_tests),
                                           any_positive_test_mom = max(positive_test),
                                           before_preg_mom = max(before_preg),
                                           during_preg_mom = max(during_preg),
                                           after_preg_mom = max(after_preg),
                                           positive_before_preg_mom = max(positive_before_preg),
                                           positive_during_preg_mom = max(positive_during_preg),
                                           positive_after_preg_mom = max(positive_after_preg)), by = STUDY_ID]

# Merge the summary data with the oaxaca data
oaxaca <- merge(oaxaca, lab_mom_summary, by = "STUDY_ID", all.x = TRUE)

# Replace NAs with 0s for the total_tests_mom, any_positive_test_mom, before_preg_mom, during_preg_mom, after_preg_mom, positive_before_preg_mom, positive_during_preg_mom, positive_after_preg_mom
oaxaca[is.na(total_tests_mom), total_tests_mom := 0]
oaxaca[is.na(any_positive_test_mom), any_positive_test_mom := 0]
oaxaca[is.na(before_preg_mom), before_preg_mom := 0]
oaxaca[is.na(during_preg_mom), during_preg_mom := 0]
oaxaca[is.na(after_preg_mom), after_preg_mom := 0]
oaxaca[is.na(positive_before_preg_mom), positive_before_preg_mom := 0]
oaxaca[is.na(positive_during_preg_mom), positive_during_preg_mom := 0]
oaxaca[is.na(positive_after_preg_mom), positive_after_preg_mom := 0]

# Look at the correlation between mom drug testing before pregnancy / during pregnancy / after pregnancy and infant drug testing
# Collapse to be the mean of the testing rates for infants by race by the mom drug testing variables
any_test_by_race <- oaxaca[, .(mean_infant_testing = mean(tested),
           mean_infant_positive = mean(positive_test)), by = .(black, before_preg_mom, during_preg_mom, after_preg_mom, year)]

# Categorize the mom drug testing into only before, only during, only after, adn the combinations of before and during, before and after, during and after, and all three
any_test_by_race[, testing_cat := case_when(before_preg_mom == 1 & during_preg_mom == 0 & after_preg_mom == 0 ~ "Only Before",
                                        before_preg_mom == 0 & during_preg_mom == 1 & after_preg_mom == 0 ~ "Only During",
                                        before_preg_mom == 0 & during_preg_mom == 0 & after_preg_mom == 1 ~ "Only After",
                                        before_preg_mom == 1 & during_preg_mom == 1 & after_preg_mom == 0 ~ "Before and During",
                                        before_preg_mom == 1 & during_preg_mom == 0 & after_preg_mom == 1 ~ "Before and After",
                                        before_preg_mom == 0 & during_preg_mom == 1 & after_preg_mom == 1 ~ "During and After",
                                        before_preg_mom == 1 & during_preg_mom == 1 & after_preg_mom == 1 ~ "All Three",
                                        TRUE ~ "None")]

# Plot this using the facet_wrap on the testing_cat and have a line for the mean_infant_Testing by black == 0 and black == 1
ggplot(any_test_by_race) +
    geom_line(aes(x = year, y = mean_infant_testing, color = as.factor(black))) +
    facet_wrap(~testing_cat) +
    theme_minimal() +
    labs(title = "Mean Infant Testing by Mom Drug Testing Before, During, and After Pregnancy",
         x = "Year",
         y = "Mean Infant Testing Rate") +
    scale_color_manual(values = c("black", "red"))
ggsave(file.path(output_dir, "mom_drug_testing_and_infant_drug_testing.pdf"), width = 10, height = 10)

# Plot the positive testing rates using facet_wrap on the testing_cat and have a line for the mean_infant_positive by black == 0 and black == 1
ggplot(any_test_by_race) +
    geom_line(aes(x = year, y = mean_infant_positive, color = as.factor(black))) +
    facet_wrap(~testing_cat) +
    theme_minimal() +
    labs(title = "Mean Infant Positive Testing by Mom Drug Testing Before, During, and After Pregnancy",
         x = "Year",
         y = "Mean Infant Positive Testing Rate") +
    scale_color_manual(values = c("black", "red"))
ggsave(file.path(output_dir, "mom_drug_testing_and_infant_positive_testing.pdf"), width = 10, height = 10)

# Just focus on the moms tested during pregnancy
preg_testing_summary <- oaxaca[, .(mean_infant_testing = mean(tested),
           mean_infant_positive = mean(positive_test),
           N = .N), by = .(black, during_preg_mom, year)]
preg_testing_summary[, during_preg := ifelse(during_preg_mom == 1, "Tested During Pregnancy", "Not Tested During Pregnancy")]

# Add confidence intervals
preg_testing_summary[, ci_testing := 1.96 * sqrt(mean_infant_testing * (1 - mean_infant_testing)/N)]
preg_testing_summary[, ci_positive := 1.96 * sqrt(mean_infant_positive * (1 - mean_infant_positive)/N)]

preg_testing_summary[, ci_upper_bound_testing := mean_infant_testing + ci_testing]
preg_testing_summary[, ci_lower_bound_testing := mean_infant_testing - ci_testing]
preg_testing_summary[, ci_upper_bound_positive := mean_infant_positive + ci_positive]
preg_testing_summary[, ci_lower_bound_positive := mean_infant_positive - ci_positive]

# Now focus on mom positive during pregnancy
preg_testing_pos_summary <- oaxaca[, .(mean_infant_testing = mean(tested),
           mean_infant_positive = mean(positive_test)), by = .(black, positive_during_preg_mom, year)]

# Plot the testing rates by black == 0 and black == 1 for the moms tested during pregnancy
ggplot(preg_testing_summary) +
    geom_line(aes(x = year, y = mean_infant_testing, color = as.factor(black), group = as.factor(black))) +
    geom_point(aes(x = year, y = mean_infant_testing, color = as.factor(black))) +
    # Use geom_ribbon to add the confidence intervals
    geom_ribbon(aes(x = year, ymin = ci_lower_bound_testing, ymax = ci_upper_bound_testing, fill = as.factor(black)), alpha = 0.2) +
    facet_wrap(~during_preg) +
    theme_bw() +
    labs(title = "Infant Testing Rate by Maternal Drug Testing Status During Pregnancy and Racial Group of Infant",
         x = "Year",
         y = "Testing Rate") +
  scale_color_manual(values = c("0" = "mediumvioletred", "1" = "midnightblue"), labels = c("White", "Black")) +
  scale_fill_manual(values = c("0" = "mediumvioletred", "1" = "midnightblue"), labels = c("White", "Black")) +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        plot.title = element_text(size = 18, face = "bold"),
        panel.grid = element_blank(),
        axis.line = element_line(),
        strip.text = element_text(size = 14, face = "bold"), # Larger facet label text
        strip.background = element_rect(fill = "white", color = "black") # White background, black border
) +
    # Add keep the legend but there should be no title
    theme(legend.title = element_blank())
ggsave(file.path(output_dir, "mom_drug_testing_during_pregnancy_and_infant_testing.pdf"), width = 14, height = 6)

# Plot the testing rates by black == 0 and black == 1 for the moms tested during pregnancy
ggplot(preg_testing_pos_summary) +
    geom_line(aes(x = year, y = mean_infant_testing, color = as.factor(black))) +
    facet_wrap(~positive_during_preg_mom) +
    theme_bw() +
    labs(title = "Mean Infant Testing by Mom Positive Drug Test Status During Pregnancy",
         x = "Year",
         y = "Mean Infant Testing Rate") +
    scale_color_manual(values = c("black", "red"))
ggsave(file.path(output_dir, "mom_positive_drug_testing_during_pregnancy_and_infant__testing.pdf"), width = 10, height = 6)

# Plot the the positive testing rates by black == 0 and black == 1 for the moms tested during pregnancy
ggplot(preg_testing_summary) +
    geom_line(aes(x = year, y = mean_infant_positive, color = as.factor(black))) +
    facet_wrap(~during_preg_mom) +
    theme_bw() +
    labs(title = "Mean Infant Positive Testing by Mom Drug Test Status During Pregnancy",
         x = "Year",
         y = "Mean Infant Positive Testing Rate") +
    scale_color_manual(values = c("black", "red"))
ggsave(file.path(output_dir, "mom_drug_testing_during_pregnancy_and_infant_positive_testing.pdf"), width = 10, height = 6)

# Plot the the positive testing rates by black == 0 and black == 1 for the moms who tested positive during pregnancy
ggplot(preg_testing_pos_summary) +
    geom_line(aes(x = year, y = mean_infant_positive, color = as.factor(black))) +
    facet_wrap(~positive_during_preg_mom) +
    theme_bw() +
    labs(title = "Mean Infant Positive Testing by Mom Positive Drug Test Status During Pregnancy",
         x = "Year",
         y = "Mean Infant Positive Testing Rate") +
    scale_color_manual(values = c("black", "red"))
ggsave(file.path(output_dir, "mom_positive_drug_testing_during_pregnancy_and_infant_positive_testing.pdf"), width = 10, height = 6)
