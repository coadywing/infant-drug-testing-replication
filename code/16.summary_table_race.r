#===============================================================================
# Script 16: Create summary table with reweighted means
#
# Merge base statistics with black reweighted means from propensity score analysis
# MKD
# June 11, 2024
#===============================================================================

# Packages
if(!require("pacman")) install.packages("pacman")
library(pacman)
p_load(data.table, collapse, tidyverse, gtsummary, lubridate, rmarkdown, tableone, kableExtra, dplyr, modelsummary, vtable, here)

# Variable name mapping for display
variable_labels <- list(
  "age_deliv" = 'Delivery Age',
  "age_bin15" = 'Age Under 15',
  "age_bin20" = 'Age 15-19',
  "age_bin25" = 'Age 20-24',
  "age_bin30" = 'Age 25-29',
  "age_bin35" = 'Age 30-34',
  "age_bin40" = 'Age 35-39',
  "age_bin45" = 'Age 40+',
  "medicaid" = 'Medicaid',
  "private_ins" = 'Private Insurance',
  "other_ins" = 'Other Insurance',
  "any_sud" = 'Maternal Substance Use Disorder',
  "any_cpd" = 'Maternal Chronic Pain Disorder',
  "any_gest_diab" = 'Maternal Gestational Diabetes',
  "any_hypt" = 'Maternal Gestational Hypertensive Disorder',
  "any_ment_hlth" = 'Maternal Mental Health Disorder',
  "any_prenatal" = 'Any Prenatal Care',
  "twelve_prenatal_plus" = '12+ Prenatal Visits',
  "tested" = 'Infant Tested',
  "positive_test" = 'Infant Positive Test',
  "crude_mort_201418_10k" = 'Crude Mortality Rate (2014-2018) per 10,000 population',
  "mom_black" = 'Mom Black',
  "mom_white" = 'Mom White',
  "mom_other" = 'Mom Other',
  "lbw" = 'Low Birth Weight',
  "premature" = 'Premature',
  "NAS1_rate" = 'NAS Rate',
  "above_avg_nas1" = 'Above Average NAS Rate',
  "encounters_within_1month" = 'Encounters Within 1 Month of Birth'
)

# ============================================================================
# Create base summary table
# ============================================================================
# Read in the processed data
dt <- fread(here("data", "outputs", "final_analytic_sample_with_infant_char_for_reg.csv"))

# Limit to white and black infants
dt_to_summarize <- dt[race_recode == "White" | race_recode == "Black",
  c("race_recode", names(variable_labels)), with = FALSE]

# Rename variables for display
old_names <- names(variable_labels)
new_names <- as.character(variable_labels)
setnames(dt_to_summarize, old = old_names, new = new_names)

# Calculate the number of infants for each race group
infant_counts <- dt_to_summarize %>%
  group_by(race_recode) %>%
  summarise(Count = n())

# Create a new row with this information
infant_counts <- infant_counts %>%
  mutate(name = "Count", summary_label = as.character(Count)) %>%
  select(race_recode, name, summary_label) %>%
  spread(key = race_recode, value = summary_label)

# Create the summary table
summary_table <- dt_to_summarize %>%
  pivot_longer(cols = -race_recode) %>%
  group_by(race_recode, name) %>%
  summarise(Mean = mean(value, na.rm = TRUE), Count = sum(value, na.rm = TRUE)) %>%
  mutate(summary_label = sprintf("%.2f (%.0f)", Mean, Count)) %>%
  select(race_recode, name, summary_label) %>%
  distinct() %>%
  spread(key = race_recode, value = summary_label)

# Append the infant counts row to the summary table
summary_table <- bind_rows(infant_counts, summary_table)


# ============================================================================
# Add black reweighted means
# ============================================================================
# Load reweighted means from Script 15
reweighted_means_black <- fread(here("data", "outputs", "temp_reweighted_means.csv"))

# Rename columns with display labels
setnames(reweighted_means_black,
         new = c('White', 'Race Recode', 'Delivery Age', 'Age 15-19', 'Age 20-24', 'Age 25-29', 'Age 30-34', 'Age 35-39', 'Age 40-44', 'Age 45+',
                 'Medicaid', 'Private Insurance', 'Other Insurance', 'Maternal Substance Use Disorder', 'Maternal Chronic Pain Disorder',
                 'Maternal Gestational Diabetes', 'Maternal Gestational Hypertensive Disorder', 'Maternal Mental Health Disorder',
                 'Any Prenatal Care', '12+ Prenatal Visits', 'Infant Tested', 'Infant Positive Test',
                 'Crude Mortality Rate (2014-2018) per 10,000 population', 'Mom Black', 'Mom White', 'Mom Other', 'Low Birth Weight', 'Premature', 'NAS Rate', 'Above Average NAS Rate',
                 'Encounters Within 1 Month of Birth', 'ATT_weight_white2'))

# Convert to long format
reweighted_means_black <- reweighted_means_black %>%
  pivot_longer(cols = -`Race Recode`) %>%
  mutate(name = factor(name, levels = names(reweighted_means_black)))

# Drop the "Race Recode" column and rows with ATT_weight_white2 and White
reweighted_means_black <- reweighted_means_black %>%
  filter(name != "ATT_weight_white2", name != "White") %>%
  select(-`Race Recode`)

# Add the reweighted means to the summary table
summary_table <- merge(summary_table, reweighted_means_black, by = "name", all.x = TRUE)

# Change "value" to be "Black Reweighted"
setnames(summary_table, "value", "Black Reweighted")

# Set the summary table as a data.table
summary_table <- as.data.table(summary_table)

# Round the black reweighted means to 2 decimal places
summary_table[, `Black Reweighted` := sprintf("%.2f", `Black Reweighted`)]


fwrite(summary_table, here("results", "summary_table_with_black_reweighted.csv"))
