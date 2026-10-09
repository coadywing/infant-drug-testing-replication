# Drug overdose mortality IN 
# MKD 
# April 16 2024

# Install packages 
if(!require("pacman")) install.packages("pacman")
library(pacman)
p_load(data.table, collapse, fst, lubridate, dplyr, haven, here)
rm(list=ls())

# Create a summary table
dt_in <- fread(here("data", "inputs", "IN_mortality_2011_2021.csv"))

# Create a summary table of drug overdose mortality
summary <- dt_in[, .(
  od_deaths = sum(drug_od_death),
  all_deaths = .N), by = .(race_recode, countyoc, year)]

# For 2011-2018: sum the number of drug overdose deaths and all deaths by county, race_recode 
summary1 <- summary[year < 2019, .(
  od_deaths_201118 = sum(od_deaths),
  all_deaths_201118 = sum(all_deaths)), by = .(race_recode, countyoc)]

# Make wide and then total all deaths by county 
summary_wide <- dcast(summary1, countyoc ~ race_recode, value.var = c("od_deaths_201118", "all_deaths_201118"))
summary_wide[is.na(summary_wide)] <- 0

summary_wide[, all_od_deaths_201118 := od_deaths_201118_black + od_deaths_201118_white + od_deaths_201118_other]
summary_wide[, all_deaths_201118 := all_deaths_201118_black + all_deaths_201118_white + all_deaths_201118_other] 

# Repeat this for 2014-2018 
summary2 <- dt_in[year >= 2014 & year <= 2018, .(
  od_deaths_201418 = sum(drug_od_death),
  all_deaths_201418 = .N), by = .(race_recode, countyoc)]

summary2_wide <- dcast(summary2, countyoc ~ race_recode, value.var = c("od_deaths_201418", "all_deaths_201418"))
summary2_wide[is.na(summary2_wide)] <- 0

summary2_wide[, all_od_deaths_201418 := od_deaths_201418_black + od_deaths_201418_white + od_deaths_201418_other]
summary2_wide[, all_deaths_201418 := all_deaths_201418_black + all_deaths_201418_white + all_deaths_201418_other]

# Combine 
summary_wide <- merge(summary_wide, summary2_wide, by = "countyoc", all = TRUE)

# Create a new FIPS variable that is 18 + the countyoc (last three digits)
summary_wide[, FIPS := paste0(18,substr(countyoc, 3, 5))]

# Read in the population data 
pop <- readxl::read_xlsx(here("data", "inputs", "2019_cntyest.xlsx"), skip = 1)
setDT(pop)

# Merge 
pop <- pop[, 2:11]
setnames(pop, "...2", "FIPS")
pop[, FIPS := as.character(FIPS)]
summary_wide <- merge(summary_wide, pop, by = "FIPS", all = TRUE)

# Save 
fwrite(summary_wide, here("data", "inputs", "IN_mortality_summary_201118_201418.csv"))
