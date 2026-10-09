# County-year-race drug overdose mortality in IN 
# MKD 
# April 5 2024

# Install packages 
if(!require("pacman")) install.packages("pacman")
library(pacman)
p_load(data.table, collapse, fst, lubridate, dplyr, haven, here)
rm(list=ls())
source(here("code", "00_paths.r"))

# Read and combine mortality data for 2011-2021
years <- 2011:2021
dt_in <- rbindlist(lapply(years, function(year) {
  path <- sprintf("%s/MULT%d.USAllCnty.dta", path_nvss_mortality_dir, year)
  dt <- setDT(read_dta(path))
  dt[stateoc == "IN"]
}), fill = TRUE)

drug_od_cod <- c("X40", "X41", "X42", "X43", "X44", "X60", "X61", "X62", "X63", "X64", "X85", "Y10", "Y11", "Y12", "Y13", "Y14")

dt_in[, drug_od_death := ifelse(ucod %in% drug_od_cod, 1, 0)]

dt_in[, .N, by = c("countyoc", "drug_od_death", "year")][order(year, -drug_od_death, -N)]

# Create a race variable 
dt_in[, .N, by = racer40][order(racer40, -N)]
dt_in[, race_recode := case_when(racer40 == "01" ~ "white", 
                            racer40 == "02" ~ "black", 
                            TRUE ~ "other")]
dt_in[, .N, by = race_recode]

# write out the indiana table 
fwrite(dt_in, here("data", "inputs", "IN_mortality_2011_2021.csv"))

