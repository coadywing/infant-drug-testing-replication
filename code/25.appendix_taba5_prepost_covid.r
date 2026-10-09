# Appendix table with Covid pre-post testing rate and positive test rate 
# This is Table A5 
# MKD 
# Nov 14 2024 

# Packages 
if(!require(pacman)) install.packages("pacman")
library(pacman)
p_load(data.table, lubridate, fixest, modelsummary, collapse, ggplot2, dplyr, gridExtra, here)

rm(list = ls())

# Load data
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

# Collapse to be the test rate and positive rate by pre and post period and race
pre_post <- dt[, .(.N, tested = sum(tested), positive_test = sum(positive_test)), by = .(post, race_recode)]

# Create the rates 
pre_post[, testing_rate := tested/N]
pre_post[, positive_rate := positive_test/tested]

# End 