# NAS among infants by county 
# MKD 
# July 12 2024 

# Packages 
if(!require("pacman")){
  install.packages("pacman")
  library(pacman)
}
p_load(data.table, collapse, lubridate, dplyr, here)

## Calculate the average number of infants born with NAS in 2019
## by county

# Read in the IN_SID_2019.csv file 
dt_2019 <- fread(file.path(path_hcup_sid_dir, "IN_SID_2019.csv"))

# Read in the IN_SID_2018.csv file 
dt_2018 <- fread(file.path(path_hcup_sid_dir, "IN_SID_2018.csv"))

# ICD-10 codes for NAS 
nas_codes <- c("P961") 
nas_codes2 <- c("P961", "P0414", "P0413", "P0415", 
                "P0416", "P0417", "P042", "P043",
                "P0441", "P0442", "P0449", "P0481")

# Limit to the diagnoses, age, and ZIP code variables (dx are I10_DX1-I10_DX60, age, and zip_code)
columns_to_keep <- c("AGE", "ZIP", "I10_BIRTH", "I10_DELIVERY", paste0("I10_DX", 1:60))

# Subset the data to keep only the relevant columns
dt <- dt_2019[, ..columns_to_keep]
dt <- dt_2018[, ..columns_to_keep] # the birth and delivery columns are not in the 2018 data

# Loop over all of the I10_DX columns and create a new column that is 1 if the diagnosis is NAS and 0 otherwise
for(i in 1:60){
  dt[, paste0("NAS1_", i) := as.numeric(get(paste0("I10_DX", i)) %in% nas_codes)]
  dt[, paste0("NAS2_", i) := as.numeric(get(paste0("I10_DX", i)) %in% nas_codes2)]
}

# Calculate the total number of NAS diagnoses for each patient
dt[, NAS1_total := rowSums(select(dt, starts_with("NAS1_")), na.rm = TRUE)]
dt[, NAS2_total := rowSums(select(dt, starts_with("NAS2_")), na.rm = TRUE)]
dt[, NAS1 := as.numeric(NAS1_total > 0)]
dt[, NAS2 := as.numeric(NAS2_total > 0)]

# Calculate the number of infants with NAS1, NAS2, and number of births by ZIP code 
cty_summary <- dt[, .(NAS1 = sum(NAS1), NAS2 = sum(NAS2), births = sum(I10_BIRTH)), by = ZIP]

# Save the results
fwrite(cty_summary, here("data", "inputs", "nas_counts_by_zip.csv"))

