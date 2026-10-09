#===============================================================================
# Script 11: Categorize maternal insurance
#
# Merge on the insurance data
# MKD
# June 10, 2024
#===============================================================================

# Packages
if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)}
p_load(data.table, lubridate, collapse, here)
rm(list=ls())
source(here("code", "00_paths.r"))

# External data paths
input_insurance <- file.path(path_inpc_csv_dir, "insurance.sas7bdat.csv")
input_encounters <- file.path(path_inpc_csv_dir, "encounter.sas7bdat.csv")

# Read in the data on mom's
moms <- fread(here("data", "outputs", "mom_dx_enc_collapse.csv"))

    # Define the 1 year date after delivery date and year before delivery date
    moms[, DELIVERY_DATE := as.Date(DELIVERY_DATE)]
    moms[, YEAR1_AFTER_DELIV := DELIVERY_DATE + years(1)]
    moms[, YEAR1_BEFORE_DELIV := DELIVERY_DATE - years(1)]

# Read in the insurance data (full INPC insurance file)
ins <- fread(input_insurance)

    # Restrict to only the STUDY_IDs we need
    ins2 <- ins[STUDY_ID %in% moms$STUDY_ID]

# Read in the encounter data (full INPC encounter file)
enc <- fread(input_encounters)

    # Restrict to only the STUDY_IDs we need
    enc2 <- enc[STUDY_ID %in% moms$STUDY_ID]

# Merge the insurance data with the encounter data
enc_ins <- merge(enc2, ins2, by = c("STUDY_ID", "ENCOUNTER_ID_DE"), all = TRUE)

# Use the modified insurance schema to categorize the insurance data
enc_ins[, INSURANCE_TEXT := tolower(INSURANCE_TEXT)]
enc_ins[, INSURANCE_CODE := tolower(INSURANCE_CODE)]

# Define insurance categories with their keywords
insurance_patterns <- list(
  medicaid = "medicaid|aca|buckeye|caresource|chip|healthwise|hoosier care connect|hip|hoosier|healthy indiana|managed health services|mhs|mdwise|passport|590 program",
  medicare = "medicare|csi|heartland|ma|medicare advantage|paramount|590",
  private = "aetna|anthem|blue cross|bc|coventry|care source|coresource|humana|iu|members health|priority|united|private|allstate|all savers|auxiant|abh|aultcare|advantage|aliera|allegiance|altrua|amerigroup|amerihealth|avalon|all state|allied|americo|ambetter|aperion|ac|allsavers|administrative|american|aarp|arc|asr|amtrust|assurant|amish|aig|apwu|accendo|broadspire|bankers|bas|benefit|colonial|crum|community|conseco|consociate|corizon|continental general|cincinnati|custom design benefits|cigna|commercial|community health alliance|community hlth alliance|cha|church|commercia|capital|corvel|companion|ccmsi|central|century|degree|dunn|dean|everence|emblem|employee|employer|ebms|erie|encore|esis|fore|first|founders|franciscan|farm|fring|federated|geico|gallagher|geha|gerber|grange|group administrators|global|gilsbar|gmp|gpm|golden|healthscope|healthlink|heritage|hartford|key benefit|kskj|jwf specialty|kemper|lutheran|life|lucent|liberty|loomis|laborers welfare|lumico|mediplus|meridian health|meritain|medishare|medi share|medben|mutual|mcd|mhbp|molina|medishare|medi-share|medico|meridian|meritan|michiana|multiplan|mvp|nalc|national|notre dame|nationwide|optimed|optimum|one call|oxford|prohealth|pekin|physician|progessive|phcs|php|partners|progressive|prairie|providence|premier|philadelphia|pai|quality correctional|q3|royal neighbors|retiree|reliance standard|river|rural carrier|reserve|secura|security|sagamore|starmark|samba|sisco|sedgwick|select|sentry|safeco|shelter|sedgewick|st vincent|shenandoah|signature|stewart|southern|siho|trinity|third party|travelers|travlers|travel|tufts|tsc|tall tree|trustmark|trexis|transamerica|tokio|thrivent|uhc|university|unified|usa senior|unicare|upmc|uhss|umr|umwa|unique|vendors|wellcare|web|world|wexford|westfield|wps|william|western|washington national|west|wea trust|wellbrooke|witham|zurich|york|yoder|zion|zenith|zelis",
  self = "self pay|selfpay|do not use|sliding fee scale|uninsured",
  veteran = "va |veteran|tricare|usaa|veterns|va hospital",
  other = "aaa|auto|athletic|accident|covid|county|carpenters|inmate|jail|jay occupational services|equitable|hospice|liability|misc|other|paoli|police|plumbers|pipe|roofers|student resources|sag|town|the health plan|union|violent|worker|workman|work|wcomp|wc"
)

# Create flags for each insurance category
for (category in names(insurance_patterns)) {
  pattern <- insurance_patterns[[category]]
  enc_ins[, (category) := ifelse(INSURANCE_TEXT %ilike% pattern | INSURANCE_CODE %ilike% pattern, 1, 0)]
}

# Prioritize categories to remove double-categorized rows
# Priority order: self > medicaid > medicare > veteran > private > other
priority_order <- c("self", "medicaid", "medicare", "veteran", "private", "other")
for (i in 1:(length(priority_order) - 1)) {
  current_cat <- priority_order[i]
  lower_cats <- priority_order[(i + 1):length(priority_order)]
  for (lower_cat in lower_cats) {
    enc_ins[get(current_cat) == 1, (lower_cat) := 0]
  }
}

# Save this temporary data
fwrite(enc_ins, here("data", "outputs", "temp_enc_ins.csv"))

# Merge the insurance data with the moms data
moms_ins <- merge(moms, enc_ins, by = "STUDY_ID", all = TRUE, allow.cartesian = TRUE)

# Restrict to the variables we need
moms_ins <- moms_ins[, .(STUDY_ID, DELIVERY_DATE, YEAR1_AFTER_DELIV, YEAR1_BEFORE_DELIV, ADMIT_TIME, DISCHARGE_TIME, medicaid, medicare, private, self, veteran, other)]

# Limit to the encounters where the admit_time/discharge_time is within the year before or after delivery
moms_ins[, keep := as.numeric(ADMIT_TIME >= YEAR1_BEFORE_DELIV & ADMIT_TIME <= YEAR1_AFTER_DELIV)]
moms_ins[is.na(keep), keep := as.numeric(DISCHARGE_TIME >= YEAR1_BEFORE_DELIV & DISCHARGE_TIME <= YEAR1_AFTER_DELIV)]
moms_ins <- moms_ins[keep == 1]

# For each mother, summarize the insurance categories
mom_sum <- moms_ins[, .(medicaid = mean(medicaid),
                        medicare = mean(medicare),
                        private = mean(private),
                        self = mean(self),
                        veteran = mean(veteran),
                        other = mean(other)), by = STUDY_ID]

mom_sum[, .N, by = medicaid]

# Categorize "primary" insurance based on the highest proportion
    # If .5 or more for Medicaid, then classify as Medicaid
    mom_sum[medicaid >= .5, insurance := "Medicaid"]

    # If not Medicaid and .5 or more private, then classify as private
    mom_sum[is.na(insurance) & private >= .5, insurance := "Private"]

    # If not Medicaid or private and .5 or more Medicare, then classify as Medicare
    mom_sum[is.na(insurance) & medicare >= .5, insurance := "Medicare"]

    # If not Medicaid, private, or Medicare and .5 or more self, then classify as self
    mom_sum[is.na(insurance) & self >= .5, insurance := "Self"]

    # If not Medicaid, private, Medicare, or self and .5 or more veteran, then classify as veteran
    mom_sum[is.na(insurance) & veteran >= .5, insurance := "Veteran"]

    # If not Medicaid, private, Medicare, self, or veteran, then classify as other
    mom_sum[is.na(insurance), insurance := "Other"]

    # Count the number of mothers in each category
    mom_sum[, .N, by = insurance]

# Save the data with the insurance categories (intermediate)
fwrite(mom_sum, here("data", "outputs", "temp_enc_ins.csv"))

# Limit to STUDY_ID, insurance
mom_ins <- mom_sum[, .(STUDY_ID, insurance)]

# Merge this data with the mom data
moms <- merge(moms, mom_ins, by = "STUDY_ID", all = TRUE)

# Save the data
fwrite(moms, here("data", "outputs", "mom_sud_cpd_preec_enc_ins.csv"))