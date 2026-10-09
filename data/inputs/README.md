# data/inputs

Input files read by the scripts in `code/`. The restricted source data (INPC, HCUP SID, NVSS)
are not stored here; their locations are set in `code/00_paths.r` (see the main README).

## Included in this repository

| File | Used in | Description |
|------|---------|-------------|
| `umbilical_cord_loinc.csv` | Script 04 | Umbilical cord drug test codes: INPC component test code, chart name, and LOINC code. Compiled by the authors. |
| `Loinc_2.76_Export_20231116174148_Meconium.csv` | Script 04 | Search export of meconium LOINC codes from LOINC version 2.76 (exported 2023-11-16). |
| `2019_cntyest.xlsx` | `code/nvss_code/summary_drug_mortality_in.r` | Indiana county population estimates, 2010–2019 (U.S. Census Bureau, Vintage 2019). |

The two LOINC files contain material from LOINC (https://loinc.org), copyright © Regenstrief
Institute, Inc., available at no cost under the license at https://loinc.org/license.

## Public files to download

| File | Used in | Source |
|------|---------|--------|
| `GroupLoincTerms.csv` | Script 31 | LOINC Groups file, distributed with the LOINC release at https://loinc.org/downloads/ (free account required). |
| `ZIP_COUNTY_122023.csv` | Scripts 08, 14, 26 | HUD USPS ZIP Code Crosswalk, ZIP–county, 4th quarter 2023, from https://www.huduser.gov/portal/datasets/usps_crosswalk.html, saved as CSV. |

## Created by the pipeline

The following files are derived from restricted data. The pipeline writes them here, and later
scripts read them. They are not distributed.

| File | Written by | Source data |
|------|-----------|-------------|
| `mom_matched.csv`, `baby_matched.csv` | Script 01 | INPC natality linked files |
| `nas_counts_by_zip.csv` | `code/hcup_sid/nas_infant_drugs.r` | HCUP SID |
| `IN_mortality_2011_2021.csv` | `code/nvss_code/clean_mortality_in.r` | NVSS Multiple Cause of Death |
| `IN_mortality_summary_201118_201418.csv` | `code/nvss_code/summary_drug_mortality_in.r` | NVSS Multiple Cause of Death |
