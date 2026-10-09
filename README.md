# Racial Disparities in Infant Drug Testing

This is the replication package for "Racial Disparities in Infant Drug Testing: A Research Note," forthcoming in *Demography*. The study uses linked birth certificate and electronic health records from Indiana to document large racial disparities in newborn drug testing and positive test rates.

## Project Structure

```
infant_drug_tests/
├── code/
│   ├── main.R                          # Run this to reproduce all results
│   ├── 00_paths.r                      # External data paths (edit for your environment)
│   ├── 01.convert_natality_linked.r    # Convert INPC natality SAS files
│   ├── 02.check_data.r                 # Validate converted data
│   ├── 03.build_cohort.r               # Build birth cohort
│   ├── 04.lab_codes_filter.r           # Filter lab records by LOINC codes
│   ├── 05.combine_filtered_lab_files.r # Combine filtered lab files
│   ├── 06.opioid_drug_codes.r          # Identify opioid drug codes
│   ├── 07.labs_natality_cohort.r       # Merge labs with natality cohort
│   ├── 08.merge_county_crosswalk.r     # Merge ZIP-county crosswalk
│   ├── 09.merge_cnty_drug_od.r         # Merge county drug overdose data
│   ├── 10.create_mom_cpd_sud_preec_file.r  # Create maternal diagnosis file
│   ├── 11.merge_insurance.r            # Merge insurance records
│   ├── 12.clean_create_analytic_sample.r   # Clean and create analytic sample
│   ├── 13.add_infant_characteristics.r # Add infant characteristics
│   ├── 14.merge_cnty_nas.r             # Merge county NAS rates
│   ├── 15.data_prep_reweighting.r      # Prepare data for reweighting
│   ├── 16.summary_table_race.r         # Summary statistics by race (Table 1)
│   ├── 17.figure1.r                    # Figure 1: testing rates by year
│   ├── 18.figure_4.r                   # Figure 4: observed vs reweighted means
│   ├── 19.figure_2_3.r                 # Figures 2-3: testing/positive rates by race
│   ├── 20.oaxaca_blinder_decomp_appendix_taba4.r  # Oaxaca-Blinder decomposition
│   ├── 21.decomposition_ps.r           # Propensity score decomposition
│   ├── 22.decomp_year_fig5.r           # Decomposition by year (Figure 5)
│   ├── 23.appendix_fig23_county_mortality.r  # Appendix: county mortality figures
│   ├── 24.appendix_taba2_model.r       # Appendix Table A2: regression model
│   ├── 25.appendix_taba5_prepost_covid.r   # Appendix Table A5: pre/post COVID
│   ├── 26.appendix_figure_covid.r      # Appendix: COVID figures
│   ├── 27.compare_nvss.r              # Compare to NVSS counts
│   ├── 28.overlap.r                    # Propensity score overlap
│   ├── 30.reweight_positive_table.r    # Reweighted positive test table
│   ├── 31.find_drug_codes_mom.r        # Identify maternal drug test codes
│   ├── 32.filter_labs_drug_codes_mom.r # Filter maternal lab records
│   ├── 33.mom_drug_testing_file.r      # Create maternal drug testing file
│   ├── 34.mom_drug_testing_correlations.r  # Maternal-infant testing correlations
│   ├── hcup_sid/
│   │   └── nas_infant_drugs.r          # NAS counts from HCUP SID
│   └── nvss_code/
│       ├── clean_mortality_in.r        # Clean NVSS mortality data
│       ├── infant_summary_in.r         # Indiana infant mortality summary
│       └── summary_drug_mortality_in.r # Drug-related mortality summary
├── data/
│   ├── inputs/                         # Public input files (see data/inputs/README.md)
│   └── outputs/                        # Intermediate processed data (not tracked)
├── results/                            # Tables and figures (tracked by git)
├── manuscript/                         # LaTeX manuscript
│   ├── main.tex                        # Master LaTeX file
│   ├── 00_abstract.tex
│   ├── 01_intro.tex
│   ├── 02_data.tex
│   ├── 03_methods.tex
│   ├── 04_results.tex
│   ├── 05_discussion.tex
│   ├── 06_figures_tables.tex
│   ├── 07_appendix_text.tex
│   ├── 08_appendix_figures_tables.tex
│   ├── references.bib
│   ├── figures_tables/                 # Compiled figures and tables
│   └── build/                          # LaTeX build output (gitignored)
├── README.md
├── FILE_PIPELINE.md                    # Overview of the script pipeline
└── latexmkrc                           # LaTeX build configuration
```

## Reproducing Results

1. Clone the repository.
2. Download the public files listed in `data/inputs/README.md` into `data/inputs/`.
3. Place restricted data files as described below.
4. Edit `code/00_paths.r` to set the file paths for your computing environment.
5. Set the working directory to the repository root.
6. Run `code/main.R`.

This will process the INPC natality and lab data, build the analytic sample, and generate all tables and figures in `results/`.

## Restricted Data Requirements

The primary data sources are not publicly available and must be obtained separately. File paths in the code are specific to the secure computing environment where the analysis was conducted.

### INPC Natality Linked Files (SAS)

Linked birth certificate and electronic health record data from the Indiana Network for Patient Care (INPC), provided as SAS datasets. These contain mother-infant matched records for Indiana births.

- **Files:** `rdrp5665_mom_matched_fnl.sas7bdat`, `rdrp5665_baby_matched_fnl.sas7bdat`
- **Used in:** Script 01
- **Access:** Regenstrief Institute's Regenstrief Data Services acts as the custodian of these data. Data are available from the Regenstrief Institute for researchers who meet the criteria for access.

### INPC Converted CSV Files

Detailed encounter-level records from the INPC, converted from SAS to CSV format. These include demographics, lab results, encounters, diagnoses, and insurance information.

- **Files:** `encounter.sas7bdat.csv`, `demographics.sas7bdat.csv`, `labs.sas7bdat.csv`, `diagnosis.sas7bdat.csv`, `insurance.sas7bdat.csv`
- **Used in:** Scripts 02-04, 10-11, 13, 32

### HCUP SID (State Inpatient Databases)

Indiana State Inpatient Database files for 2018-2019 from the Healthcare Cost and Utilization Project. Used to construct county-level neonatal abstinence syndrome (NAS) counts.

- **Files:** `IN_SID_2018.csv`, `IN_SID_2019.csv`
- **Used in:** `code/hcup_sid/nas_infant_drugs.r`
- **Access:** Available from [HCUP](https://hcup-us.ahrq.gov/) with a data use agreement.

### CDC NVSS Multiple Cause of Death

National Vital Statistics System mortality data for 2011-2021 (Stata format) and natality data for 2019-2022. Mortality data used to compute county-level drug overdose mortality rates for Indiana. Natality data to compare the demographics for INPC births to the demographics for all IN births. 

- **Files:** `MULT2011.USAllCnty.dta` through `MULT2021.USAllCnty.dta` and `Natality_2019.csv` through `Natality_2022.csv`
- **Used in:** `code/nvss_code/clean_mortality_in.r`
- **Access:** Available from [NBER](https://www.nber.org/research/data/mortality-data-vital-statistics-nchs-multiple-cause-death-data) or [CDC WONDER](https://wonder.cdc.gov/).

## Public Data

A few small public input files (LOINC code lists and county population estimates) are included in `data/inputs/`. Two larger public files, the LOINC Groups file and the HUD ZIP–county crosswalk, must be downloaded separately. See [`data/inputs/README.md`](data/inputs/README.md) for the list of files, their sources, and the derived files that the pipeline writes to that folder.

## R Packages

Packages are installed automatically via `pacman` when running the code. Key packages include:

- `data.table` — data manipulation
- `fixest` — regression estimation
- `ggplot2` — visualization
- `survey` — survey-weighted analysis
- `haven` — reading SAS and Stata files
- `lubridate` — date manipulation
- `collapse` — fast data transformations
- `modelsummary` — regression tables
- `boot` — bootstrap inference
- `xtable` — LaTeX table export
- `duckdb` — database queries for large files

## LaTeX Compilation

To compile the manuscript from the project root:

```bash
latexmk -pdf manuscript/main.tex
```

The `latexmkrc` file sets `$do_cd = 1` so that latexmk changes to the manuscript directory before compiling. Build artifacts and the PDF are written to `manuscript/build/` (gitignored).
