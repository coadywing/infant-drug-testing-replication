# File Pipeline: Scripts 1-34

## 3 Phases Overview

| Phase | Scripts | Purpose | Output |
|-------|---------|---------|--------|
| **1. Build** | 1-14 | Raw data → Cohort → Analytic sample | `final_analytic_sample_with_infant_char_nas.csv` |
| **2. Analyze** | 15-30 | Propensity scores and Tables, figures | Latex tables + PDF figures |
| **3. Supplemental Maternal** | 31-34 | Maternal drug testing → Correlations | Appendix figure |

---

## Phase 1: Data Pipeline (Scripts 1-14)

Each script progressively builds the analytic sample by adding data.

```
1. Convert SAS → CSV
2. Validate & Deduplicate
3. Build Mother-Infant Cohort
4-6. Add Lab Testing Data
7. Merge Labs with Cohort
8. Add County Info
9-10. Add Maternal Diagnoses (SUD, CPD, etc.)
11. Add Insurance
12. Clean & Create Analytic Sample
13. Add Infant Characteristics (birthweight, prematurity, NICU)
14. Add County NAS Rates → FINAL ANALYTIC SAMPLE
```

**Final Output**: `final_analytic_sample_with_infant_char_nas.csv`

---

## Phase 2: Statistical Analysis (Scripts 15-30)

All scripts read from Script 15's core output and create analysis results.

| Script | Input | Output Type | Purpose |
|--------|-------|-------------|---------|
| 15 | Script 14 output | CSV with weights/PS | **Core:** Propensity scores, IPW weights |
| 16 | Script 15 | CSV table | Summary statistics by race |
| 17 | Script 15 | PDF figure | Appendix Figure A2 |
| 18 | Script 15 | PDF figure | Figure 4 (main paper) |
| 19 | Script 15 | PDF figure | Figures 2-3 (main paper) |
| 20 | Script 15 | LaTeX table | Appendix Tables A1-A2 (decomposition) |
| 21 | Script 15 | LaTeX table | Table 2 (PS-weighted decomposition) |
| 22 | Script 15 | PDF figures | Figure 5 (year-by-year decomposition) |
| 23 | Script 15 | PDF figures | Appendix Figures A2-A3 (county mortality) |
| 24 | Script 15 | LaTeX table | Appendix Table A2 (model specs) |
| 25 | Script 15 | CSV table | Appendix Table A5 (pre/post COVID) |
| 26 | Script 15 + COVID data | PDF figures | Appendix Figures A4-A5 (COVID correlation) |
| 27 | Script 15 + NVSS | CSV table | Appendix Table A6 (NVSS comparison) |
| 28 | Script 15 | PDF figures | Appendix Figures A6-A7 (PS overlap) |
| 30 | Script 15 | Results | Appendix Table A7 (sensitivity analysis) |

---

## Phase 3: Maternal Drug Testing (Scripts 31-34)

Sequential pipeline to analyze maternal-infant drug testing correlations.

```
31. Find Drug LOINC Codes
    ↓
32. Filter Maternal Lab Data (2019-2023)
    ↓
33. Consolidate into Single File (linked to Script 15 sample)
    ↓
34. Create Appendix Figures A8-A13 (maternal vs infant correlations)
```

| Script | Input | Output | Key Variables |
|--------|-------|--------|---|
| 31 | LOINC file | `loinc_drug_codes_moms.csv` | Drug test LOINC codes |
| 32 | Script 31 + Lab data | `lab_Moms_[2019-2023].csv` (5 files) | Filtered maternal labs by year |
| 33 | Script 32 + Script 15 | `lab_moms_collapse.csv` | Maternal drug tests collapsed by person/date |
| 34 | Script 33 + Script 15 | 6 PDF figures | Testing periods: before/during/after pregnancy |

---

## Semantic Configuration Variables

All scripts use these naming conventions in configuration sections:

**Single Input/Output:**
```r
prev_script_output <- ".."        # Input from previous script
current_output_main <- ".."       # Primary output of this script
```

**Multiple Inputs:**
```r
prev_script_output_main <- ".."      # Primary input
prev_script_output_aux <- ".."       # Secondary input
input_*_dir / input_*_file <- ".."   # Raw data inputs
output_dir <- ".."                   # Output directory
```

All paths use ".." as placeholders—fill in actual paths once per script.

---

## How to Run

1. Fill in actual file paths (replace ".." in each script)
2. Run scripts sequentially in phase order: 1-14 → 15 → 16-30 → 31-34
3. Verify each script creates expected output before running next script
4. Check upstream outputs if a script fails

---

## Key Outputs by Type

**Tables (LaTeX/CSV)**: Scripts 16, 20, 21, 24, 25, 27, 30
**Figures (PDF)**: Scripts 17, 18, 19, 22, 23, 26, 28, 34

*Last updated: February 9, 2026*
