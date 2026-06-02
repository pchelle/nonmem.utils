---
name: dataset-analysis
description: >
  Analyze PopPK datasets with nonmem.utils using inventory summaries, covariate
  summaries, time/TAD profiles, and covariate correlation outputs.
metadata:
  source_files:
    - inst/quarto/dataset-analysis-docx.qmd
    - inst/quarto/dataset-analysis-pdf.qmd
    - vignettes/dataset-analysis.Rmd
  audience: users-and-agents
---

# Dataset Analysis

Use this skill when a user asks to explore PK dataset structure, summarize
covariates, inspect concentration-time patterns, or generate a dataset report.

## Required inputs

- `data_path`: `.csv` (or `.tab`) dataset file
- `meta_data_path`: dictionary `.csv` with `Name`, `Type`, `Label`, `Unit`
- Optional: `bins` for profile summaries (default: 7 or 11 depending workflow)

## Preflight checks

1. Confirm files exist and are readable.
2. Confirm metadata includes required columns (`Name`, `Type`, `Label`, `Unit`).
3. Confirm metadata has required types for analysis (`id`, `time` or `tad`, `dv`).
4. Warn (do not fail) when optional covariate types are absent (`cov`, `cat`).

## Workflow

1. Load data and dictionary.
2. Run inventories:
   - `data_inventory(data, meta_data)`
   - `cov_inventory(data, meta_data)`
   - `cat_inventory(data, meta_data)`
3. Run profile diagnostics:
   - `time_profile(data, meta_data, bins = bins)`
   - `tad_profile(data, meta_data, bins = bins)`
4. Run covariate structure diagnostics:
   - `cov_plot(data, meta_data)`
   - `cov_cor(data, meta_data)`
5. Optional interactive path: `nonmem.utils::run_shiny("dataset-analysis")`.
6. Optional report path: `nonmem.utils::report_dataset_analysis(...)`.

## Expected outputs

- Inventory tables (overall and by categorical stratification)
- Continuous and categorical covariate summaries
- Time/TAD plots (linear, log, BLQ percent)
- Covariate pair plots
- Correlation/ANOVA summary tables
- Optional docx report

## Failure handling

- Missing dictionary columns or key variable types: hard fail with remediation.
- Missing optional covariate columns/types: warning and continue.
- Invalid categorical mapping labels: warn and continue with raw values.

## Minimal command examples

```r
library(nonmem.utils)
data <- readr::read_csv("dataset.csv")
meta_data <- readr::read_csv("dictionary.csv")

data_inventory(data, meta_data)
time_profile(data, meta_data, bins = 11)
report_dataset_analysis("dataset.csv", "dictionary.csv")
```
