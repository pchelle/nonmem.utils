---
name: covariate-analysis
description: >
  Assess ETA distributions and ETA-covariate relationships in nonmem.utils using
  plots, association tables, and optional Omega summaries from .res files.
metadata:
  source_files:
    - inst/quarto/covariate-analysis.qmd
    - vignettes/covariate-analysis.Rmd
  audience: users-and-agents
---

# Covariate Analysis

Use this skill when a user asks to evaluate covariate effects on PK variability
using ETA diagnostics and covariate association summaries.

## Required inputs

- `data_path`: NONMEM results (`.tab`/`.par`) or compatible `.csv`
- `meta_data_path`: dictionary `.csv`
- ETA variables declared in dictionary with `Type = "eta"`
- Optional `res_path`: NONMEM `.res` file for Omega summaries

## Preflight checks

1. Confirm files exist and are readable.
2. Confirm dictionary has required columns (`Name`, `Type`, `Label`, `Unit`).
3. Hard fail if no `eta` variables are declared.
4. Warn if no `cov`/`cat` variables exist; run ETA-only diagnostics.
5. If `res_path` is provided, attempt `nonmem_res(res_path)`; warn if unavailable.

## Workflow

1. Load data and dictionary.
2. Run ETA structure diagnostics:
   - `eta_plot(data, meta_data)`
3. Run ETA-vs-covariate diagnostics:
   - `eta_cov_plot(data, meta_data)`
   - `eta_cor(data, meta_data)`
4. Optional Omega summary (if `.res` available):
   - Parse with `nonmem_res(res_path)` and summarize Omega estimates.
5. Optional interactive path: `nonmem.utils::run_shiny("covariate-analysis")`.
6. Optional report path: render `inst/quarto/covariate-analysis.qmd` directly.

## Expected outputs

- ETA pair/distribution plots
- ETA vs covariate faceted diagnostics
- Correlation/ANOVA summary table between ETAs and covariates
- Optional Omega summary table

## Failure handling

- Missing ETA declarations in metadata: hard fail.
- Missing optional `.res` or failed parse: warning and continue without Omega table.
- No covariates: warning and provide ETA-only section.

## Minimal command examples

```r
library(nonmem.utils)
data <- readr::read_table("run001.tab", skip = 1)
meta_data <- readr::read_csv("dictionary.csv")

eta_plot(data, meta_data)
eta_cor(data, meta_data)
nonmem.utils::run_shiny("covariate-analysis")
```
