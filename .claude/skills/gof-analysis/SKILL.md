---
name: gof-analysis
description: >
  Evaluate PopPK model goodness-of-fit in nonmem.utils using observed-vs-predicted
  plots, residual diagnostics, and individual profile checks.
metadata:
  source_files:
    - inst/quarto/gof-analysis.qmd
    - vignettes/gof-analysis.Rmd
  audience: users-and-agents
---

# GOF Analysis

Use this skill when a user asks to assess model fit quality from NONMEM output
files (`.tab` or compatible) and diagnostic variables.

## Required inputs

- `data_path`: NONMEM results (`.tab`) or compatible `.csv`
- `meta_data_path`: dictionary `.csv`
- Required diagnostic columns in data: `PRED`, `IPRED`, `CWRES`
- Optional diagnostic column: `NPDE`

## Preflight checks

1. Confirm files exist and are readable.
2. Confirm dictionary has minimal required columns (`Name`, `Type`, `Label`, `Unit`).
3. Hard fail if `PRED`, `IPRED`, or `CWRES` are missing.
4. Warn if `NPDE` is missing; skip NPDE-specific diagnostics.

## Workflow

1. Load results and dictionary.
2. Run observed-vs-predicted diagnostics:
   - `dv_preds(data, meta_data)`
   - Optional log scaling with `gg_log()`
3. Run residual diagnostics:
   - `residual_hist(y_type = "cwres", ...)`
   - `residual_qq(y_type = "cwres", ...)`
   - `residual_plot(x_type = "time"|"tad"|"pred"|"ipred", y_type = "cwres", ...)`
4. If NPDE exists, repeat residual diagnostics for `y_type = "npde"`.
5. Run individual profile checks:
   - `ind_time_profiles(data, meta_data)`
   - `ind_tad_profiles(data, meta_data)`
6. Optional interactive path: `nonmem.utils::run_shiny("gof-analysis")`.
7. Optional report path: `nonmem.utils::report_gof_analysis(...)`.

## Expected outputs

- DV vs PRED/IPRED plots (linear and optional log)
- CWRES diagnostics (histogram, QQ, trend plots)
- NPDE diagnostics when available
- Individual profile diagnostic plots
- Optional docx report

## Failure handling

- Missing required GOF columns: hard fail with clear missing-column message.
- Missing NPDE: warning and continue.
- Sparse or filtered data causing unstable diagnostics: warn and continue.

## Minimal command examples

```r
library(nonmem.utils)
data <- readr::read_table("run001.tab", skip = 1)
meta_data <- readr::read_csv("dictionary.csv")

dv_preds(data, meta_data)
residual_plot(x_type = "pred", y_type = "cwres", data, meta_data)
report_gof_analysis("run001.tab", "dictionary.csv")
```
