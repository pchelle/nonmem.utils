# nonmem.utils — API Reference

Complete function reference for `nonmem.utils` (v0.1.0). A toolbox of functions, Shiny
apps, and Quarto reports to view, investigate, and report NONMEM (PopPK) results.

> **Read this before writing code.** Most functions take `data` (a NONMEM dataset) and
> `meta_data` (a dictionary `data.frame`). See "Core concepts" below.

## Core concepts

### The `meta_data` dictionary (REQUIRED input for most functions)

A `data.frame` (usually read from a CSV via `readr::read_csv()`). Required columns:

| Column  | Purpose |
|---------|---------|
| `Name`  | Exact dataset column name (case-sensitive) |
| `Type`  | Lower-case variable type (see below) |
| `Label` | Display label (for `cat`, JSON `"value":"label"` mappings joined by `\|`) |
| `Unit`  | Display unit (ignored for `cat`) |
| `Min`   | Expected minimum (used by `check_ranges()`, `pull_limits()`, `gg_lim()`) |
| `Max`   | Expected maximum |

**`Type` values:** `id`, `occ`, `time`, `tad`, `dv`, `amt`, `evid`, `mdv`, `cov`
(continuous covariate), `cat` (categorical covariate), `eta` (BSV/BOV parameter),
`blq` (below-limit-of-quantification flag), `lloq` (lower limit of quantification).

Categorical mapping in `Label` uses JSON parsed by `jsonlite`, joined by pipes:
`"0":"male"|"1":"female"`. A template lives at `system.file("template-dictionary.csv", package = "nonmem.utils")`.

### Auto-filling of NONMEM variables

`fill_meta_vars()` fills missing required `Type` entries with NONMEM defaults (`ID`,
`TIME`, `MDV`, `EVID`, `BLQ`, `LLOQ`, …). `fill_nonmem_vars(data, lloq = 10, group = "All")`
adds missing `ID`, `AMT`, `EVID`, `OCC`, `MDV`, `LLOQ`, `OBS`, `PRED`, `IPRED`, `BLQ`,
`CWRES`, `NPDE`, `REP` columns with safe defaults. Most plotting functions call these
internally, so optional NONMEM columns can be omitted.

### Reading NONMEM files

- Input datasets (`.csv`): `readr::read_csv("path.csv")`
- NONMEM output tables (`.tab`, 2-line header): `readr::read_table("path.tab", skip = 1)`
- NONMEM result files (`.res`): `nonmem_res("path.res")`

### List-return convention

Several functions return a **named list** indexed first by categorical split (`"All"` +
one entry per `cat` variable), then by sub-type. Always index defensively, e.g.
`result$All` / `result$All$Linear`.

### Built-in example data

- `data_501` — NONMEM tutorial (Bergstrand) dataset 501.
- `meta_data_501` — matching dictionary.

---

## Figures — Goodness-of-fit (`figures-gof.R`)

All return `ggplot` objects; `theme_bw()` base. Pass `as_plotly = TRUE` to add a `text`
aesthetic for plotly tooltips (use `plotly::ggplotly(p, tooltip = "text")`). BLQ
observations (`BLQ > 0`) are shown as rugs at `LLOQ`.

### `dv_preds(data, meta_data = NULL, as_plotly = FALSE)`
DV vs PRED **and** IPRED (both populations), with identity line, loess smooths, and a
colored legend (`Population`/`Individual`). Most common GOF plot.

### `dv_pred(data, meta_data = NULL, as_plotly = FALSE)`
DV vs population predictions (`PRED`) only.

### `dv_ipred(data, meta_data = NULL, as_plotly = FALSE)`
DV vs individual predictions (`IPRED`) only.

### `residual_plot(x_type = "time", y_type = "cwres", data, meta_data = NULL, as_plotly = FALSE)`
Residual `y_type` vs `x_type`. `x_type` accepts meta-data types (`time`, `pred`,
`ipred`, …); `y_type` is a residual name lower-cased, upper-cased internally
(`"cwres"`, `"wres"`, `"npde"`). Adds horizontal zero line + loess.
```r
residual_plot("time", "cwres", data_501, meta_data_501)
residual_plot("pred", "cwres", data_501, meta_data_501)
```

### `residual_qq(y_type = "cwres", data, meta_data = NULL)`
Normal QQ-plot of residual `y_type` with reference line.

### `residual_hist(y_type = "cwres", data, meta_data = NULL, bins = 21)`
Histogram of residual `y_type` overlaid with a fitted normal density (mean/SD of data),
plus zero and mean reference lines. `bins` controls the histogram bin count.

---

## Figures — Covariate analysis (`figures-covariate.R`)

`eta_plot()` and `eta_cov_plot()` return a **named list** (`All` + one plot per `cat`
variable); `cov_plot()` returns a single plot. **All three return GGally `ggmatrix`
objects**, not plain `ggplot` — so `is.ggplot()` is FALSE and `patchwork::wrap_plots()`
/ `print()` is the idiomatic way to display them. They summarise data to one row per
`id` (`summarise_all(first)`).

### `eta_plot(data, meta_data = NULL)`
Pairs plot (`GGally::ggpairs`) of `eta` variables: robust correlations (upper),
loess smooths (lower), histograms with mean/zero lines (diag). Colored by each `cat`.
Requires `eta` Type in `meta_data`. Returns early with `cli::cli_alert_danger()` if no
`id`/`eta`.

### `eta_cov_plot(data, meta_data = NULL)`
Matrix (`GGally::ggduo`) of `eta` variables (Y) vs `cov` + `cat` variables (X):
loess+lm for continuous, boxplots for categorical. Horizontal zero line.

### `cov_plot(data, meta_data = NULL)`
Pairs plot of `cov` + `cat` variables: correlations (upper), loess (lower), histograms
(diag). Returns a GGally `ggmatrix` (not a plain `ggplot`).

### `boot_hist(y_type = "theta", data, meta_data = NULL, bins = 11, ci = 0.95)`
Histograms of **bootstrap** estimates. `y_type` one of `"theta"`, `"omega"`, `"sigma"`.
Returns a **named list** of one ggplot per parameter.
- `meta_data` for bootstrap needs columns: `Name`, `Value` (final estimate),
  `Label`, `Unit` (and must match `data` column names).
- For `"sigma"` with `CV` units, square-roots and scales by 100.
- For `"omega"`, diagonal (`omega11`, `omega22`, …) converted to CV%; off-diagonal
  converted to correlations using `omegaXY / sqrt(omegaXX * omegaYY)`.
```r
boot_hist("theta", boot_data, boot_meta)        # list -> patchwork::wrap_plots()
boot_hist("omega", boot_data, boot_meta)
```

---

## Figures — Time profiles (`figures-time-profile.R`)

`time_profile()` and `tad_profile()` return a **nested list**: `[split][c("Linear","Log","Percent BLQ")]`,
where `split` is `"All"` plus one entry per `cat` variable. `ind_*_profiles()` return a
**list** of facet pages (one per `n_rows * n_cols` IDs).

### `time_profile(data, meta_data = NULL, bins = 7, as_plotly = FALSE)`
`dv` vs `time` with observed points + median / 5th / 95th percentile lines
(computed via `vpc_summary()` over `bins`), faceted by `cat`. Includes a Percent-BLQ line
plot. Requires `blq`/`lloq` for BLQ features (auto-filled if absent).

### `tad_profile(data, meta_data = NULL, bins = 7, as_plotly = FALSE)`
Same as `time_profile()` but vs `tad` (time after dose).

### `ind_time_profiles(data, meta_data = NULL, n_rows = 2, n_cols = 3, as_plotly = FALSE)`
Individual DV / PRED / IPRED vs `time`, simulated with `simulate_pk()`. PK model
(1/1-abs/2/3-cmt) chosen from available params (`KA`, `V2`, `V3`). Paginated into facet
pages of `n_rows * n_cols` IDs. **Input must include PK parameter columns**
(e.g. `CL`, `V`) for simulation.

### `ind_tad_profiles(data, meta_data = NULL, n_rows = 2, n_cols = 3, as_plotly = FALSE)`
Same as `ind_time_profiles()` but vs `tad`.

---

## Tables — Inventory (`tables-inventory.R`)

### `data_inventory(data, meta_data)` → **named list** of `data.frame`s
Counts Subjects, Studies (ID×OCC), Doses (`EVID %in% c(1,4)`), Observations
(`MDV == 0`), Percent BLQ, plus per-subject/per-study ratios. `"All"` entry is a
2-column (Data/All) table; one extra entry per `cat` variable (columns = categories).
Handles missing `occ`/`evid`/`blq` gracefully.

### `cov_inventory(data, meta_data)` → **named list** of `data.frame`s
Continuous covariate summary statistics (N, Mean, Median, SD, Min, Max) per subject.
`"All"` plus one entry per `cat` level, named `"{CAT} : {level}"`.

### `cat_inventory(data, meta_data)` → single `data.frame`
Count and Percent per category, for every `cat` variable, side by side (padded with NA).

---

## Tables — Correlation (`tables-correlation.R`)

### `cov_cor(data, meta_data)` → `data.frame`
Symmetric matrix of **continuous-covariate** Spearman correlations and
**categorical-vs-continuous** lm coefficient summaries (cell text like
`"0.123 (p: 0.456)"`). Data is collapsed to one row per `id` first.

### `eta_cor(data, meta_data)` → `data.frame`
Rows = covariates (`cov` + `cat`), columns = `eta`s. Spearman for continuous,
ANOVA/lm summaries for categorical. Pair with `highlight_significant()`.

---

## Tables — VPC (`tables-vpc.R`)

### `vpc_summary(data, x, y, group = NULL, bins = 5, stairstep = FALSE, ci = 0.9)` → `data.frame`
Generic binning summary used by time profiles: columns `bins, x, n, y, ymin, ymax, blq`
(median + `ci` quantiles of `y` per bin; `blq` = % of `y > 0`). `bins` may be a scalar
count or explicit edges. `stairstep = TRUE` duplicates rows at bin edges.

---

## Meta-data utilities (`utilities-meta-data.R`)

### `dictionary_check(data, meta_data)` → `invisible(NULL)`
`cli` warnings listing `meta_data$Name` missing from `data` and vice versa.

### `fill_meta_vars(meta_data = NULL)` → `data.frame`
Appends default `Type` rows (`id`, `occ`, `time`, `tad`, `evid`, `mdv`, `blq`, `lloq`)
when absent. Returns `NULL` if `meta_data` is `NULL`.

### `fill_nonmem_vars(data, lloq = 10, group = "All")` → `data.frame`
Adds missing NONMEM columns (`ID` from `CID`, `AMT`, `EVID`, `OCC`, `MDV`, `LLOQ`,
`OBS/PRED/IPRED`, `BLQ/CWRES/NPDE`, `REP`, `group`) with safe defaults.

### `pull_name(type, meta_data = NULL)` → character
Dataset column `Name`(s) matching a `Type` (e.g. `pull_name("cov", meta_data)`).

### `pull_label(name, meta_data = NULL)` → character
Display label(s) `"Label [Unit]"` for `Name`(s); units dropped for `cat`. Preserves
input order; unescapes Unicode.

### `pull_limits(name, meta_data = NULL)` → `c(Min, Max)`
Expected range for `Name`; NA → ±Inf.

### `pull_cat(name, meta_data = NULL)` → `data.frame(value, label)`
Parsed categorical mapping from the `Unit` column (JSON `"value":"label"` joined by `|`).
Note: categorical mapping is encoded in `Unit`, not `Label`.

### `map_cat_data(data, meta_data)` → `data.frame`
Converts every `cat` variable to a labelled `factor` (levels = coded values,
labels = decoded labels). Used internally by most plotting functions.

### `check_ranges(data, meta_data)` → `data.frame`
Augments `meta_data` with `Data Min`, `Data Max`, and `Out of Range` (count) per
variable. DV ranges ignore `MDV == 1` / `BLQ > 0`; AMT ranges use dose rows only.

> **Caveat:** unlike most functions, `check_ranges()` does **not** auto-fill `blq`/`mdv`.
> It will error (`attempt to select less than one element`) if those `Type`s are absent
> from `meta_data`. Ensure `meta_data` declares `blq` (and `mdv`) — or append a
> `BLQ`/`MDV` row — before calling.

---

## NONMEM run utilities (`utilities-nonmem-runs.R`)

### `nonmem_res(res_file)` → **list**
Parses a NONMEM `.res` (LST-style) text file. Fields: `Name`, `Date`, `Subjects`,
`Observations`, `n_theta`, `n_omega`, `n_sigma`, `Method`, `Min` (logical),
`Cov` (logical), `Evaluations`, `Significant Digits`, `OFV`, `AIC`, `BIC`,
`Estimation Time [s]`, `Covariance Time [s]`, `Theta`/`Omega`/`Sigma` (each a list
of `Name, Initial, Lower, Upper, Estimates, RSE`, and `Shrinkage` for Omega/Sigma).
AIC = OFV + 2·n_par; BIC = OFV + Obs·n_par.

### `cov_to_cor(data, cov_name = "Estimates")` → `data.frame`
Converts a lower-triangular covariance vector to correlations + adds `CV`
(=sqrt variance on diagonal). Infers dimension from row count.

### `format_result(nonmem_result)` → `data.frame`
Flattens a `nonmem_res()` list to one printable row: Theta/Omega/Sigma collapsed to
multi-line character strings with estimates, RSE, CV%, and shrinkage.

---

## NONMEM / PK utilities (`utilities-nonmem.R`)

### `simulate_pk(data)` → `data.frame`
Simulates DV from PK parameters via **mrgsolve**. Auto-selects model from column names:
`V3`→3-cmt, `V2`→2-cmt, `KA`→1-cmt-absorb, else 1-cmt (infusion). **`data` must contain
PK parameter columns** (`CL`, `V`, …). Returns one row per simulated time point.

### `calc_tad(id, time, amt = NULL, evid = NULL, rate = NULL, dur = NULL)` → numeric
Computes time-after-dose vector. Strategies by available inputs:
- only `time`: `time - time[1]` per `id`.
- `evid` only: resets at doses (`evid %in% c(1,4)`), occasions via `evid > 3`.
- `amt` only: doses = non-NA positive `amt`.
- `rate` or `dur`: offsets by infusion duration (time after end of infusion).
Records before first dose return negative TAD.

---

## VPC utilities (`utilities-vpc.R`, `tables-vpc.R`)

### `bin_values(values, bins = 7)` → `factor`
Quantile-based binning. If ≤ `bins` unique values, returns them as factor levels;
otherwise `cut()` on unique quantile breaks.

### `run_vpc(data, x = "TIME", group = "All", bins = 7, ci = 0.8, lloq = 10)` → **list**
Runs prediction-corrected / prediction-variability-corrected VPC. Computes `pcDV`,
`pcOBS`, `pvcDV`, `pvcOBS`, then summarizes per `REP`/`Bins`/`group`. Returns a named
list with one `data.frame` per `vpc_type` plus `data` (REP==1 subset). Requires `REP`
(repetition/replicate) column for simulation-based VPC. Uses `cli` progress steps.

### `vpc_plots(data, meta_data, x = "TAD", bins = 7, ci = 0.8, lloq = 10)` → **named list** of ggplot
One VPC plot per `vpc_types` (`vpc`, `pc_vpc`, `pvc_vpc`, `blq`, `npde`). Applies
`gg_lim()` on the x-axis. Currently produces the `"All"` (ungrouped) set only.

### `vpc_types` — exported character vector: `c("vpc", "pc_vpc", "pvc_vpc", "blq", "npde")`.

---

## Figure utilities (`utilities-figures.R`)

### `gg_log(plot_object, x = TRUE, y = TRUE)` → ggplot
Pretty log10 axes (`scales::log_breaks()`, `label_log()`, log-tick guide). No-op if both
`x` and `y` are `FALSE`.

### `plotly_log(plotly_object, x = TRUE, y = TRUE)` → plotly
Log scale for plotly via `plotly::layout(xaxis/yaxis = list(type = "log"))`.

### `gg_lim(plot_object, meta_data, x = NULL, y = NULL)` → ggplot
Applies `Min`/`Max` from `meta_data` via `coord_cartesian()` for the named `x`/`y`.

### `tooltip_text(data, var_names)` → character
HTML tooltip string (`<b>VAR</b>: value<br>…`) for plotly `text` aesthetics.

---

## Table / stats utilities (`utilities-tables.R`)

### `cor_report(data, x, y)` → character
Spearman correlation + p-value string, e.g. `"0.234 (p: 0.012)"` / `"(p< 0.001)"`.

### `lm_report(data, x, y)` → character
`lm(y ~ x)` coefficient summary string (one term per `<br>`). **`x` should be the
categorical (`cat`) variable.** Returns intercept/level estimates + p-values.

---

## Shiny apps (`utilities-apps.R`)

Bundled apps (folders under `inst/shiny/`): `covariate-analysis`, `dataset-analysis`,
`gof-analysis`, `limited-sampling-analysis`, `optimization-profile`, `vpc-analysis`,
`workflow-summary`.

### `run_shiny(app_name)`
Runs a bundled Shiny app via `shiny::runApp(system.file("shiny", app_name, …))`.
Example: `nonmem.utils::run_shiny("gof-analysis")`.

### `shiny_toolbox()`
Interactive RStudio **gadget** (miniUI) to pick an app or report, select input files via
`rstudioapi`, and insert the matching `nonmem.utils::…()` call into the console. Also
exposed as an RStudio Addin. Requires `shiny`, `miniUI`, `rstudioapi`.

---

## Report rendering (`utilities-rendering.R`)

All `report_*()` copy the bundled Quarto `.qmd` to a temp dir, render to `.docx` via
`rmarkdown::render()` with `params`, and copy the output to `report_path`. Return
`TRUE`/`FALSE` (file exists). Input datasets are read with `readr`.

### `report_dataset_analysis(data_path, meta_data_path, report_path = "dataset-analysis.docx", bins = 7)`
### `report_gof_analysis(data_path, meta_data_path, report_path = "gof-analysis.docx")`
### `report_vpc_analysis(data_path, meta_data_path, report_path = "vpc-analysis.docx", bins = 7, ci = 0.8)`
### `report_limited_sampling_analysis(model_path, data_path, sampling_path = "Sampling-Strategies.xlsx", run_folder = "LSA_Runs", pop_size = 1000, pop_cv = 0.025, covariates = c("BW", "AGE", "FFM"), categoricals = NULL, lloq = 0.01, baseline = 0.005, y_lim = 50, report_path = "limited-sampling-analysis.docx")`
Limited-sampling analysis. `model_path` is a `.cpp` mrgsolve model; `sampling_path` an
`.xlsx`; sampling template at `system.file("Sampling-Strategies.xlsx", package = "nonmem.utils")`.
### `report_workflow_summary(dir_path, report_path = "workflow-summary.docx")`
Summarizes a directory of NONMEM runs (uses `nonmem_res()` over the folder).

### `typst_table(data, linebreak = "<br>")` → character vector
Renders a `data.frame` as raw Typst `#table(...)` (for Quarto→PDF). Bold headers;
`<br>` substituted with Typst line breaks.

### `typst_scientific(values, digit = 2)` → character
Formats numbers as Typst scientific notation, e.g. `$1.23 dot 10^(4)$`.

### `highlight_significant(data, pval = 0.05, color = "green", format = "html")` → `data.frame`
Parses p-values out of cell text (via internal `parse_pvalue`, patterns `(p: …)` /
`(p< …)`) and styles cells ≤ `pval`. `format` one of `"html"` (colored bold),
`"docx"` (`**bold**`), `"typst"` (`#text(color)[*…*]`).
```r
eta_cor(data_501, meta_data_501) |> highlight_significant(pval = 0.05, format = "html")
```

---

## Data

### `data_501` — data.frame, NONMEM tutorial dataset 501.
### `meta_data_501` — data.frame, dictionary for `data_501` (defines `SEX` as `cat`, etc.).

---

## Dependencies (relevant to usage)

Imports: `cli`, `dplyr`, `tidyr`, `ggplot2`, `GGally`, `mrgsolve`, `plotly`, `scales`,
`stringi`, `jsonlite`, `rmarkdown`. Suggests (for apps/reports): `bslib`, `DT`,
`officedown`, `miniUI`, `rstudioapi`, `shinydashboard`, `shinyWidgets`, `patchwork`,
`ragg`. Requires R ≥ 4.3.
