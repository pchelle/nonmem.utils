# Covariate Analysis (vignette: `covariate-analysis.Rmd`)

Identify and quantify factors that explain variability in PK parameters.

## Preamble

Covariate analysis requires `ETA` variable(s) in the NONMEM result file (e.g. `.tab` /
`.par`). Declare them in the dictionary as `Type: eta`:

| Name | Type | Label   | Unit |
|------|------|---------|------|
| ETA1 | eta  | ηCL     |      |
| ETA2 | eta  | ηV      |      |

## Interactive analysis

```r
nonmem.utils::run_shiny("covariate-analysis")
```

## Quarto report

```r
nonmem.utils::report_covariate_analysis(
  data_path = "my/dataset.tab",
  meta_data_path = "my/dictionary.csv"
)
```

> Note: `report_covariate_analysis()` is referenced by the vignette; the export set in
> v0.1.0 exposes `report_gof_analysis`, `report_dataset_analysis`, `report_vpc_analysis`,
> `report_limited_sampling_analysis`, and `report_workflow_summary`. Prefer the Shiny app
> or compose figures/tables manually if the report function is unavailable.

## Available analyses

### Eta distribution and correlation

```r
eta_plot(data_501, meta_data_501)   # pairs plot of etas (list: All + per cat)
```

### Covariates vs etas

Two functions:

- **`eta_cov_plot(data, meta_data)`** — graphical analysis of covariate distributions and
  their correlation with etas (matrix of plots; list: `All` + per `cat`).
- **`eta_cor(data, meta_data)`** — table with:
  - Spearman correlation test results between **continuous** covariates and etas;
  - ANOVA test results between **categorical** covariates and etas.

  Pair with `highlight_significant()` to colour significant cells:
  ```r
  eta_cor(data_501, meta_data_501) |>
    highlight_significant() |>
    knitr::kable()
  ```
