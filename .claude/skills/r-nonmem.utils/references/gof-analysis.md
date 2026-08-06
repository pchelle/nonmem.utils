# GOF Analysis (vignette: `gof-analysis.Rmd`)

Goodness-of-fit (GOF) analysis evaluates how well a PopPK model describes observed data.

## Preamble

The NONMEM result file (e.g. `.tab`) must include usual variables such as `PRED`,
`IPRED`, `CWRES` and `NPDE`. These do **not** need to be declared in `meta_data` —
`PRED`/`IPRED` use the `dv` type for labelling.

## Interactive analysis

```r
nonmem.utils::run_shiny("gof-analysis")
```

## Quarto report

```r
nonmem.utils::report_gof_analysis(
  data_path = "my/dataset.tab",
  meta_data_path = "my/dictionary.csv"
)
```

## Available analyses

### Observed vs predicted

`dv_preds()` plots observed vs individual **and** population predictions in linear and
log scales:

```r
dv_preds(data_501, meta_data_501)

# log-log scale via the helper
dv_preds(data_501, meta_data_501) |> gg_log()
```

(`dv_pred()` and `dv_ipred()` are the population-only and individual-only variants.)

### Residuals

Three residual functions:

- `residual_hist()` — histogram of residuals
  ```r
  residual_hist(y_type = "cwres", data = data_501, meta_data = meta_data_501)
  ```
- `residual_qq()` — QQ-plot of residuals
  ```r
  residual_qq(y_type = "cwres", data = data_501, meta_data = meta_data_501)
  ```
- `residual_plot()` — residuals vs another variable to inspect trends
  ```r
  patchwork::wrap_plots(
    residual_plot(x_type = "time", y_type = "cwres", data = data_501, meta_data = meta_data_501),
    residual_plot(x_type = "pred", y_type = "cwres", data = data_501, meta_data = meta_data_501)
  )
  ```

### Individual time profiles

`ind_time_profiles()` and `ind_tad_profiles()` review individual profiles. They leverage
1-, 2-, and 3-compartment models via `mrgsolve` and NONMEM PK-parameter naming (e.g. if
the dataset has only `CL` and `V`, a 1-compartment model is assumed). **Input must
include PK parameter columns** for simulation.

```r
ind_time_profiles(
  data = data_501 |> dplyr::filter(ID <= 6),
  meta_data = meta_data_501
)
```
