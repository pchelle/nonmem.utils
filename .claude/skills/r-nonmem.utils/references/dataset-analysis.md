# Dataset Analysis (vignette: `dataset-analysis.Rmd`)

Summarise and visualise a PopPK **input** dataset.

## Interactive analysis

```r
nonmem.utils::run_shiny("dataset-analysis")
```

## Quarto report

```r
nonmem.utils::report_dataset_analysis(
  data_path = "my/dataset.csv",
  meta_data_path = "my/dictionary.csv"
)
```

## Available analyses

### Inventories

Three functions summarise available PK data:

- **`data_inventory(data, meta_data)`** — number of subjects, PK studies (ID×OCC),
  doses, observations, and percent BLQ, plus per-subject/per-study ratios.
  Returns a **named list**; `"All"` is the overall table; one extra table per `cat`
  variable (columns = categories).
  ```r
  pk_summary <- data_inventory(data_501, meta_data_501)
  ```

- **`cov_inventory(data, meta_data)`** — summary statistics (N, Mean, Median, SD, Min,
  Max) of continuous covariates. Returns a **named list**; `"All"` plus one table per
  `cat` level (`"{CAT} : {level}"`).

- **`cat_inventory(data, meta_data)`** — count and percent per category for every `cat`
  variable (single `data.frame`, side by side).

### Time profiles

`time_profile()` and `tad_profile()` plot `dv` vs `time`/`tad` with median, 5th and 95th
percentile profiles. Both take a `bins` argument (default 7).

Both return a **nested list**: first level = `"All"` + one entry per `cat` variable;
each entry has `"Linear"`, `"Log"`, and `"Percent BLQ"` plots (faceted by `cat`).

```r
tp_plots <- time_profile(data_501, meta_data_501)
tp_plots$All$Linear
tp_plots$All$Log
tp_plots$SEX$Linear
```

### Covariates distribution and correlation

(See `covariate-analysis` reference for `cov_plot()` and `cov_cor()`.)
