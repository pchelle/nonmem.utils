# report_limited_sampling_analysis

Render a docx document that report limited sampling analyses results

## Usage

``` r
report_limited_sampling_analysis(
  model_path,
  data_path,
  sampling_path = "Sampling-Strategies.xlsx",
  run_folder = "LSA_Runs",
  pop_size = 1000,
  pop_cv = 0.025,
  covariates = c("BW", "AGE", "FFM"),
  categoricals = NULL,
  lloq = 0.01,
  baseline = 0.005,
  y_lim = 50,
  report_path = "limited-sampling-analysis.docx"
)
```

## Arguments

- model_path:

  Path to the `.cpp` model file

- data_path:

  Path to the data file

- sampling_path:

  Path to the sampling strategies `.xslx` file. Template file available
  at `system.file("Sampling-Strategies.xlsx", package = "nonmem.utils")`

- run_folder:

  Path to the folder where the runs will be stored

- pop_size:

  Population size for the analysis

- pop_cv:

  Coefficient of variation for the population

- covariates:

  Covariate names to be used in the analysis

- categoricals:

  Categorical covariate names to be used in the analysis

- lloq:

  Lower limit of quantification for the analysis

- baseline:

  Baseline value for the analysis

- y_lim:

  Y-axis limits for the plots

- report_path:

  Path to the output report file

## See also

Other report:
[`highlight_significant()`](https://pchelle.github.io/nonmem.utils/reference/highlight_significant.md),
[`report_dataset_analysis()`](https://pchelle.github.io/nonmem.utils/reference/report_dataset_analysis.md),
[`report_gof_analysis()`](https://pchelle.github.io/nonmem.utils/reference/report_gof_analysis.md),
[`report_vpc_analysis()`](https://pchelle.github.io/nonmem.utils/reference/report_vpc_analysis.md),
[`report_workflow_summary()`](https://pchelle.github.io/nonmem.utils/reference/report_workflow_summary.md),
[`typst_scientific()`](https://pchelle.github.io/nonmem.utils/reference/typst_scientific.md),
[`typst_table()`](https://pchelle.github.io/nonmem.utils/reference/typst_table.md)
