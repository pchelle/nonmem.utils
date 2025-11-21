# report_dataset_analysis

Render a docx document that analyses a dataset

## Usage

``` r
report_vpc_analysis(
  data_path,
  meta_data_path,
  report_path = "vpc-analysis.docx",
  bins = 7,
  ci = 0.8
)
```

## Arguments

- data_path:

  Path to the data file

- meta_data_path:

  Path to the metadata file

- report_path:

  Path to the output report file

- bins:

  Number of bins for time profile plots

- ci:

  Confidence interval for the analysis

## See also

Other report:
[`highlight_significant()`](https://pchelle.github.io/nonmem.utils/reference/highlight_significant.md),
[`report_dataset_analysis()`](https://pchelle.github.io/nonmem.utils/reference/report_dataset_analysis.md),
[`report_gof_analysis()`](https://pchelle.github.io/nonmem.utils/reference/report_gof_analysis.md),
[`report_limited_sampling_analysis()`](https://pchelle.github.io/nonmem.utils/reference/report_limited_sampling_analysis.md),
[`report_workflow_summary()`](https://pchelle.github.io/nonmem.utils/reference/report_workflow_summary.md),
[`typst_scientific()`](https://pchelle.github.io/nonmem.utils/reference/typst_scientific.md),
[`typst_table()`](https://pchelle.github.io/nonmem.utils/reference/typst_table.md)
