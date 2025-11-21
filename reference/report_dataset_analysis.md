# report_dataset_analysis

Render a docx document that analyses a dataset

## Usage

``` r
report_dataset_analysis(
  data_path,
  meta_data_path,
  report_path = "dataset-analysis.docx",
  bins = 7
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

## See also

Other report:
[`highlight_significant()`](https://pchelle.github.io/nonmem.utils/reference/highlight_significant.md),
[`report_gof_analysis()`](https://pchelle.github.io/nonmem.utils/reference/report_gof_analysis.md),
[`report_limited_sampling_analysis()`](https://pchelle.github.io/nonmem.utils/reference/report_limited_sampling_analysis.md),
[`report_vpc_analysis()`](https://pchelle.github.io/nonmem.utils/reference/report_vpc_analysis.md),
[`report_workflow_summary()`](https://pchelle.github.io/nonmem.utils/reference/report_workflow_summary.md),
[`typst_scientific()`](https://pchelle.github.io/nonmem.utils/reference/typst_scientific.md),
[`typst_table()`](https://pchelle.github.io/nonmem.utils/reference/typst_table.md)
