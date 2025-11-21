# typst_scientific

Add characters to render number into scientific writing

## Usage

``` r
typst_scientific(values, digit = 2)
```

## Arguments

- values:

  Values to render into scientific format

- digit:

  Number of digits

## See also

Other report:
[`highlight_significant()`](https://pchelle.github.io/nonmem.utils/reference/highlight_significant.md),
[`report_dataset_analysis()`](https://pchelle.github.io/nonmem.utils/reference/report_dataset_analysis.md),
[`report_gof_analysis()`](https://pchelle.github.io/nonmem.utils/reference/report_gof_analysis.md),
[`report_limited_sampling_analysis()`](https://pchelle.github.io/nonmem.utils/reference/report_limited_sampling_analysis.md),
[`report_vpc_analysis()`](https://pchelle.github.io/nonmem.utils/reference/report_vpc_analysis.md),
[`report_workflow_summary()`](https://pchelle.github.io/nonmem.utils/reference/report_workflow_summary.md),
[`typst_table()`](https://pchelle.github.io/nonmem.utils/reference/typst_table.md)

## Examples

``` r
typst_scientific(stats::rlnorm(5))
#> [1] "$3.63 dot 10^(-01)$" "$6.49 dot 10^(-01)$" "$1.41 dot 10^(00)$" 
#> [4] "$1.21 dot 10^(00)$"  "$3.62 dot 10^(00)$" 
```
