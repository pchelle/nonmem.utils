# typst_table

Print a table in raw typst for pdf document rendering

## Usage

``` r
typst_table(data, linebreak = "<br>")
```

## Arguments

- data:

  A data.frame to render

- linebreak:

  Line break pattern to be subbed by typst line break

## See also

Other report:
[`highlight_significant()`](https://pchelle.github.io/nonmem.utils/reference/highlight_significant.md),
[`report_dataset_analysis()`](https://pchelle.github.io/nonmem.utils/reference/report_dataset_analysis.md),
[`report_gof_analysis()`](https://pchelle.github.io/nonmem.utils/reference/report_gof_analysis.md),
[`report_limited_sampling_analysis()`](https://pchelle.github.io/nonmem.utils/reference/report_limited_sampling_analysis.md),
[`report_vpc_analysis()`](https://pchelle.github.io/nonmem.utils/reference/report_vpc_analysis.md),
[`report_workflow_summary()`](https://pchelle.github.io/nonmem.utils/reference/report_workflow_summary.md),
[`typst_scientific()`](https://pchelle.github.io/nonmem.utils/reference/typst_scientific.md)

## Examples

``` r
data_with_linebreak <- data.frame(Example = c("$10^2$<br>$-->$", "#sym.checkmark"))
typst_table(data_with_linebreak)
#>  [1] "```{=typst}"                "#table("                   
#>  [3] "  stroke: 1pt,"             "  align: horizon,"         
#>  [5] "  columns: 1,"              "  table.header[*Example*],"
#>  [7] "  [$10^2$ \\ $-->$], "      "  [#sym.checkmark], "      
#>  [9] ")"                          "```"                       
```
