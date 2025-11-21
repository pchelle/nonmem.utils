# highlight_significant

Add color for significant cell

## Usage

``` r
highlight_significant(data, pval = 0.05, color = "green", format = "html")
```

## Arguments

- data:

  A data.frame

- pval:

  Significance threshold for p-value

- color:

  Color when significant

- format:

  `"typst"`, `"html"`

## See also

Other report:
[`report_dataset_analysis()`](https://pchelle.github.io/nonmem.utils/reference/report_dataset_analysis.md),
[`report_gof_analysis()`](https://pchelle.github.io/nonmem.utils/reference/report_gof_analysis.md),
[`report_limited_sampling_analysis()`](https://pchelle.github.io/nonmem.utils/reference/report_limited_sampling_analysis.md),
[`report_vpc_analysis()`](https://pchelle.github.io/nonmem.utils/reference/report_vpc_analysis.md),
[`report_workflow_summary()`](https://pchelle.github.io/nonmem.utils/reference/report_workflow_summary.md),
[`typst_scientific()`](https://pchelle.github.io/nonmem.utils/reference/typst_scientific.md),
[`typst_table()`](https://pchelle.github.io/nonmem.utils/reference/typst_table.md)

## Examples

``` r
cor_data <- cov_cor(data_501, meta_data_501)
cor_data
#>   Covariates                                            Weight
#> 1     Weight                                                  
#> 2        Age                                  0.207 (p: 0.113)
#> 3        Sex Male: 74.15 (p< 0.001)<br>Female: 5.91 (p: 0.094)
#>                                                  Age
#> 1                                                   
#> 2                                                   
#> 3 Male: 50.91 (p< 0.001)<br>Female: -0.26 (p: 0.936)

cor_data |> highlight_significant(pval = 0.1, format = "html")
#>   Covariates
#> 1     Weight
#> 2        Age
#> 3        Sex
#>                                                                                          Weight
#> 1                                                                                              
#> 2                                                                              0.207 (p: 0.113)
#> 3 <font color="green"><strong>Male: 74.15 (p< 0.001)<br>Female: 5.91 (p: 0.094)</strong></font>
#>                                                  Age
#> 1                                                   
#> 2                                                   
#> 3 Male: 50.91 (p< 0.001)<br>Female: -0.26 (p: 0.936)

cor_data |> highlight_significant(pval = 0.1, format = "typst")
#>   Covariates                                                            Weight
#> 1     Weight                                                                  
#> 2        Age                                                  0.207 (p: 0.113)
#> 3        Sex #text(green)[*Male: 74.15 (p< 0.001)<br>Female: 5.91 (p: 0.094)*]
#>                                                  Age
#> 1                                                   
#> 2                                                   
#> 3 Male: 50.91 (p< 0.001)<br>Female: -0.26 (p: 0.936)

cor_data |> highlight_significant(pval = 0.1, format = "docx")
#>   Covariates                                                Weight
#> 1     Weight                                                      
#> 2        Age                                      0.207 (p: 0.113)
#> 3        Sex **Male: 74.15 (p< 0.001)<br>Female: 5.91 (p: 0.094)**
#>                                                  Age
#> 1                                                   
#> 2                                                   
#> 3 Male: 50.91 (p< 0.001)<br>Female: -0.26 (p: 0.936)
```
