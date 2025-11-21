# eta_cor

Get a data.frame of

- Spearman correlation between covariates and etas

- Wilcoxon test between categorical covariates and etas

## Usage

``` r
eta_cor(data, meta_data)
```

## Arguments

- data:

  A data.frame of the pk data

- meta_data:

  A data.frame of meta data

## Value

A data.frame summarizing the data

## Examples

``` r
# Summarize data by ID
sum_data <- data_501 |>
  dplyr::group_by(ID) |>
  dplyr::summarise_all(dplyr::first)

eta_cor(sum_data, meta_data_501)
#>   Covariates                                              ηCL
#> 1     Weight                                -0.035 (p: 0.793)
#> 2        Age                                 0.004 (p: 0.974)
#> 3        Sex Male: 0.00 (p: 0.885)<br>Female: 0.00 (p: 0.986)
#>                                                  ηV
#> 1                                  0.045 (p: 0.733)
#> 2                                 -0.015 (p: 0.908)
#> 3 Male: -0.01 (p: 0.763)<br>Female: 0.00 (p: 0.990)
```
