# cov_cor

Get a data.frame of Spearman correlation between covariates

## Usage

``` r
cov_cor(data, meta_data)
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

cov_cor(sum_data, meta_data_501)
#>   Covariates                                            Weight
#> 1     Weight                                                  
#> 2        Age                                  0.207 (p: 0.113)
#> 3        Sex Male: 74.15 (p< 0.001)<br>Female: 5.91 (p: 0.094)
#>                                                  Age
#> 1                                                   
#> 2                                                   
#> 3 Male: 50.91 (p< 0.001)<br>Female: -0.26 (p: 0.936)
```
