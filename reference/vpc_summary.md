# vpc_summary

Get a data.frame summarizing data as a vpc

## Usage

``` r
vpc_summary(data, x, y, group = NULL, bins = 5, stairstep = FALSE, ci = 0.9)
```

## Arguments

- data:

  A data.frame of the pk data

- x:

  Name of `x` variable

- y:

  Name of `y` variable

- group:

  Name of `group` variable

- bins:

  Number of bins if one value is provided. Or edges of bins if an array
  is provided. Default binning uses number of rows/10

- stairstep:

  Logical defining if VPC data.frame correspond to stairstep plot

- ci:

  Confidence interval (value between 0 and 1)

## Value

A data.frame summarizing the data distribution with variables: `bins`,
`x`, `n`, `y`, `ymin`, `ymax`, and `blq`

## Examples

``` r
vpc_summary(
  data = data_501 |> dplyr::filter(MDV == 0),
  x = "TIME",
  y = "DV",
  bins = 7
)
#> # A tibble: 4 × 8
#>   bins  group     n     x     y  ymin  ymax   blq
#>   <fct> <chr> <int> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 1     ""       60     1 11.0   3.46  31.4   100
#> 2 5     ""       60     5 43.1  15.0  147.    100
#> 3 12    ""       60    12 23.2   8.61  62.0   100
#> 4 24    ""       60    24  8.11  2.02  23.4   100
```
