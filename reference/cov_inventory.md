# cov_inventory

Get a list of data.frames summarizing covariate data

## Usage

``` r
cov_inventory(data, meta_data)
```

## Arguments

- data:

  A data.frame of the pk data

- meta_data:

  A data.frame of meta data

## Value

A list of data.frames summarizing the covariate data

## Examples

``` r
cov_inventories <- cov_inventory(data_501, meta_data_501)

cov_inventories$All
#> # A tibble: 6 × 3
#>   Statistics `Weight [kg]` `Age [yrs]`
#>   <chr>              <dbl>       <dbl>
#> 1 N                   60          60  
#> 2 Mean                76.9        50.8
#> 3 Median              77.4        51  
#> 4 SD                  13.6        12.4
#> 5 Min                 50.3        25  
#> 6 Max                123          75  

cov_inventories[["SEX : Female"]]
#> # A tibble: 6 × 3
#>   Statistics `Weight [kg]` `Age [yrs]`
#>   <chr>              <dbl>       <dbl>
#> 1 N                   28          28  
#> 2 Mean                80.1        50.6
#> 3 Median              79.3        53  
#> 4 SD                  14.1        14.9
#> 5 Min                 58.4        25  
#> 6 Max                123          75  

cov_inventories[["SEX : Male"]]
#> # A tibble: 6 × 3
#>   Statistics `Weight [kg]` `Age [yrs]`
#>   <chr>              <dbl>       <dbl>
#> 1 N                   32          32  
#> 2 Mean                74.2        50.9
#> 3 Median              72.0        48.5
#> 4 SD                  12.8        10.0
#> 5 Min                 50.3        38  
#> 6 Max                 96.3        72  
```
