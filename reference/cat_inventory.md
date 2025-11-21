# cat_inventory

Get a data.frame summarizing categorical data

## Usage

``` r
cat_inventory(data, meta_data)
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
cat_inventories <- cat_inventory(data_501, meta_data_501)

cat_inventories
#> # A tibble: 2 × 3
#>   Sex    Count Percent
#>   <fct>  <int>   <dbl>
#> 1 Male      32    53.3
#> 2 Female    28    46.7
```
