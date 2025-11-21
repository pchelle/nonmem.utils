# cor_report

Report of Spearman correlation with p-value

## Usage

``` r
cor_report(data, x, y)
```

## Arguments

- data:

  A data.frame

- x:

  name of `x` variable

- y:

  name of `y` variable

## Value

A character of correlation and p-value

## Examples

``` r
# Summarize data by ID
sum_data <- data_501 |>
  dplyr::group_by(ID) |>
  dplyr::summarise_all(dplyr::first)

# Spearman correlation with p-value
cor_report(sum_data, x = "WT", y = "AGE")
#> [1] "0.207 (p: 0.113)"
```
