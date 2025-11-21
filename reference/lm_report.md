# lm_report

Report of lm coefficients with p-value If a categorical variable is
used, should be as `x` argument

## Usage

``` r
lm_report(data, x, y)
```

## Arguments

- data:

  A data.frame

- x:

  name of `x` variable (categorical variable)

- y:

  name of `y` variable (continuous variable)

## Value

A character of lm coefficients and p-value

## Examples

``` r
# Summarize data by ID
sum_data <- data_501 |>
  dplyr::group_by(ID) |>
  dplyr::summarise_all(dplyr::first)

# Map categorical data
sum_data <- map_cat_data(sum_data, meta_data_501)

# ANOVA correlation with p-value
# Categorical variable, SEX, as x argument
lm_report(sum_data, x = "SEX", y = "WT")
#> [1] "Male: 74.15 (p< 0.001)<br>Female: 5.91 (p: 0.094)"
```
