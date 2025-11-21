# residual_plot

Plot histogram of residual (`y`)

## Usage

``` r
residual_hist(y_type = "cwres", data, meta_data = NULL, bins = 21)
```

## Arguments

- y_type:

  Type of y variable

- data:

  A data.frame of data

- meta_data:

  A data.frame of meta data

- bins:

  Number of bins

## Examples

``` r
residual_hist(
y_type = "cwres",
data = data_501,
meta_data = meta_data_501,
bins = 11
)

```
