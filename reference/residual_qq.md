# residual_qq

Plot qq-plot of residuals (`y`)

## Usage

``` r
residual_qq(y_type = "cwres", data, meta_data = NULL)
```

## Arguments

- y_type:

  Type of y variable

- data:

  A data.frame of data

- meta_data:

  A data.frame of meta data

## Examples

``` r
residual_qq(
y_type = "cwres",
data = data_501,
meta_data = meta_data_501
)

```
