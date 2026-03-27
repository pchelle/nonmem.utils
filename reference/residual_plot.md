# residual_plot

Plot residual (`y`) vs `x`

## Usage

``` r
residual_plot(x_type = "time", y_type = "cwres", data, meta_data = NULL)
```

## Arguments

- x_type:

  Type of x variable

- y_type:

  Type of y variable

- data:

  A data.frame of data

- meta_data:

  A data.frame of meta data

## Examples

``` r
residual_plot(
x_type = "time",
y_type = "cwres",
data = data_501,
meta_data = meta_data_501
)
#> Warning: pseudoinverse used at 0.885
#> Warning: neighborhood radius 11.115
#> Warning: reciprocal condition number  8.2288e-17
#> Warning: There are other near singularities as well. 365.38

```
