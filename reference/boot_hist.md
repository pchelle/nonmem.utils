# boot_hist

Histogram of bootstrap estimated (`y`)

## Usage

``` r
boot_hist(y_type = "theta", data, meta_data = NULL, bins = 11, ci = 0.95)
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

- ci:

  Confidence interval

## Examples

``` r
# Mimicking bootstrap results obtained through nonmem
boot_data <- data.frame(
  theta1 = stats::rlnorm(1e3, meanlog = log(2), sdlog = 1),
  theta2 = stats::rlnorm(1e3, meanlog = log(10), sdlog = 0.5),
  omega11 = stats::runif(1e3),
  sigma11 = stats::rlnorm(1e3, meanlog = 0, sdlog = 2)
)

boot_meta <- data.frame(
  Name = c("theta1", "theta2", "omega11", "sigma11"),
  # Estimated values assessed by bootstrap
  Value = c(2, 10, 0.5, 1),
  Label = c("Clearance", "Volume", "BSV", "RUV: SD"),
  Unit = c("L/h", "L", "%", "mg/L")
)

# Fixed effects
p_theta <- boot_hist(y_type = "theta", data = boot_data, meta_data = boot_meta)
patchwork::wrap_plots(p_theta)


# Random effects
p_omega <- boot_hist(y_type = "omega", data = boot_data, meta_data = boot_meta)
p_sigma <- boot_hist(y_type = "sigma", data = boot_data, meta_data = boot_meta)
patchwork::wrap_plots(c(p_omega, p_sigma))

```
