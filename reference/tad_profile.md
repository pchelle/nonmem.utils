# tad_profile

Plot Time After Dose (TAD) vs DV

## Usage

``` r
tad_profile(data, meta_data = NULL, bins = 7)
```

## Arguments

- data:

  A data.frame of data

- meta_data:

  A data.frame of meta data

- bins:

  Number of bins to use for VPC, default is 7

## Examples

``` r
# Requires BLQ and LLOQ
tp_meta <- dplyr::bind_rows(
  meta_data_501,
  data.frame(Name = c("BLQ", "LLOQ"), Type = c("blq", "lloq"), Label = c("BLQ", "LLOQ"))
)
tp_data <- data_501 |> dplyr::mutate(BLQ = 0, LLOQ = 1)

# Get the time profile plots
tp_plots <- tad_profile(tp_data, tp_meta)
#> Warning: Ignoring unknown aesthetics: text
#> Warning: Ignoring unknown aesthetics: text
#> Warning: Ignoring unknown aesthetics: text
#> Warning: Ignoring unknown aesthetics: text
#> Warning: Ignoring unknown aesthetics: text
#> Warning: Ignoring unknown aesthetics: text
#> Warning: Ignoring unknown aesthetics: text
#> Warning: Ignoring unknown aesthetics: text
#> Warning: Ignoring unknown aesthetics: text
#> Warning: Ignoring unknown aesthetics: text

# Plots are split by categorical variables, all use them all
names(tp_plots)
#> [1] "SEX" "All"

# DV in linear scale
tp_plots$All$Linear


# DV in log scale
tp_plots$All$Log


# Data split by categorical covariate
tp_plots$SEX$Linear

```
