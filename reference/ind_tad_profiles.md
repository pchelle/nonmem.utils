# ind_tad_profiles

Plot DV, PRED and IPRED vs TAD

## Usage

``` r
ind_tad_profiles(data, meta_data = NULL, n_rows = 2, n_cols = 3)
```

## Arguments

- data:

  A data.frame of data

- meta_data:

  A data.frame of meta data

- n_rows:

  Number of rows in the plot grid, default is 2

- n_cols:

  Number of columns in the plot grid, default is 3

## Examples

``` r
# Simulate 1-compartment model
pk_data <- data_501 |>
  dplyr::mutate(CL = 2, V = 40)
ind_tad_profiles(pk_data, meta_data_501)
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
#> [[1]]

#> 
#> [[2]]

#> 
#> [[3]]

#> 
#> [[4]]

#> 
#> [[5]]

#> 
#> [[6]]

#> 
#> [[7]]

#> 
#> [[8]]

#> 
#> [[9]]

#> 
#> [[10]]

#> 
```
