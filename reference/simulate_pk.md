# simulate_pk

Simulate PK from PK data. Currently, only 1-, 2- and 3-compartment
infusion models are supported.

## Usage

``` r
simulate_pk(data)
```

## Arguments

- data:

  A data.frame of PK data.

## Value

A data.frame with simulated PK data.

## Examples

``` r
head(simulate_pk(data_501 |> dplyr::filter(ID == 1)))
#>   ID TIME tad  CENTRAL        DV
#> 1  1  0.0 0.0  0.00000 0.0000000
#> 2  1  0.1 0.1 19.91422 0.6988722
#> 3  1  0.2 0.2 39.65785 1.3917578
#> 4  1  0.3 0.3 59.23235 2.0787081
#> 5  1  0.4 0.4 78.63918 2.7597739
#> 6  1  0.5 0.5 97.87976 3.4350056
```
