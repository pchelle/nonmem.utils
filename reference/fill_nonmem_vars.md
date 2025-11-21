# fill_nonmem_vars

Fill in the nonmem variables that are not present in the data

## Usage

``` r
fill_nonmem_vars(data, lloq = 10, group = "All")
```

## Arguments

- data:

  A data.frame of the pk data

- lloq:

  Lower limit of quantification (default is 10)

- group:

  Name of the grouping variable

## Value

A data.frame with the nonmem variables filled in
