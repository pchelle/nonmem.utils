# vpc_plots

Generate VPC plots for a given dataset.

## Usage

``` r
vpc_plots(data, meta_data, x = "TAD", bins = 7, ci = 0.8, lloq = 10)
```

## Arguments

- data:

  A data.frame of data

- meta_data:

  A data.frame of meta data

- x:

  The x variable to use for VPC, default is "TAD"

- bins:

  Number of bins to use for VPC, default is 7

- ci:

  Confidence interval for the VPC plot, default is 0.8

- lloq:

  Lower limit of quantification, default is 10
