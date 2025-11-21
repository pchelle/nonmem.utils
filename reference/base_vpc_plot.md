# base_vpc_plot

Base vpc plot

## Usage

``` r
base_vpc_plot(
  data,
  vpc_data,
  variable_labels,
  x = "TAD",
  ci = 0.8,
  type = "vpc"
)
```

## Arguments

- data:

  A data.frame of data

- vpc_data:

  A data.frame of VPC data

- variable_labels:

  A list of variable labels

- ci:

  Confidence interval for the VPC plot

- type:

  Type of VPC plot, one of `"vpc"`, `"pc_vpc"`, `"pvc_vpc"`, `"blq"`,
  `"npde"`

## Value

A ggplot object
