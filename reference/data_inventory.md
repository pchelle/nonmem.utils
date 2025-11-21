# data_inventory

Get a list of data.frames summarizing data

## Usage

``` r
data_inventory(data, meta_data)
```

## Arguments

- data:

  A data.frame of the pk data

- meta_data:

  A data.frame of meta data

## Value

A list of data.frame summarizing the data

## Examples

``` r
all_inventories <- data_inventory(data_501, meta_data_501)

# Since meta_data_501 identifies SEX as cat covariate
# inventory has a field split by Sex categories
names(all_inventories)
#> [1] "Sex" "All"

all_inventories$All
#>                       Data All
#> 1                 Subjects  60
#> 2                  Studies  60
#> 3                    Doses  60
#> 4             Observations 240
#> 5      Studies per Subject   1
#> 6        Doses per Subject   1
#> 7          Doses per Study   1
#> 8 Observations per Subject   4
#> 9   Observations per Study   4

all_inventories$Sex
#>                        Sex Male Female
#> 1                 Subjects   32     28
#> 2                  Studies   32     28
#> 3                    Doses   32     28
#> 4             Observations  128    112
#> 5      Studies per Subject    1      1
#> 6        Doses per Subject    1      1
#> 7          Doses per Study    1      1
#> 8 Observations per Subject    4      4
#> 9   Observations per Study    4      4

```
