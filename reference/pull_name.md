# pull_name

Pull Name from data `Type` field

## Usage

``` r
pull_name(type, meta_data = NULL)
```

## Arguments

- type:

  Type of variable to retrieve

- meta_data:

  A data.frame of meta data

## Examples

``` r
# Dictionary
meta_data_501
#>    Name  Type                      Label                    Unit Min Max
#> 1    ID    id                    Subject                    <NA>  NA  NA
#> 2   OCC   occ                   Occasion                    <NA>  NA  NA
#> 3  TIME  time                       Time                       h  NA  NA
#> 4   TAD   tad            Time After Dose                       h  NA  NA
#> 5    DV    dv              Concentration                    mg/L  NA  NA
#> 6  EVID  evid           Event Identifier                    <NA>  NA  NA
#> 7   MDV   mdv Missing Dependent Variable                    <NA>  NA  NA
#> 8   AMT   amt                     Amount                      mg  NA  NA
#> 9  RATE  rate                       Rate                    mg/h  NA  NA
#> 10   WT   cov                     Weight                      kg  NA  NA
#> 11  AGE   cov                        Age                     yrs  NA  NA
#> 12  SEX   cat                        Sex "0":"Male"|"1":"Female"  NA  NA
#> 13   CL param                  Clearance                     L/h  NA  NA
#> 14    V param                     Volume                       L  NA  NA
#> 15 ETA1   eta                  Clearance                    <NA>  NA  NA
#> 16 ETA2   eta                     Volume                    <NA>  NA  NA

pull_name("time", meta_data_501)
#> [1] "TIME"

pull_name("cov", meta_data_501)
#> [1] "WT"  "AGE"
```
