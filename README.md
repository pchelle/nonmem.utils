
# nonmem.utils

<!-- badges: start -->
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of `{nonmem.utils}` is to provide a toolbox that includes helper functions, shiny apps and quarto template reports to help viewing, investigating and reporting Nonmem.

> [!CAUTION]
> This repository is currently under development

## Installation

You can install the development version of nonmem.utils like so:

``` r
remotes::install_github("pchelle/nonmem.utils")
```

## Example

Run the following code to open the toolbox (also available in the RStudio Addins menu):

``` r
nonmem.utils::shiny_toolbox()
```

Select the desired tool from the toolbox (for instance, covariate-analysis) and click on the button <button>Done</button>.

The appropriate code will be added to the R console to start the corresponding tool.

## Usage

Most of the functions use the following arguments `data` and `meta_data`.

### `data`

The `data` argument is a data.frame that contains the data to be analyzed.

In Nonmem, the input datasets are usually provided as `.csv` files. 
In such cases, the tools will read them using `readr::read_csv()`.

```r
data <- readr::read_csv("path/to/your/data.csv")
```

Nonmem output files are usually provided as `.tab` files with 2 lines header. 
In such cases, the tools will read them using `readr::read_table()`.

```r
data <- readr::read_table("path/to/your/data.tab", skip = 1)
```

### `meta_data`

The `meta_data` argument is a dictionary data.frame that contains the information about the dataset.

The `meta_data` is expected to be provided as `.csv` files.
The tools will read them using `readr::read_csv()`.

```r
meta_data <- readr::read_csv("path/to/your/dictionary.csv")
```

It should contain the following columns:

- __`Name`__: name of the dataset variable
- __`Type`__: type of the dataset variables in lower cases.
  - The type can be one of the following: `id`, `occ`, `time`, `tad`, `dv`, `amt`, `evid`, `mdv`.
  - If the variable is a continuous covariate, use `cov`.
  - If the variable is a categorical covariate, use `cat`.
  - If the variable is a model BSV/BOV parameters, use `eta`.

- __`Label`__: displayed label of the dataset variable
- __`Unit`__: displayed unit of the dataset variable
  - For categorical covariates (Type is `cat`), see explanations and example below.
  
- __`Min`__: the minimum expected value of the dataset variable
- __`Max`__: the maximum expected value of the dataset variable

Mapping between the categorical covariate values and labels can be provided in the __`Label`__ column.
The `jsonlite` package is leveraged to parse the mapping.
As a consequence, the mapping is expected to be provided as __`"value":"label"`__ split by pipes `|`.

For instance, to map a variable named `sex` with `0` coding for _male_ and `1` coding for _female_, 
the Label cell should be have the following value:

```json
"0":"male"|"1":"female"
```
