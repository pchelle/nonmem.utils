# Get Started (vignette: `nonmem-utils.Rmd`)

`nonmem.utils` provides helpers for reporting PopPK modeling performed with NONMEM. The
core concept: a dictionary `data.frame` (`meta_data`) maps each dataset variable
(`data`) to its label, unit, type, and expected range.

## The two inputs

### `data` — the NONMEM dataset (data.frame)

```r
# Input dataset (.csv)
data <- readr::read_csv("path/to/your/data.csv")

# NONMEM output table (.tab, 2-line header)
data <- readr::read_table("path/to/your/data.tab", skip = 1)
```

### `meta_data` — the dictionary (data.frame)

Columns: `Name`, `Type`, `Label`, `Unit`, `Min`, `Max`.

- `Type` (lower-case): `id`, `occ`, `time`, `tad`, `dv`, `amt`, `evid`, `mdv`;
  `cov` (continuous covariate), `cat` (categorical covariate), `eta` (BSV/BOV).
- For categorical covariates, encode value→label mapping in `Label` as JSON joined by
  pipes, parsed by `jsonlite`: `"0":"male"|"1":"female"`.
- Tip: defining multiple dictionaries can be useful when many covariates are present.

Example datasets shipped with the package: `data_501` and `meta_data_501`.

## Minimal example

Once `data` and `meta_data` are defined they populate graphical and tabular results:

```r
library(nonmem.utils)

cov_cor(data_501, meta_data_501)   # correlation table
cov_plot(data_501, meta_data_501)  # pairs plot
```

## Available analyses

- Dataset Analysis and Summary → `dataset-analysis`
- Covariate Analysis → `covariate-analysis`
- Goodness of Fit Analysis → `gof-analysis`

## Shiny toolbox

```r
nonmem.utils::shiny_toolbox()   # also in the RStudio Addins menu
```

Pick a tool, click **Done**, and the matching `nonmem.utils::…()` call is inserted into
the console.

## Reporting

The package supports `html`, `pdf` (Typst), and `docx` output. Default reports:

```r
nonmem.utils::report_dataset_analysis(data_path, meta_data_path)
nonmem.utils::report_gof_analysis(data_path, meta_data_path)
nonmem.utils::report_vpc_analysis(data_path, meta_data_path)
nonmem.utils::report_workflow_summary(dir_path)
```
