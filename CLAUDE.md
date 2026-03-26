# nonmem.utils

## What This Repository Is

`nonmem.utils` is an R package providing a toolbox of functions, Shiny
apps, and Quarto template reports to view, investigate, and report
NONMEM (nonlinear mixed effects modeling software) results. It is
primarily used in pharmacometrics and clinical pharmacology workflows.

## Package Structure

    R/                          # R source code
      figures-covariate.R       # Covariate analysis plots
      figures-gof.R             # Goodness-of-fit plots
      figures-time-profile.R    # Time profile plots
      globals.R                 # Package globals
      tables-correlation.R      # Correlation tables
      tables-inventory.R        # Inventory tables
      tables-vpc.R              # VPC tables
      utilities-apps.R          # Shiny app utilities
      utilities-figures.R       # Figure utilities
      utilities-meta-data.R     # Metadata utilities
      utilities-nonmem-runs.R   # NONMEM run utilities
      utilities-nonmem.R        # NONMEM utilities
      utilities-rendering.R     # Rendering utilities
      utilities-tables.R        # Table utilities
      utilities-vpc.R           # VPC utilities
    inst/
      shiny/                    # Shiny app source files
        covariate-analysis/
        dataset-analysis/
        gof-analysis/
        limited-sampling-analysis/
        optimization-profile/
        vpc-analysis/
        workflow-summary/
      quarto/                   # Quarto template reports
        covariate-analysis.qmd
        dataset-analysis.qmd
        gof-analysis.qmd
        limited-sampling-analysis.qmd
        vpc-analysis.qmd
        workflow-summary.qmd
      models/                   # Example NONMEM model files
      test/                     # Test data files
    man/                        # roxygen2-generated documentation
    tests/                      # testthat tests
    vignettes/                  # Package vignettes
    .github/workflows/          # GitHub Actions CI workflows

## Key Commands

``` bash
# Run all tests
Rscript -e "devtools::test()"

# Check the package
Rscript -e "devtools::check()"

# Redocument
Rscript -e "devtools::document()"

# Load package interactively
Rscript -e "devtools::load_all()"
```

## Skills

This project includes Claude Skills from
[posit-dev/skills](https://github.com/posit-dev/skills) to assist with
development, code review, testing, and CI. The following skills are
available in `.claude/skills/`:

| Skill                    | Purpose                                                     |
|--------------------------|-------------------------------------------------------------|
| `shiny-bslib`            | Build modern Shiny dashboards using bslib Bootstrap 5       |
| `shiny-bslib-theming`    | Advanced theming for Shiny apps with bslib                  |
| `r-package-development`  | R package development with devtools, testthat, and roxygen2 |
| `testing-r-packages`     | Best practices for R package testing with testthat 3+       |
| `cli`                    | CLI styling and messaging using the cli R package           |
| `critical-code-reviewer` | Rigorous adversarial code reviews                           |
| `describe-design`        | Create architectural documentation                          |

## Technology Stack

- **Language**: R (≥ 4.3)
- **Package development**: devtools, roxygen2, testthat, pkgdown
- **Shiny**: shiny, bslib, shinyWidgets, DT, miniUI, shinydashboard
- **Reporting**: Quarto, rmarkdown, officedown
- **Visualization**: ggplot2, plotly, GGally, patchwork
- **Data**: dplyr, tidyr, readr
- **Utilities**: cli, stringi, scales, jsonlite
- **Simulation**: mrgsolve
- **CI**: GitHub Actions (r-lib/actions)
