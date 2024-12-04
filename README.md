# nonmem-utils

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

Shiny App to preview and investigate output files exported by Nonmem.

> [!CAUTION]
> This repository is currently under development

> [!IMPORTANT]
> The Shiny Apps below require the following packages to be installed on RStudio

```r
install.packages(c("dplyr", "ggplot2", "shinydashboard", "plotly", "mrgsolve"))
```

## Model Diagnostics

The Shiny App below is a Swiss Army Knife that allows you to plot various diagnostics including:

- Goodness of Fit (histograms of residuals, obs vs pred, res vs pred/time, etc.)
- Covariate Analysis (histograms of covariate, correlation plots with smoothers)
- Review simulated individual PK profiles

```r
shiny::runGitHub(repo = "shiny-nonmem-viewers", username = "pchelle", ref = "main")
```

## Reviewing Optimization Profile

When estimating a model parameters, Nonmem outputs an `.ext` file that summarizes for each iteration (`ITERATION`) the values of the Objective Function (`OBJ`) and parameters.
The Shiny App below allows users to read such `.ext` files and preview the parameters profiles during the optimization.

```r
shiny::runGitHub(repo = "shiny-nonmem-viewers", subdir = "optimization-profile", username = "pchelle", ref = "main")
```

## Reviewing Model Correlations

```r
shiny::runGitHub(repo = "shiny-nonmem-viewers", subdir = "correlation-matrix", username = "pchelle", ref = "main")
```
