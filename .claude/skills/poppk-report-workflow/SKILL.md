---
name: poppk-report-workflow
description: >
  Orchestrate dataset, GOF, and covariate analyses into a consistent PopPK
  reporting workflow using nonmem.utils functions, shiny apps, and report templates.
metadata:
  source_files:
    - inst/quarto/dataset-analysis-docx.qmd
    - inst/quarto/gof-analysis.qmd
    - inst/quarto/covariate-analysis.qmd
    - vignettes/dataset-analysis.Rmd
    - vignettes/gof-analysis.Rmd
    - vignettes/covariate-analysis.Rmd
  audience: users-and-agents
---

# PopPK Report Workflow

Use this skill when a user asks for a full model-evaluation workflow combining
input-data quality checks, model-fit diagnostics, and covariate-effect review.

## Required inputs

- Dataset/results file(s): `.csv`, `.tab`, optionally `.par`
- Dictionary `.csv` (`Name`, `Type`, `Label`, `Unit`)
- Optional `.res` for Omega context
- Optional report output preferences (docx/pdf/typst)

## Preflight checks

1. Validate dataset and dictionary file readability.
2. Validate key dictionary typing (`id`, `dv`, `time`/`tad`, `eta`, `cov`, `cat`).
3. Validate GOF columns (`PRED`, `IPRED`, `CWRES`) when GOF is requested.
4. Track optional fields (`NPDE`, `.res`) and gracefully degrade if absent.

## Workflow

1. Run **dataset analysis** skill.
2. Run **gof analysis** skill.
3. Run **covariate analysis** skill.
4. Consolidate outputs into narrative sections:
   - Data integrity and representativeness
   - Structural/predictive fit
   - Variability drivers and candidate covariates
5. Produce artifacts via:
   - Interactive apps (`run_shiny(...)`) for exploration
   - Report functions (`report_dataset_analysis`, `report_gof_analysis`)
   - Direct quarto rendering for covariate report template if needed

## Expected outputs

- Reproducible sequence of analyses
- Consistent table/figure set across projects
- Consolidated reporting artifacts ready for review

## Failure handling

- Stop on missing required core inputs.
- Continue with warnings on optional diagnostics unavailable (e.g., NPDE, `.res`).
- Surface clear remediation actions before retrying.

## Minimal command examples

```r
nonmem.utils::report_dataset_analysis("dataset.csv", "dictionary.csv")
nonmem.utils::report_gof_analysis("run001.tab", "dictionary.csv")
nonmem.utils::run_shiny("covariate-analysis")
```
