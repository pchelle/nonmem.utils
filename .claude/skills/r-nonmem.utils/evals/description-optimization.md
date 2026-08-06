# Description Optimization — r-nonmem.utils

Performed after functionality was verified (with_skill pass_rate = 100%).

## Required-element checklist (from references/description-optimization.md)

| Element | Status |
|---------|--------|
| `library({pkg})` recognition token | ✅ `library(nonmem.utils)` |
| `{pkg}::` recognition token | ✅ `nonmem.utils::` |
| File extensions the package works with | ✅ `.tab / .csv / .res` |
| Domain-specific problem language | ✅ NONMEM, GOF, covariate, dataset, VPC, Shiny, Quarto |

## Sibling-skill differentiation

This repo already ships task-oriented workflow skills whose names overlap by domain:
- `gof-analysis` — "Evaluate PopPK model goodness-of-fit in nonmem.utils …"
- `covariate-analysis` — "Assess ETA distributions and ETA-covariate relationships …"
- `dataset-analysis` — "Analyze PopPK datasets with nonmem.utils …"
- `poppk-report-workflow` — orchestration

Those trigger on the **analysis task**. `r-nonmem.utils` triggers on **package code**
(`library(nonmem.utils)` / `nonmem.utils::`) and serves the **function-level API +
gotchas**. The description states this boundary explicitly ("Distinct from the
task-oriented … skills: this is the function-level API + gotchas reference") so a
query like "evaluate GOF" loads the workflow skill, while "what args does
residual_plot take" / `nonmem.utils::dv_preds()` loads this one.

## Validation method

A full 20-query train/held-out trigger loop requires a harness that measures whether a
skill loads for a given query (run each query 3×, compute trigger rate). That harness is
not available in this environment, so the description was validated by the recognition-
token checklist above and sibling-differentiation review rather than held-out scoring.

## Decision

Keep current description. It satisfies every required element, leads with the package-
name tokens (the strongest discriminator), and is explicitly scoped against the sibling
workflow skills.
