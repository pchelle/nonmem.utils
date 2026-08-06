---
name: r-nonmem.utils
description: >
  Package-API reference for nonmem.utils. Use when code loads or uses nonmem.utils
  (library(nonmem.utils), nonmem.utils::) and you need exact function signatures,
  arguments, the `meta_data` dictionary convention, reading NONMEM .tab/.csv/.res
  files, or GOF / covariate / dataset / VPC figures, tables, Shiny apps, and Quarto
  reports. Distinct from the task-oriented gof-analysis / covariate-analysis /
  dataset-analysis skills: this is the function-level API + gotchas reference.
metadata:
  author: Pierre Chelle
  version: "0.1"
  source: local
license: Apache-2.0
---

# nonmem.utils — NONMEM/PopPK reporting toolbox

> **Read `references/API.md` before writing code.** It has every function signature,
> argument, return type, and the list-return conventions. Vignettes in `references/`
  (`get-started`, `dataset-analysis`, `covariate-analysis`, `gof-analysis`) show the
  canonical workflows.

## Mental model

Two inputs drive almost everything:
- `data` — a NONMEM dataset (`.csv` via `readr::read_csv`, `.tab` via `readr::read_table(skip = 1)`).
- `meta_data` — a **dictionary** `data.frame` with columns `Name`, `Type`, `Label`,
  `Unit`, `Min`, `Max`. `Type` ∈ {`id`,`occ`,`time`,`tad`,`dv`,`amt`,`evid`,`mdv`,
  `cov`,`cat`,`eta`,`blq`,`lloq`}. Categorical value→label mapping is JSON in `Label`:
  `"0":"male"|"1":"female"`. Missing NONMEM cols (`PRED`,`IPRED`,`MDV`,…) are auto-filled.

**Returns are often lists:** inventory/eta/eta-cov functions return `list(All = …, <cat> = …)`;
`time_profile()`/`tad_profile()` return `list(All = list("Linear","Log","Percent BLQ"), …)`.
Index with `$All` first.

## Quick Reference (opt = optional, adv = advanced)

| Task | Function | Key params |
|------|----------|-----------|
| DV vs PRED+IPRED | `dv_preds(data, meta_data, as_plotly=FALSE)` | `as_plotly` (opt) adds tooltip `text` |
| DV vs PRED / IPRED | `dv_pred()`, `dv_ipred()` | same signature |
| Residual vs x | `residual_plot(x_type="time", y_type="cwres", data, meta_data, as_plotly=FALSE)` | `x_type`∈{time,pred,ipred,…}; `y_type`∈{cwres,wres,npde} |
| Residual QQ / hist | `residual_qq(y_type, data, meta_data)`; `residual_hist(y_type, data, meta_data, bins=21)` | `bins` (opt) |
| Eta pairs | `eta_plot(data, meta_data)` → list | needs `eta` Type; plots are GGally `ggmatrix` |
| Eta vs covariates | `eta_cov_plot(data, meta_data)` → list | plots are `ggmatrix` |
| Covariate pairs | `cov_plot(data, meta_data)` → `ggmatrix` | not a plain `ggplot` |
| Eta–covariate table | `eta_cor(data, meta_data)` → df | → `highlight_significant(pval=0.05, format="html")` |
| Bootstrap hist | `boot_hist(y_type="theta", data, meta_data, bins=11, ci=0.95)` → list | `y_type`∈{theta,omega,sigma}; `meta_data` needs `Value` |
| Data / cov / cat inventory | `data_inventory()`, `cov_inventory()`, `cat_inventory()` | first two → list |
| Time / TAD profile | `time_profile(data, meta_data, bins=7, as_plotly=FALSE)`; `tad_profile(...)` | `bins` (opt); → nested list |
| Individual profiles | `ind_time_profiles(data, meta_data, n_rows=2, n_cols=3)`; `ind_tad_profiles(...)` | data needs PK param cols (`CL`,`V`,…) |
| Log axes | `gg_log(plot, x=TRUE, y=TRUE)` (ggplot); `plotly_log(obj, x, y)` (plotly) | `x`,`y` (opt) |
| Apply meta range | `gg_lim(plot, meta_data, x=NULL, y=NULL)` | |
| Parse .res file | `nonmem_res(res_file)` → list | → `format_result()` |
| VPC | `vpc_plots(data, meta_data, x="TAD", bins=7, ci=0.8, lloq=10)` → list; `run_vpc(...)` | `ci` (opt); `run_vpc` needs `REP` |
| Compute TAD | `calc_tad(id, time, amt=NULL, evid=NULL, rate=NULL, dur=NULL)` | infusion: pass `rate`/`dur` (opt) |
| Shiny app | `run_shiny(app_name)` | app folder name |
| Toolbox gadget | `shiny_toolbox()` | needs shiny/miniUI/rstudioapi |
| Reports (.docx) | `report_dataset_analysis()`, `report_gof_analysis()`, `report_vpc_analysis(data_path, meta_data_path, bins=7, ci=0.8)`, `report_workflow_summary(dir_path)`, `report_limited_sampling_analysis(...)` | see API.md |
| Typst helpers | `typst_table(data, linebreak="<br>")`; `typst_scientific(values, digit=2)` | for Quarto→PDF |

## Gotchas

- `cat` mapping lives in the **`Unit`** column for `pull_cat()` but in **`Label`** for the
  template — set it per your `meta_data`; `map_cat_data()` turns `cat` cols into factors.
- `eta_plot()`, `eta_cov_plot()`, `cov_plot()` return **GGally `ggmatrix`** objects (in
  lists for the eta_* pair), not plain `ggplot`; display with `print()`/`patchwork::wrap_plots()`.
- `check_ranges()` does **not** auto-fill `blq`/`mdv` and errors if those `Type`s are
  absent from `meta_data` — declare them (or append a `BLQ`/`MDV` row) first.
- `ind_*_profiles()` & `simulate_pk()` **require PK parameter columns**; model is chosen
  from `KA`/`V2`/`V3` presence.
- `highlight_significant()` parses p-values out of cell text — feed it `eta_cor()`/`cov_cor()` output.
- Use `library(nonmem.utils)` then base ggplot2 helpers; `dv_preds() |> gg_log()` is idiomatic.
