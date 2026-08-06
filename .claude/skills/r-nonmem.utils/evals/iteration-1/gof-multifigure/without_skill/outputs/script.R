#!/usr/bin/env Rscript
#
# 2x2 goodness-of-fit figure for a NONMEM result table
# using the {nonmem.utils} package and {patchwork}.
#
# Panels:
#   (1) DV vs PRED + IPRED   (log-log)
#   (2) CWRES vs TIME
#   (3) CWRES vs PRED
#   (4) CWRES normal QQ
#
# ------------------------------------------------------------

## ---- Inputs (edit as needed) ------------------------------------------
tab_path  <- "path/to/run.tab"        # NONMEM $TABLE output (.tab)
dict_path <- "path/to/dictionary.csv" # dataset dictionary / meta_data CSV

## ---- Setup ------------------------------------------------------------
library(nonmem.utils)
library(patchwork)
library(ggplot2)

## ---- Read the NONMEM table --------------------------------------------
# nonmem_res() reads a NONMEM table file (.tab) and returns a tidy
# data.frame with standardized column names (ID, TIME, DV, PRED, IPRED,
# CWRES, ...).
nm_res <- nonmem_res(tab_path)

## ---- Load the dataset dictionary --------------------------------------
# The dictionary describes each column (name, label, units, limits,
# transformation, ...). The plotting helpers use it to annotate axes.
# Fall back to a plain CSV read if no dedicated loader is available.
meta_data <- tryCatch(
  read_meta_data(dict_path),
  error = function(e) read.csv(dict_path, stringsAsFactors = FALSE)
)

## ---- (1) DV vs PRED and IPRED, log-log --------------------------------
p_dv <- dv_preds(
  data      = nm_res,
  meta_data = meta_data
) +
  gg_log()                           # log-log both axes

## ---- (2) CWRES vs TIME ------------------------------------------------
p_res_time <- residual_plot(
  data      = nm_res,
  meta_data = meta_data,
  x         = "TIME",
  y         = "CWRES"
)

## ---- (3) CWRES vs PRED ------------------------------------------------
p_res_pred <- residual_plot(
  data      = nm_res,
  meta_data = meta_data,
  x         = "PRED",
  y         = "CWRES"
)

## ---- (4) CWRES normal QQ plot -----------------------------------------
p_qq <- residual_qq(
  data      = nm_res,
  meta_data = meta_data,
  y         = "CWRES"
)

## ---- Assemble the 2x2 layout ------------------------------------------
gof_figure <- (p_dv + p_res_time) /
              (p_res_pred + p_qq) +
  plot_annotation(
    title    = "Goodness-of-fit diagnostics",
    tag_levels = "1"
  ) &
  theme(legend.position = "bottom")

## ---- Render -----------------------------------------------------------
print(gof_figure)

# Optional: save to file
# ggsave("gof_2x2.png", gof_figure,
#        width = 10, height = 10, dpi = 300)
