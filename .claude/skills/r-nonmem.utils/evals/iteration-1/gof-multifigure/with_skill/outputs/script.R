#' 2x2 Goodness-of-fit figure for a NONMEM result table.
#'
#' Panels:
#'   (1) Observed DV vs PRED and IPRED  (log-log)
#'   (2) CWRES vs TIME
#'   (3) CWRES vs PRED
#'   (4) CWRES normal QQ plot
#'
#' Inputs (set these before running):
tab_path  <- "results.tab"     # NONMEM $TABLE output (2-line header)
dict_path <- "dictionary.csv"  # dataset dictionary: Name, Type, Label, Unit, Min, Max

library(nonmem.utils)
library(readr)
library(patchwork)

# --- Read the NONMEM .tab (skip first header line) and the dictionary ----------
data      <- read_table(tab_path, skip = 1)
meta_data <- read_csv(dict_path)

# --- Build the four GOF panels ------------------------------------------------
# (1) DV vs PRED + IPRED on log-log axes (gg_log() is a no-op when both FALSE)
p1_dv <- dv_preds(data = data, meta_data = meta_data) |>
  gg_log(x = TRUE, y = TRUE)

# (2) CWRES vs TIME  (x_type / y_type come before data / meta_data)
p2_res_time <- residual_plot(
  x_type = "time", y_type = "cwres",
  data = data, meta_data = meta_data
)

# (3) CWRES vs PRED
p3_res_pred <- residual_plot(
  x_type = "pred", y_type = "cwres",
  data = data, meta_data = meta_data
)

# (4) CWRES normal QQ plot  (y_type comes before data / meta_data)
p4_qq <- residual_qq(y_type = "cwres", data = data, meta_data = meta_data)

# --- Assemble the 2x2 figure --------------------------------------------------
gof_figure <- (p1_dv | p2_res_time) / (p3_res_pred | p4_qq) +
  plot_annotation(
    title    = "Goodness-of-fit diagnostics",
    tag_levels = "1"
  )

gof_figure
