## Smoke test: validates the code patterns the r-nonmem.utils SKILL recommends.
suppressPackageStartupMessages({
  library(nonmem.utils)
  library(patchwork)
})

data <- data_501
meta <- meta_data_501
ok <- function(label, cond) {
  cat(sprintf("[%s] %s\n", if (isTRUE(cond)) "PASS" else "FAIL", label))
}

## GOF
ok("dv_preds returns ggplot", ggplot2::is.ggplot(dv_preds(data, meta)))
ok("dv_preds |> gg_log returns ggplot", ggplot2::is.ggplot(dv_preds(data, meta) |> gg_log()))
ok("dv_pred returns ggplot", ggplot2::is.ggplot(dv_pred(data, meta)))
ok("dv_ipred returns ggplot", ggplot2::is.ggplot(dv_ipred(data, meta)))
ok("residual_plot(time,cwres)", ggplot2::is.ggplot(residual_plot("time", "cwres", data, meta)))
ok("residual_plot(pred,cwres)", ggplot2::is.ggplot(residual_plot("pred", "cwres", data, meta)))
ok("residual_qq(cwres)", ggplot2::is.ggplot(residual_qq("cwres", data, meta)))
ok("residual_hist(cwres,bins=11)", ggplot2::is.ggplot(residual_hist("cwres", data, meta, bins = 11)))

## Individual profiles (need PK params)
pk <- data |> dplyr::mutate(CL = 2, V = 40) |> dplyr::filter(ID <= 6)
ok("ind_time_profiles returns list", is.list(ind_time_profiles(pk, meta)))
ok("ind_tad_profiles returns list", is.list(ind_tad_profiles(pk, meta)))

## Covariate
ok("eta_plot returns list", is.list(eta_plot(data, meta)))
ok("eta_cov_plot returns list", is.list(eta_cov_plot(data, meta)))
ok("cov_plot returns ggmatrix", inherits(cov_plot(data, meta), "ggmatrix"))
ec <- eta_cor(data, meta)
ok("eta_cor returns data.frame", is.data.frame(ec))
ok("highlight_significant(html) returns data.frame", is.data.frame(highlight_significant(ec, pval = 0.05, format = "html")))
ok("highlight_significant(typst) returns data.frame", is.data.frame(highlight_significant(ec, pval = 0.05, format = "typst")))
ok("cov_cor returns data.frame", is.data.frame(cov_cor(data, meta)))

## Inventory
di <- data_inventory(data, meta)
ok("data_inventory returns list with All", is.list(di) && !is.null(di$All))
ci <- cov_inventory(data, meta)
ok("cov_inventory returns list with All", is.list(ci) && !is.null(ci$All))
ok("cat_inventory returns data.frame", is.data.frame(cat_inventory(data, meta)))
## check_ranges requires blq/mdv types in meta_data (no auto-fill) — documented gotcha
data_b <- dplyr::mutate(data, BLQ = 0)
meta_b <- dplyr::bind_rows(meta, data.frame(Name = "BLQ", Type = "blq", Label = "BLQ"))
ok("check_ranges returns data.frame with Out of Range",
   is.data.frame(check_ranges(data_b, meta_b)) && "Out of Range" %in% names(check_ranges(data_b, meta_b)))

## Time profile nested list indexing (documented convention)
tp <- time_profile(data, meta)
ok("time_profile has All$Linear", !is.null(tp$All$Linear))
ok("time_profile has All$Log", !is.null(tp$All$Log))
ok("time_profile has All$Percent BLQ", !is.null(tp$All[["Percent BLQ"]]))
tp2 <- tad_profile(data, meta)
ok("tad_profile has All$Linear", !is.null(tp2$All$Linear))

## Meta-data utilities
ok("pull_name(cov) returns chars", is.character(pull_name("cov", meta)))
ok("pull_label(TIME) returns char", is.character(pull_label("TIME", meta)))
ok("pull_limits(TIME) length 2", length(pull_limits("TIME", meta)) == 2)
ok("map_cat_data returns data.frame", is.data.frame(map_cat_data(data, meta)))
ok("fill_meta_vars fills id", "id" %in% fill_meta_vars(meta)$Type)

## gg_lim
ok("gg_lim applies meta range", ggplot2::is.ggplot(gg_lim(dv_preds(data, meta), meta, x = "DV")))

## calc_tad
tad2 <- calc_tad(id = data$ID, time = data$TIME, amt = data$AMT)
ok("calc_tad returns numeric length==nrow", is.numeric(tad2) && length(tad2) == nrow(data))

## VPC
ok("vpc_plots returns list", is.list(vpc_plots(data, meta, x = "TIME")))
ok("run_vpc returns list", is.list(run_vpc(data, x = "TIME")))
ok("vpc_types exported", identical(vpc_types, c("vpc", "pc_vpc", "pvc_vpc", "blq", "npde")))

## bootstrap hist documented meta requirement
boot_data <- data.frame(theta1 = stats::rlnorm(1e3, log(2), 1))
boot_meta <- data.frame(Name = "theta1", Value = 2, Label = "Clearance", Unit = "L/h")
ok("boot_hist(theta) returns list", is.list(boot_hist("theta", boot_data, boot_meta)))

## typst helpers
ok("typst_table returns char vector", is.character(typst_table(data.frame(a = 1))))
ok("typst_scientific returns char", is.character(typst_scientific(1234)))

cat("\nDONE\n")
