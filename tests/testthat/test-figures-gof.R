test_that("dv_preds renders correctly", {
  p <- dv_preds(data_501, meta_data_501)
  suppressWarnings(vdiffr::expect_doppelganger("dv_preds", p))
})

test_that("dv_pred renders correctly", {
  p <- dv_pred(data_501, meta_data_501)
  suppressWarnings(vdiffr::expect_doppelganger("dv_pred", p))
})

test_that("dv_ipred renders correctly", {
  p <- dv_ipred(data_501, meta_data_501)
  suppressWarnings(vdiffr::expect_doppelganger("dv_ipred", p))
})

test_that("residual_plot time vs cwres renders correctly", {
  p <- residual_plot(x_type = "time", y_type = "cwres", data = data_501, meta_data = meta_data_501)
  suppressWarnings(vdiffr::expect_doppelganger("residual_plot-time-cwres", p))
})

test_that("residual_plot time vs npde renders correctly", {
  p <- residual_plot(x_type = "time", y_type = "npde", data = data_501, meta_data = meta_data_501)
  suppressWarnings(vdiffr::expect_doppelganger("residual_plot-time-npde", p))
})

test_that("residual_qq cwres renders correctly", {
  p <- residual_qq(y_type = "cwres", data = data_501, meta_data = meta_data_501)
  vdiffr::expect_doppelganger("residual_qq-cwres", p)
})

test_that("residual_qq npde renders correctly", {
  p <- residual_qq(y_type = "npde", data = data_501, meta_data = meta_data_501)
  vdiffr::expect_doppelganger("residual_qq-npde", p)
})

test_that("residual_hist cwres renders correctly", {
  p <- residual_hist(y_type = "cwres", data = data_501, meta_data = meta_data_501)
  vdiffr::expect_doppelganger("residual_hist-cwres", p)
})

test_that("residual_hist npde renders correctly", {
  p <- residual_hist(y_type = "npde", data = data_501, meta_data = meta_data_501, bins = 11)
  vdiffr::expect_doppelganger("residual_hist-npde", p)
})
