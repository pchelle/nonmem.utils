test_that("dv_preds snapshot", {
  p <- dv_preds(data_501, meta_data_501)
  expect_snapshot(p)
})

test_that("dv_pred snapshot", {
  p <- dv_pred(data_501, meta_data_501)
  expect_snapshot(p)
})

test_that("dv_ipred snapshot", {
  p <- dv_ipred(data_501, meta_data_501)
  expect_snapshot(p)
})

test_that("residual_plot time vs cwres snapshot", {
  p <- residual_plot(x_type = "time", y_type = "cwres", data = data_501, meta_data = meta_data_501)
  expect_snapshot(p)
})

test_that("residual_plot time vs npde snapshot", {
  p <- residual_plot(x_type = "time", y_type = "npde", data = data_501, meta_data = meta_data_501)
  expect_snapshot(p)
})

test_that("residual_qq cwres snapshot", {
  p <- residual_qq(y_type = "cwres", data = data_501, meta_data = meta_data_501)
  expect_snapshot(p)
})

test_that("residual_qq npde snapshot", {
  p <- residual_qq(y_type = "npde", data = data_501, meta_data = meta_data_501)
  expect_snapshot(p)
})

test_that("residual_hist cwres snapshot", {
  p <- residual_hist(y_type = "cwres", data = data_501, meta_data = meta_data_501)
  expect_snapshot(p)
})

test_that("residual_hist npde snapshot", {
  p <- residual_hist(y_type = "npde", data = data_501, meta_data = meta_data_501, bins = 11)
  expect_snapshot(p)
})
