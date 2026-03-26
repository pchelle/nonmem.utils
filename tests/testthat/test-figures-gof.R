test_that("dv_preds returns a ggplot object", {
  p <- dv_preds(data_501, meta_data_501)
  expect_s3_class(p, "ggplot")
})

test_that("dv_preds uses correct axis labels from meta_data_501", {
  p <- dv_preds(data_501, meta_data_501)
  expect_match(p$labels$x, "Concentration")
  expect_match(p$labels$y, "Concentration")
})

test_that("dv_pred returns a ggplot object", {
  p <- dv_pred(data_501, meta_data_501)
  expect_s3_class(p, "ggplot")
})

test_that("dv_pred uses correct axis labels from meta_data_501", {
  p <- dv_pred(data_501, meta_data_501)
  expect_match(p$labels$x, "Concentration")
  expect_match(p$labels$y, "Population Predicted Concentration")
})

test_that("dv_ipred returns a ggplot object", {
  p <- dv_ipred(data_501, meta_data_501)
  expect_s3_class(p, "ggplot")
})

test_that("dv_ipred uses correct axis labels from meta_data_501", {
  p <- dv_ipred(data_501, meta_data_501)
  expect_match(p$labels$x, "Concentration")
  expect_match(p$labels$y, "Fitted Concentration")
})

test_that("residual_plot returns a ggplot object for time vs cwres", {
  p <- residual_plot(x_type = "time", y_type = "cwres", data = data_501, meta_data = meta_data_501)
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$y, "CWRES")
  expect_match(p$labels$x, "Time")
})

test_that("residual_plot returns a ggplot object for time vs npde", {
  p <- residual_plot(x_type = "time", y_type = "npde", data = data_501, meta_data = meta_data_501)
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$y, "NPDE")
})

test_that("residual_qq returns a ggplot for cwres", {
  p <- residual_qq(y_type = "cwres", data = data_501, meta_data = meta_data_501)
  expect_s3_class(p, "ggplot")
  expect_match(p$labels$x, "Normal distribution quantiles")
  expect_match(p$labels$y, "CWRES")
})

test_that("residual_qq returns a ggplot for npde", {
  p <- residual_qq(y_type = "npde", data = data_501, meta_data = meta_data_501)
  expect_s3_class(p, "ggplot")
  expect_match(p$labels$y, "NPDE")
})

test_that("residual_hist returns a ggplot for cwres", {
  p <- residual_hist(y_type = "cwres", data = data_501, meta_data = meta_data_501)
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$y, "Count")
})

test_that("residual_hist returns a ggplot for npde", {
  p <- residual_hist(y_type = "npde", data = data_501, meta_data = meta_data_501, bins = 11)
  expect_s3_class(p, "ggplot")
})
