test_that("eta_plot All group snapshot", {
  result <- eta_plot(data_501, meta_data_501)
  expect_type(result, "list")
  expect_named(result, c("All", "SEX"))
  expect_snapshot(result$All)
})

test_that("eta_plot SEX group snapshot", {
  result <- eta_plot(data_501, meta_data_501)
  expect_snapshot(result$SEX)
})

test_that("eta_cov_plot All group snapshot", {
  result <- eta_cov_plot(data_501, meta_data_501)
  expect_type(result, "list")
  expect_named(result, c("All", "SEX"))
  expect_snapshot(result$All)
})

test_that("eta_cov_plot SEX group snapshot", {
  result <- eta_cov_plot(data_501, meta_data_501)
  expect_snapshot(result$SEX)
})

test_that("cov_plot snapshot", {
  result <- cov_plot(data_501, meta_data_501)
  expect_snapshot(result)
})

test_that("eta_plot returns empty ggplot when no etas in meta_data", {
  meta_no_eta <- meta_data_501[meta_data_501$Type != "eta", ]
  result <- eta_plot(data_501, meta_no_eta)
  expect_snapshot(result)
})

test_that("eta_cov_plot returns empty ggplot when no etas in meta_data", {
  meta_no_eta <- meta_data_501[meta_data_501$Type != "eta", ]
  result <- eta_cov_plot(data_501, meta_no_eta)
  expect_snapshot(result)
})

test_that("cov_plot returns empty ggplot when no covariates in meta_data", {
  meta_no_cov <- meta_data_501[!meta_data_501$Type %in% c("cov", "cat"), ]
  result <- cov_plot(data_501, meta_no_cov)
  expect_snapshot(result)
})
