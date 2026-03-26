test_that("eta_plot All group renders correctly", {
  result <- eta_plot(data_501, meta_data_501)
  expect_type(result, "list")
  expect_named(result, c("All", "SEX"))
  vdiffr::expect_doppelganger("eta_plot-All", result$All)
})

test_that("eta_plot SEX group renders correctly", {
  result <- eta_plot(data_501, meta_data_501)
  vdiffr::expect_doppelganger("eta_plot-SEX", result$SEX)
})

test_that("eta_cov_plot All group renders correctly", {
  result <- eta_cov_plot(data_501, meta_data_501)
  expect_type(result, "list")
  expect_named(result, c("All", "SEX"))
  vdiffr::expect_doppelganger("eta_cov_plot-All", result$All)
})

test_that("eta_cov_plot SEX group renders correctly", {
  result <- eta_cov_plot(data_501, meta_data_501)
  vdiffr::expect_doppelganger("eta_cov_plot-SEX", result$SEX)
})

test_that("cov_plot renders correctly", {
  result <- cov_plot(data_501, meta_data_501)
  vdiffr::expect_doppelganger("cov_plot", result)
})

test_that("eta_plot returns empty ggplot when no etas in meta_data", {
  meta_no_eta <- meta_data_501[meta_data_501$Type != "eta", ]
  result <- eta_plot(data_501, meta_no_eta)
  vdiffr::expect_doppelganger("eta_plot-no-eta", result)
})

test_that("eta_cov_plot returns empty ggplot when no etas in meta_data", {
  meta_no_eta <- meta_data_501[meta_data_501$Type != "eta", ]
  result <- eta_cov_plot(data_501, meta_no_eta)
  vdiffr::expect_doppelganger("eta_cov_plot-no-eta", result)
})

test_that("cov_plot returns empty ggplot when no covariates in meta_data", {
  meta_no_cov <- meta_data_501[!meta_data_501$Type %in% c("cov", "cat"), ]
  result <- cov_plot(data_501, meta_no_cov)
  vdiffr::expect_doppelganger("cov_plot-no-cov", result)
})
