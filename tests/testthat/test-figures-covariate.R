test_that("eta_plot returns a list of ggmatrix objects", {
  result <- eta_plot(data_501, meta_data_501)
  expect_type(result, "list")
  # Should have All group and SEX group
  expect_named(result, c("All", "SEX"))
  expect_s3_class(result$All, "gg")
})

test_that("eta_cov_plot returns a list of gg objects", {
  result <- eta_cov_plot(data_501, meta_data_501)
  expect_type(result, "list")
  expect_named(result, c("All", "SEX"))
  expect_s3_class(result$All, "gg")
})

test_that("cov_plot returns a ggmatrix object", {
  result <- cov_plot(data_501, meta_data_501)
  expect_s3_class(result, "gg")
})

test_that("eta_plot returns empty ggplot when no etas in meta_data", {
  meta_no_eta <- meta_data_501[meta_data_501$Type != "eta", ]
  result <- eta_plot(data_501, meta_no_eta)
  expect_s3_class(result, "ggplot")
})

test_that("eta_cov_plot returns empty ggplot when no etas in meta_data", {
  meta_no_eta <- meta_data_501[meta_data_501$Type != "eta", ]
  result <- eta_cov_plot(data_501, meta_no_eta)
  expect_s3_class(result, "ggplot")
})

test_that("cov_plot returns empty ggplot when no covariates in meta_data", {
  meta_no_cov <- meta_data_501[!meta_data_501$Type %in% c("cov", "cat"), ]
  result <- cov_plot(data_501, meta_no_cov)
  expect_s3_class(result, "ggplot")
})
