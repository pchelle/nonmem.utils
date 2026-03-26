test_that("cov_cor returns a data.frame with covariate correlations", {
  sum_data <- data_501 |>
    dplyr::group_by(ID) |>
    dplyr::summarise_all(dplyr::first)

  result <- cov_cor(sum_data, meta_data_501)
  expect_s3_class(result, "data.frame")
  # Should have Covariates column and one column per continuous covariate
  expect_true("Covariates" %in% names(result))
  # Continuous covariate labels become columns (Labels without units)
  expect_true(all(c("Weight", "Age") %in% names(result)))
  # Row labels include both continuous and categorical covariates
  expect_true(all(c("Weight", "Age", "Sex") %in% result$Covariates))
})

test_that("cov_cor correlation between Age and Weight is formatted", {
  sum_data <- data_501 |>
    dplyr::group_by(ID) |>
    dplyr::summarise_all(dplyr::first)

  result <- cov_cor(sum_data, meta_data_501)
  # The Age row (index 2) should have a formatted correlation in the Weight column
  cor_val <- result[result$Covariates == "Age", "Weight"]
  expect_type(cor_val, "character")
  expect_match(cor_val, "\\(p")
})

test_that("eta_cor returns a data.frame with eta vs covariate correlations", {
  sum_data <- data_501 |>
    dplyr::group_by(ID) |>
    dplyr::summarise_all(dplyr::first)

  result <- eta_cor(sum_data, meta_data_501)
  expect_s3_class(result, "data.frame")
  # Should have Covariates column and one column per eta
  expect_true("Covariates" %in% names(result))
  expect_true(all(c("Clearance", "Volume") %in% names(result)))
  # Covariates rows include continuous and categorical
  expect_true(all(c("Weight", "Age", "Sex") %in% result$Covariates))
})

test_that("cor_report returns a formatted string", {
  sum_data <- data_501 |>
    dplyr::group_by(ID) |>
    dplyr::summarise_all(dplyr::first)

  result <- cor_report(sum_data, x = "WT", y = "AGE")
  expect_type(result, "character")
  expect_length(result, 1)
  expect_match(result, "\\(p")
})

test_that("lm_report returns a formatted string for categorical x", {
  sum_data <- data_501 |>
    dplyr::group_by(ID) |>
    dplyr::summarise_all(dplyr::first)
  sum_data <- map_cat_data(sum_data, meta_data_501)

  result <- lm_report(sum_data, x = "SEX", y = "WT")
  expect_type(result, "character")
  expect_length(result, 1)
  expect_match(result, "\\(p")
})

test_that("check_ranges returns meta_data with Data Min, Data Max, and Out of Range columns", {
  result <- check_ranges(data_501, meta_data_501)
  expect_s3_class(result, "data.frame")
  expect_true(all(c("Data Min", "Data Max", "Out of Range") %in% names(result)))
  # Should have same number of rows as meta_data_501
  expect_equal(nrow(result), nrow(meta_data_501))
})

test_that("check_ranges Out of Range count is zero for valid data", {
  result <- check_ranges(data_501, meta_data_501)
  # meta_data_501 has no min/max constraints, so out of range should be 0
  expect_true(all(unlist(result[["Out of Range"]]) == 0))
})
