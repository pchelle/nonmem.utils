test_that("vpc_summary returns a data.frame with expected columns", {
  obs_data <- data_501 |> dplyr::filter(MDV == 0)
  result <- vpc_summary(data = obs_data, x = "TIME", y = "DV", bins = 7)

  expect_s3_class(result, "data.frame")
  expect_true(all(c("bins", "x", "n", "y", "ymin", "ymax", "blq") %in% names(result)))
})

test_that("vpc_summary bins data into the specified number of bins", {
  obs_data <- data_501 |> dplyr::filter(MDV == 0)
  result <- vpc_summary(data = obs_data, x = "TIME", y = "DV", bins = 7)

  # Number of bins should be <= bins requested
  expect_true(nrow(result) <= 7)
  expect_true(nrow(result) >= 1)
})

test_that("vpc_summary returns sensible median and quantile values", {
  obs_data <- data_501 |> dplyr::filter(MDV == 0)
  result <- vpc_summary(data = obs_data, x = "TIME", y = "DV", bins = 7)

  # Median should be between ymin and ymax for each bin
  valid_rows <- !is.na(result$y) & !is.na(result$ymin) & !is.na(result$ymax)
  expect_true(all(result$y[valid_rows] >= result$ymin[valid_rows]))
  expect_true(all(result$y[valid_rows] <= result$ymax[valid_rows]))
  # x values should be positive (TIME)
  expect_true(all(result$x[!is.na(result$x)] >= 0))
  # n should be positive in each bin
  expect_true(all(result$n[!is.na(result$n)] > 0))
})

test_that("vpc_summary supports grouping by categorical variable", {
  obs_data <- data_501 |>
    dplyr::filter(MDV == 0) |>
    map_cat_data(meta_data_501)

  result <- vpc_summary(data = obs_data, x = "TIME", y = "DV", group = "SEX", bins = 5)

  expect_s3_class(result, "data.frame")
  # Should have SEX column
  expect_true("SEX" %in% names(result))
  # Should have rows for both Male and Female
  sex_groups <- levels(obs_data$SEX)
  expect_true(length(unique(result$SEX)) == length(sex_groups))
})

test_that("bin_values bins numeric vector into specified number of bins", {
  binned <- bin_values(data_501$TIME, bins = 7)
  expect_s3_class(binned, "factor")
  expect_true(nlevels(binned) <= 7)
  expect_length(binned, nrow(data_501))
})

test_that("bin_values handles fewer unique values than bins", {
  small_vec <- c(1, 2, 3)
  binned <- bin_values(small_vec, bins = 7)
  expect_s3_class(binned, "factor")
  expect_equal(nlevels(binned), 3)
})
