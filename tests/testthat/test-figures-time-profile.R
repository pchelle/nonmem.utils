test_that("time_profile returns a named list of plot lists", {
  tp_meta <- dplyr::bind_rows(
    meta_data_501,
    data.frame(Name = c("BLQ", "LLOQ"), Type = c("blq", "lloq"), Label = c("BLQ", "LLOQ"), Unit = NA, Min = NA, Max = NA)
  )
  tp_data <- data_501 |>
    dplyr::mutate(BLQ = 0, LLOQ = 1) |>
    dplyr::filter(MDV == 0)

  result <- time_profile(tp_data, tp_meta)
  expect_type(result, "list")
  # Should have All and SEX groups
  expect_true(all(c("All", "SEX") %in% names(result)))
  # Each group has Linear, Log and Percent BLQ plots
  expect_named(result$All, c("Linear", "Log", "Percent BLQ"))
  expect_s3_class(result$All$Linear, "ggplot")
  expect_s3_class(result$All$Log, "ggplot")
  expect_s3_class(result$All$`Percent BLQ`, "ggplot")
})

test_that("time_profile linear plot has correct axis labels", {
  tp_meta <- dplyr::bind_rows(
    meta_data_501,
    data.frame(Name = c("BLQ", "LLOQ"), Type = c("blq", "lloq"), Label = c("BLQ", "LLOQ"), Unit = NA, Min = NA, Max = NA)
  )
  tp_data <- data_501 |>
    dplyr::mutate(BLQ = 0, LLOQ = 1) |>
    dplyr::filter(MDV == 0)

  result <- time_profile(tp_data, tp_meta)
  p <- result$All$Linear
  expect_match(p$labels$x, "Time")
  expect_match(p$labels$y, "Concentration")
})

test_that("tad_profile returns a named list of plot lists", {
  tp_meta <- dplyr::bind_rows(
    meta_data_501,
    data.frame(Name = c("BLQ", "LLOQ"), Type = c("blq", "lloq"), Label = c("BLQ", "LLOQ"), Unit = NA, Min = NA, Max = NA)
  )
  tp_data <- data_501 |> dplyr::mutate(BLQ = 0, LLOQ = 1)

  result <- tad_profile(tp_data, tp_meta)
  expect_type(result, "list")
  expect_true(all(c("All", "SEX") %in% names(result)))
  expect_named(result$All, c("Linear", "Log", "Percent BLQ"))
  expect_s3_class(result$All$Linear, "ggplot")
  expect_s3_class(result$All$Log, "ggplot")
})

test_that("tad_profile linear plot has correct axis labels", {
  tp_meta <- dplyr::bind_rows(
    meta_data_501,
    data.frame(Name = c("BLQ", "LLOQ"), Type = c("blq", "lloq"), Label = c("BLQ", "LLOQ"), Unit = NA, Min = NA, Max = NA)
  )
  tp_data <- data_501 |> dplyr::mutate(BLQ = 0, LLOQ = 1)

  result <- tad_profile(tp_data, tp_meta)
  p <- result$All$Linear
  expect_match(p$labels$x, "Time After Dose")
  expect_match(p$labels$y, "Concentration")
})
