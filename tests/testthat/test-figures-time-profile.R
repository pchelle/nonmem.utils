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
  expect_true(all(c("All", "SEX") %in% names(result)))
  expect_named(result$All, c("Linear", "Log", "Percent BLQ"))
})

test_that("time_profile All Linear renders correctly", {
  tp_meta <- dplyr::bind_rows(
    meta_data_501,
    data.frame(Name = c("BLQ", "LLOQ"), Type = c("blq", "lloq"), Label = c("BLQ", "LLOQ"), Unit = NA, Min = NA, Max = NA)
  )
  tp_data <- data_501 |>
    dplyr::mutate(BLQ = 0, LLOQ = 1) |>
    dplyr::filter(MDV == 0)

  result <- time_profile(tp_data, tp_meta)
  vdiffr::expect_doppelganger("time_profile-All-Linear", result$All$Linear)
})

test_that("time_profile All Log renders correctly", {
  tp_meta <- dplyr::bind_rows(
    meta_data_501,
    data.frame(Name = c("BLQ", "LLOQ"), Type = c("blq", "lloq"), Label = c("BLQ", "LLOQ"), Unit = NA, Min = NA, Max = NA)
  )
  tp_data <- data_501 |>
    dplyr::mutate(BLQ = 0, LLOQ = 1) |>
    dplyr::filter(MDV == 0)

  result <- time_profile(tp_data, tp_meta)
  vdiffr::expect_doppelganger("time_profile-All-Log", result$All$Log)
})

test_that("time_profile All Percent BLQ renders correctly", {
  tp_meta <- dplyr::bind_rows(
    meta_data_501,
    data.frame(Name = c("BLQ", "LLOQ"), Type = c("blq", "lloq"), Label = c("BLQ", "LLOQ"), Unit = NA, Min = NA, Max = NA)
  )
  tp_data <- data_501 |>
    dplyr::mutate(BLQ = 0, LLOQ = 1) |>
    dplyr::filter(MDV == 0)

  result <- time_profile(tp_data, tp_meta)
  vdiffr::expect_doppelganger("time_profile-All-PercentBLQ", result$All$`Percent BLQ`)
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
})

test_that("tad_profile All Linear renders correctly", {
  tp_meta <- dplyr::bind_rows(
    meta_data_501,
    data.frame(Name = c("BLQ", "LLOQ"), Type = c("blq", "lloq"), Label = c("BLQ", "LLOQ"), Unit = NA, Min = NA, Max = NA)
  )
  tp_data <- data_501 |> dplyr::mutate(BLQ = 0, LLOQ = 1)

  result <- tad_profile(tp_data, tp_meta)
  vdiffr::expect_doppelganger("tad_profile-All-Linear", result$All$Linear)
})

test_that("tad_profile All Log renders correctly", {
  tp_meta <- dplyr::bind_rows(
    meta_data_501,
    data.frame(Name = c("BLQ", "LLOQ"), Type = c("blq", "lloq"), Label = c("BLQ", "LLOQ"), Unit = NA, Min = NA, Max = NA)
  )
  tp_data <- data_501 |> dplyr::mutate(BLQ = 0, LLOQ = 1)

  result <- tad_profile(tp_data, tp_meta)
  vdiffr::expect_doppelganger("tad_profile-All-Log", result$All$Log)
})
