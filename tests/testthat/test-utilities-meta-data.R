test_that("pull_name returns correct variable names by type", {
  expect_equal(pull_name("id", meta_data_501), "ID")
  expect_equal(pull_name("time", meta_data_501), "TIME")
  expect_equal(pull_name("tad", meta_data_501), "TAD")
  expect_equal(pull_name("dv", meta_data_501), "DV")
  expect_equal(pull_name("evid", meta_data_501), "EVID")
  expect_equal(pull_name("mdv", meta_data_501), "MDV")
  expect_equal(pull_name("cat", meta_data_501), "SEX")
  expect_equal(sort(pull_name("cov", meta_data_501)), sort(c("WT", "AGE")))
  expect_equal(sort(pull_name("eta", meta_data_501)), sort(c("ETA1", "ETA2")))
  # Non-existent type returns empty character
  expect_equal(pull_name("nonexistent", meta_data_501), character(0))
})

test_that("pull_label returns correct labels with units", {
  expect_equal(pull_label("TIME", meta_data_501), "Time [h]")
  expect_equal(pull_label("DV", meta_data_501), "Concentration [mg/L]")
  expect_equal(pull_label("WT", meta_data_501), "Weight [kg]")
  expect_equal(pull_label("ID", meta_data_501), "Subject")
  # Categorical variables have no unit in label
  expect_equal(pull_label("SEX", meta_data_501), "Sex")
  # Multiple labels returned in the correct order
  labels <- pull_label(c("WT", "AGE", "SEX"), meta_data_501)
  expect_length(labels, 3)
  expect_equal(labels[1], "Weight [kg]")
  expect_equal(labels[2], "Age [yrs]")
  expect_equal(labels[3], "Sex")
})

test_that("pull_cat returns correct categorical mappings", {
  cat_data <- pull_cat("SEX", meta_data_501)
  expect_s3_class(cat_data, "data.frame")
  expect_named(cat_data, c("value", "label"))
  expect_equal(nrow(cat_data), 2)
  expect_true(all(c("Male", "Female") %in% cat_data$label))
})

test_that("pull_limits returns correct limits", {
  limits <- pull_limits("TIME", meta_data_501)
  expect_length(limits, 2)
  # TIME has no min/max set, so should return -Inf/Inf
  expect_equal(limits[1], -Inf)
  expect_equal(limits[2], Inf)
})

test_that("map_cat_data converts categorical columns to factors", {
  mapped_data <- map_cat_data(data_501, meta_data_501)
  expect_s3_class(mapped_data$SEX, "factor")
  expect_equal(levels(mapped_data$SEX), c("Male", "Female"))
  # Non-categorical columns remain unchanged
  expect_false(is.factor(mapped_data$WT))
  expect_false(is.factor(mapped_data$TIME))
})

test_that("fill_meta_vars adds required variables when missing", {
  minimal_meta <- data.frame(
    Name = c("TIME", "DV"),
    Type = c("time", "dv"),
    Label = c("Time", "Concentration"),
    Unit = c("h", "mg/L"),
    Min = c(NA, NA),
    Max = c(NA, NA)
  )
  filled <- fill_meta_vars(minimal_meta)
  expect_s3_class(filled, "data.frame")
  # Should have added id, occ, tad, evid, mdv, blq, lloq
  expect_true("id" %in% filled$Type)
  expect_true("evid" %in% filled$Type)
  expect_true("mdv" %in% filled$Type)
  expect_true("blq" %in% filled$Type)
  expect_true("lloq" %in% filled$Type)
})

test_that("fill_meta_vars returns NULL when meta_data is NULL", {
  expect_null(fill_meta_vars(NULL))
})

test_that("fill_nonmem_vars fills missing NONMEM columns with sensible defaults", {
  minimal_data <- data.frame(
    ID = 1:3,
    TIME = c(0, 1, 2),
    DV = c(0, 5, 3),
    AMT = c(100, 0, 0),
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0)
  )
  filled <- fill_nonmem_vars(minimal_data)
  expect_true("BLQ" %in% names(filled))
  expect_true("LLOQ" %in% names(filled))
  expect_true("PRED" %in% names(filled))
  expect_true("IPRED" %in% names(filled))
  expect_true("CWRES" %in% names(filled))
  expect_true("NPDE" %in% names(filled))
  expect_true("REP" %in% names(filled))
})
