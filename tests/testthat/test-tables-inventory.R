test_that("data_inventory returns a list with All and categorical groups", {
  inventories <- data_inventory(data_501, meta_data_501)
  expect_type(inventories, "list")
  expect_named(inventories, c("Sex", "All"))
  # All group is a data.frame
  expect_s3_class(inventories$All, "data.frame")
  expect_named(inventories$All, c("Data", "All"))
})

test_that("data_inventory All group contains sensible counts", {
  inventories <- data_inventory(data_501, meta_data_501)
  all_inv <- inventories$All
  # Number of subjects should be positive
  n_subjects <- all_inv$All[all_inv$Data == "Subjects"]
  expect_true(n_subjects > 0)
  # Number of observations should be positive
  n_obs <- all_inv$All[all_inv$Data == "Observations"]
  expect_true(n_obs > 0)
  # Doses should be positive
  n_doses <- all_inv$All[all_inv$Data == "Doses"]
  expect_true(n_doses > 0)
  # Data_501 has known structure: 60 subjects with EVID=1 records
  expect_equal(n_subjects, 60)
})

test_that("data_inventory Sex group splits data correctly", {
  inventories <- data_inventory(data_501, meta_data_501)
  sex_inv <- inventories$Sex
  expect_s3_class(sex_inv, "data.frame")
  # Should have columns for the Sex label and Male/Female categories
  expect_true(ncol(sex_inv) == 3)
  # Total subjects from both sexes should equal total
  all_subjects <- inventories$All$All[inventories$All$Data == "Subjects"]
  male_subjects <- sex_inv[sex_inv[, 1] == "Subjects", 2]
  female_subjects <- sex_inv[sex_inv[, 1] == "Subjects", 3]
  expect_equal(as.numeric(male_subjects) + as.numeric(female_subjects), all_subjects)
})

test_that("cov_inventory returns a list with All and categorical subgroups", {
  inventories <- cov_inventory(data_501, meta_data_501)
  expect_type(inventories, "list")
  expect_true("All" %in% names(inventories))
  # Should have Male and Female subgroups
  expect_true(all(c("SEX : Male", "SEX : Female") %in% names(inventories)))
})

test_that("cov_inventory All group summarises covariates correctly", {
  inventories <- cov_inventory(data_501, meta_data_501)
  all_inv <- inventories$All
  expect_s3_class(all_inv, "data.frame")
  # Should have Statistics column and covariate columns
  expect_true("Statistics" %in% names(all_inv))
  expect_true(all(c("Weight [kg]", "Age [yrs]") %in% names(all_inv)))
  # Standard statistics rows
  expect_true(all(c("N", "Mean", "Median", "SD", "Min", "Max") %in% all_inv$Statistics))
})

test_that("cov_inventory subgroups have same structure as All group", {
  inventories <- cov_inventory(data_501, meta_data_501)
  all_inv <- inventories$All
  male_inv <- inventories[["SEX : Male"]]
  expect_equal(names(all_inv), names(male_inv))
  expect_equal(all_inv$Statistics, male_inv$Statistics)
})

test_that("cat_inventory returns a data.frame with categorical summaries", {
  cat_inv <- cat_inventory(data_501, meta_data_501)
  expect_s3_class(cat_inv, "data.frame")
  # Should contain Sex, Count, and Percent columns
  expect_true(all(c("Sex", "Count", "Percent") %in% names(cat_inv)))
  # Percent should sum to 100 (for non-NA rows)
  sex_pct <- cat_inv$Percent[!is.na(cat_inv$Percent)]
  expect_equal(sum(sex_pct), 100)
})
