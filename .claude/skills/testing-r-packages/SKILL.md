---
name: testing-r-packages
description: >
  Best practices for writing R package tests using testthat version 3+. Use when writing, organizing, or improving tests for R packages. Covers test structure, expectations, fixtures, snapshots, mocking, and modern testthat 3 patterns including self-sufficient tests, proper cleanup with withr, and snapshot testing.
metadata:
  author: Garrick Aden-Buie (@gadenbuie)
  version: "1.0"
  source: https://github.com/posit-dev/skills/blob/main/r-lib/testing-r-packages/SKILL.md
license: MIT
---

# Testing R Packages with testthat

Modern best practices for R package testing using testthat 3+.

## Initial Setup

Initialize testing with testthat 3rd edition:

```r
usethis::use_testthat(3)
```

This creates `tests/testthat/` directory, adds testthat to `DESCRIPTION` Suggests with `Config/testthat/edition: 3`, and creates `tests/testthat.R`.

## File Organization

**Mirror package structure:**
- Code in `R/foofy.R` → tests in `tests/testthat/test-foofy.R`
- Use `usethis::use_r("foofy")` and `usethis::use_test("foofy")` to create paired files

**Special files:**
- `helper-*.R` - Helper functions and custom expectations, sourced before tests
- `setup-*.R` - Run during `R CMD check` only, not during `load_all()`
- `fixtures/` - Static test data files accessed via `test_path()`

## Test Structure

Tests follow a three-level hierarchy: **File → Test → Expectation**

### Standard Syntax

```r
test_that("descriptive behavior", {
  result <- my_function(input)
  expect_equal(result, expected_value)
})
```

**Test descriptions** should read naturally and describe behavior, not implementation.

### BDD Syntax (describe/it)

For behavior-driven development, use `describe()` and `it()`:

```r
describe("matrix()", {
  it("can be multiplied by a scalar", {
    m1 <- matrix(1:4, 2, 2)
    m2 <- m1 * 2
    expect_equal(matrix(1:4 * 2, 2, 2), m2)
  })

  it("can be transposed", {
    m <- matrix(1:4, 2, 2)
    expect_equal(t(m), matrix(c(1, 3, 2, 4), 2, 2))
  })
})
```

**Use `describe()` to verify you implement the right things, use `test_that()` to ensure you do things right.**

## Running Tests

Three scales of testing:

**Micro** (interactive development):
```r
devtools::load_all()
expect_equal(foofy(...), expected)
```

**Mezzo** (single file):
```r
testthat::test_file("tests/testthat/test-foofy.R")
```

**Macro** (full suite):
```r
devtools::test()
devtools::check()
```

## Core Expectations

### Equality

```r
expect_equal(10, 10 + 1e-7)      # Allows numeric tolerance
expect_identical(10L, 10L)       # Exact match required
```

### Errors, Warnings, Messages

```r
expect_error(1 / "a")
expect_error(bad_call(), class = "specific_error_class")
expect_no_error(valid_call())

expect_warning(deprecated_func())
expect_no_warning(safe_func())

expect_message(informative_func())
expect_no_message(quiet_func())
```

### Pattern Matching

```r
expect_match("Testing is fun!", "Testing")
expect_match(text, "pattern", ignore.case = TRUE)
```

### Structure and Type

```r
expect_length(vector, 10)
expect_type(obj, "list")
expect_s3_class(model, "lm")
```

### Sets and Collections

```r
expect_setequal(x, y)           # Same elements, any order
expect_contains(fruits, "apple") # Subset check (v3.2.0+)
expect_in("apple", fruits)       # Element in set (v3.2.0+)
```

## Design Principles

### 1. Self-Sufficient Tests

Each test should contain all setup, execution, and teardown code.

### 2. Self-Contained Tests (Cleanup Side Effects)

Use `withr` to manage state changes:

```r
test_that("function respects options", {
  withr::local_options(my_option = "test_value")
  withr::local_envvar(MY_VAR = "test")

  result <- my_function()
  expect_equal(result$setting, "test_value")
  # Automatic cleanup after test
})
```

**Common withr functions:**
- `local_options()` - Temporarily set options
- `local_envvar()` - Temporarily set environment variables
- `local_tempfile()` - Create temp file with automatic cleanup
- `local_tempdir()` - Create temp directory with automatic cleanup

### 3. Plan for Test Failure

Write tests assuming they will fail and need debugging:
- Tests should run independently in fresh R sessions
- Avoid hidden dependencies on earlier tests

### 4. Repetition is Acceptable

Repeat setup code in tests rather than factoring it out. Test clarity is more important than avoiding duplication.

## Snapshot Testing

For complex output that's difficult to verify programmatically:

```r
test_that("error message is helpful", {
  expect_snapshot(
    error = TRUE,
    validate_input(NULL)
  )
})
```

Snapshots stored in `tests/testthat/_snaps/`.

**Workflow:**
```r
devtools::test()                    # Creates new snapshots
testthat::snapshot_review('name')   # Review changes
testthat::snapshot_accept('name')   # Accept changes
```

## Test Fixtures and Data

Three approaches for test data:

**1. Constructor functions** - Create data on-demand:
```r
new_sample_data <- function(n = 10) {
  data.frame(id = seq_len(n), value = rnorm(n))
}
```

**2. Local functions with cleanup** - Handle side effects:
```r
local_temp_csv <- function(data, env = parent.frame()) {
  path <- withr::local_tempfile(fileext = ".csv", .local_envir = env)
  write.csv(data, path, row.names = FALSE)
  path
}
```

**3. Static fixture files** - Store in `fixtures/` directory:
```r
data <- readRDS(test_path("fixtures", "sample_data.rds"))
```

## Mocking

Replace external dependencies during testing using `local_mocked_bindings()`:

```r
test_that("function works with mocked dependency", {
  local_mocked_bindings(
    external_api = function(...) list(status = "success", data = "mocked")
  )

  result <- my_function_that_calls_api()
  expect_equal(result$status, "success")
})
```

## Common Patterns

### Testing with Temporary Files

```r
test_that("file processing works", {
  temp_file <- withr::local_tempfile(
    lines = c("line1", "line2", "line3")
  )

  result <- process_file(temp_file)
  expect_equal(length(result), 3)
})
```

### Custom Expectations in Helper Files

```r
# In tests/testthat/helper-expectations.R
expect_valid_result <- function(result) {
  expect_type(result, "list")
  expect_named(result, c("id", "value"))
}

# In test file
test_that("result is valid", {
  result <- compute_result()
  expect_valid_result(result)
})
```

## File System Discipline

**Always write to temp directory:**

```r
# Good
output <- withr::local_tempfile(fileext = ".csv")
write.csv(data, output)

# Bad - writes to package directory
write.csv(data, "output.csv")
```

**Access test fixtures with `test_path()`:**

```r
# Good - works in all contexts
data <- readRDS(test_path("fixtures", "data.rds"))
```

## testthat 3 Modernizations

**Deprecated → Modern:**
- `context()` → Remove (duplicates filename)
- `expect_equivalent()` → `expect_equal(ignore_attr = TRUE)`
- `with_mock()` → `local_mocked_bindings()`

## Quick Reference

**Initialize:** `usethis::use_testthat(3)`

**Run tests:** `devtools::test()`

**Create test file:** `usethis::use_test("name")`

**Review snapshots:** `testthat::snapshot_review()`

**Accept snapshots:** `testthat::snapshot_accept()`
