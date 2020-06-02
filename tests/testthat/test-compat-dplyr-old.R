# These tests should pass on all supported versions of dplyr. Both pre and
# post dplyr 1.0.0 should work.
# When `compat-dplyr-old.R` is removed and support for dplyr < 1.0.0 is
# deprecated, these tests should move to `test-compat-dplyr.R` instead.
# Do not just delete them, as they are important tests and are not repeated in
# `compat-dplyr.R`.

library(dplyr)

# ------------------------------------------------------------------------------
# mutate()

test_that("mutate() can keep tune_results class", {
  for (x in helper_tune_results) {
    expect_s3_class_tune_results(mutate(x, x = 1))
    expect_identical(mutate(x, x = 1)$x, rep(1, vec_size(x)))
  }
})

test_that("mutate() drops tune_results class if any tune_results columns are touched", {
  for (x in helper_tune_results) {
    expect_s3_class_bare_tibble(mutate(x, splits = 1))
    expect_s3_class_bare_tibble(mutate(x, id = 1))

    expect_identical(mutate(x, splits = 1)$splits, rep(1, vec_size(x)))
    expect_identical(mutate(x, id = 1)$id, rep(1, vec_size(x)))
  }
})

test_that("mutate() keeps tune_results class if replacement tune_results column is same as original", {
  for (x in helper_tune_results) {
    expect_s3_class_tune_results(mutate(x, splits = splits))
    expect_s3_class_tune_results(mutate(x, id = id))
  }
})

test_that("adding a column that looks like an `id` drops the class", {
  for (x in helper_tune_results) {
    expect_s3_class_bare_tibble(mutate(x, id9 = 1))
  }
})

# ------------------------------------------------------------------------------
# arrange()

test_that("arrange() keeps tune_results class when row order is modified", {
  for (x in helper_tune_results) {
    x <- mutate(x, rn = row_number())
    expect_s3_class_tune_results(arrange(x, desc(rn)))
  }
})

test_that("arrange() keeps tune_results class when row order is untouched", {
  for (x in helper_tune_results) {
    expect_s3_class_tune_results(arrange(x))

    x <- mutate(x, rn = row_number())
    expect_s3_class_tune_results(arrange(x, rn))
  }
})

# ------------------------------------------------------------------------------
# filter()

test_that("filter() drops tune_results class when rows are modified", {
  for (x in helper_tune_results) {
    expect_s3_class_bare_tibble(filter(x, 0 == 1))
    expect_s3_class_bare_tibble(filter(x, is.numeric(id)))
  }
})

test_that("filter() keeps tune_results class if row order is untouched", {
  for (x in helper_tune_results) {
    expect_s3_class_tune_results(filter(x))
    expect_s3_class_tune_results(filter(x, is.character(id)))
  }
})

# ------------------------------------------------------------------------------
# rename()

test_that("renaming can keep the tune_results class", {
  for (x in helper_tune_results) {
    x <- mutate(x, a = 1)
    x <- rename(x, b = a)
    expect_s3_class_tune_results(x)
  }
})

test_that("renaming `id` at all drops the tune_results class", {
  for (x in helper_tune_results) {
    x <- rename(x, id9 = id)
    expect_s3_class_bare_tibble(x)
  }
})

test_that("renaming `id` to a non-id name drops the tune_results class", {
  for (x in helper_tune_results) {
    x <- rename(x, stuff = id)
    expect_s3_class_bare_tibble(x)
  }
})

# ------------------------------------------------------------------------------
# select()

test_that("select() can keep tune_results class", {
  for (x in helper_tune_results) {
    expect_s3_class_tune_results(select(x, everything()))
  }
})

test_that("select() drops tune_results class if any tune_results columns aren't selected", {
  for (x in helper_tune_results) {
    expect_s3_class_bare_tibble(select(x, id))
    expect_s3_class_bare_tibble(select(x, splits))
  }
})

# ------------------------------------------------------------------------------
# slice()

test_that("slice() drops tune_results class when rows are modified", {
  for (x in helper_tune_results) {
    expect_s3_class_bare_tibble(slice(x, 0))
  }
})

test_that("slice() keeps tune_results class when rows are untouched", {
  for (x in helper_tune_results) {
    expect_s3_class_tune_results(slice(x))
    expect_s3_class_tune_results(slice(x, seq_len(nrow(x))))
  }
})
