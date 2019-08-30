context("misc functions")

# ------------------------------------------------------------------------------

source("../helper-objects.R")

# ------------------------------------------------------------------------------

test_that('model package lookup', {
  expect_equal(tune:::mod_pkgs(chi_wflow), "glmnet")
})


# ------------------------------------------------------------------------------

test_that('determine foreach operator', {
  expect_equal(tune:::get_operator(), foreach::`%do%`)
  expect_equal(tune:::get_operator(FALSE), foreach::`%do%`)
})

# ------------------------------------------------------------------------------

test_that('exponential decay', {
  expect_equal(
    expo_decay(1, start_val = 0, limit_val = 1, slope = 1/5), 0
  )
  expect_equal(
    expo_decay(1000, start_val = 0, limit_val = 1, slope = 1/5), 1
  )
  expect_equal(
    expo_decay(10, start_val = 0, limit_val = 50, slope = 1), (1 - exp(-9)) * 50
  )
})
