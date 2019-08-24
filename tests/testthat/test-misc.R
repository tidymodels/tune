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
