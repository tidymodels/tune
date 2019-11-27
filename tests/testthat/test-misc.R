context("misc functions")

# ------------------------------------------------------------------------------

source(test_path("../helper-objects.R"))

# ------------------------------------------------------------------------------

test_that('model package lookup', {
  mod_obj <- workflows::pull_workflow_spec(chi_wflow)
  expect_equal(tune:::mod_pkgs(mod_obj), "glmnet")
})


# ------------------------------------------------------------------------------

test_that('determine foreach operator', {
  expect_equal(tune:::get_operator(object = chi_wflow), foreach::`%do%`)
  expect_equal(tune:::get_operator(FALSE, chi_wflow), foreach::`%do%`)
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
