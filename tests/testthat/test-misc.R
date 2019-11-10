context("misc functions")

# ------------------------------------------------------------------------------

source("../helper-objects.R")

# ------------------------------------------------------------------------------

test_that('model package lookup', {
  mod_obj <- tune:::get_wflow_model(chi_wflow)
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


# ------------------------------------------------------------------------------

test_that('correct size of formula method', {
  splits <- rsample::vfold_cv(mtcars)
  form <- list(pre = list(formula_processor = list(formula_processor = mpg ~ .)))
  res <- tune:::exec_formula(splits$splits[[1]], form)
  expect_equivalent(nrow(res$x), dim(splits$splits[[1]])[1])
  expect_equivalent(nrow(res$y), dim(splits$splits[[1]])[1])
})
