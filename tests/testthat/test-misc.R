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

# ------------------------------------------------------------------------------

test_that('in-line formulas on outcome', {
  # see issues 121
  w1 <-
    workflow() %>%
    add_formula(log(mpg) ~ .) %>%
    add_model(linear_reg() %>% set_engine("lm"))

  expect_error(
    f1 <- fit_resamples(w1, resamples = vfold_cv(mtcars)),
    regex = NA
  )
  expect_true(inherits(f1, "resample_results"))

  w2 <-
    workflow() %>%
    add_recipe(recipe(mpg ~ ., data = mtcars) %>% step_log(mpg)) %>%
    add_model(linear_reg() %>% set_engine("lm"))

  expect_error(
    f2 <- fit_resamples(w2, resamples = vfold_cv(mtcars)),
    regex = NA
  )
  expect_true(inherits(f2, "resample_results"))

})

# ------------------------------------------------------------------------------

test_that('empty ellipses', {
  expect_error(tune:::empty_ellipses(), regexp = NA)
  expect_warning(tune:::empty_ellipses(a = 1), regexp = ": 'a'")
})
