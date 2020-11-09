context("misc functions")

# ------------------------------------------------------------------------------

source(test_path("../helper-objects.R"))
load(test_path("test_objects.RData"))

# ------------------------------------------------------------------------------

test_that('model package lookup', {
  mod_obj <- workflows::pull_workflow_spec(chi_wflow)
  expect_equal(tune::required_pkgs(mod_obj, FALSE), "glmnet")
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

# ------------------------------------------------------------------------------

test_that('accessor functions', {

  expect_equal(.get_tune_parameter_names(mt_spln_knn_bo), attributes(mt_spln_knn_bo)$parameters$id)
  attr(mt_spln_knn_bo, "parameters") <- NULL
  expect_equal(.get_tune_parameter_names(mt_spln_knn_bo), character(0))

  expect_equal(.get_tune_metrics(mt_knn_bo), attributes(mt_knn_bo)$metrics)
  attr(mt_knn_bo, "metrics") <- NULL
  expect_null(.get_tune_metrics(mt_knn_bo))

  expect_equal(.get_tune_metric_names(mt_spln_knn_bo),
               names(attributes(attributes(mt_spln_knn_bo)$metrics)$metrics))
  attr(mt_spln_knn_bo, "metrics") <- NULL
  expect_equal(.get_tune_metric_names(mt_spln_knn_bo), character(0))

  expect_equal(.get_tune_outcome_names(mt_spln_knn_bo), attributes(mt_spln_knn_bo)$outcomes)
  attr(mt_spln_knn_bo, "outcomes") <- NULL
  expect_equal(.get_tune_outcome_names(mt_spln_knn_bo), character(0))

})

test_that('accessor functions', {
  skip_if(utils::packageVersion("dials") <= "0.0.7")
  skip_if(tune:::dplyr_pre_1.0.0())

  expect_equal(
    tibble::as_tibble(.get_tune_parameters(mt_knn_bo)),
    tibble::as_tibble(attributes(mt_knn_bo)$parameters)
  )
  attr(mt_knn_bo, "parameters") <- NULL
  expect_equal(.get_tune_parameters(mt_knn_bo), tibble::tibble())
})

test_that('required package lists', {
  expect_equal(required_pkgs(lm_model, FALSE), "stats")
  expect_equal(required_pkgs(chi_wflow, FALSE), "glmnet")
})
