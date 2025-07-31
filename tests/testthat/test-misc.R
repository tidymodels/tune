test_that("exponential decay", {
  expect_equal(
    expo_decay(1, start_val = 0, limit_val = 1, slope = 1 / 5),
    0
  )
  expect_equal(
    expo_decay(1000, start_val = 0, limit_val = 1, slope = 1 / 5),
    1
  )
  expect_equal(
    expo_decay(10, start_val = 0, limit_val = 50, slope = 1),
    (1 - exp(-9)) * 50
  )
})

# ------------------------------------------------------------------------------

test_that("in-line formulas on outcome", {
  # see issues 121
  w1 <-
    workflow() %>%
    add_formula(log(mpg) ~ .) %>%
    add_model(parsnip::linear_reg() %>% parsnip::set_engine("lm"))

  expect_no_error(
    f1 <- fit_resamples(w1, resamples = rsample::vfold_cv(mtcars))
  )
  expect_true(inherits(f1, "resample_results"))

  w2 <-
    workflow() %>%
    add_recipe(
      recipes::recipe(mpg ~ ., data = mtcars) %>% recipes::step_log(mpg)
    ) %>%
    add_model(parsnip::linear_reg() %>% parsnip::set_engine("lm"))

  expect_no_error(
    f2 <- fit_resamples(w2, resamples = rsample::vfold_cv(mtcars))
  )
  expect_true(inherits(f2, "resample_results"))
})

# ------------------------------------------------------------------------------

test_that("empty ellipses", {
  expect_no_error(tune:::empty_ellipses())
  expect_snapshot(tune:::empty_ellipses(a = 1))
})

# ------------------------------------------------------------------------------

test_that("accessor functions", {
  load(test_path("data", "test_objects.RData"))

  expect_equal(
    .get_tune_parameter_names(mt_spln_knn_bo),
    attributes(mt_spln_knn_bo)$parameters$id
  )
  attr(mt_spln_knn_bo, "parameters") <- NULL
  expect_equal(.get_tune_parameter_names(mt_spln_knn_bo), character(0))

  expect_equal(.get_tune_metrics(mt_knn_bo), attributes(mt_knn_bo)$metrics)
  attr(mt_knn_bo, "metrics") <- NULL
  expect_null(.get_tune_metrics(mt_knn_bo))

  expect_equal(
    .get_tune_metric_names(mt_spln_knn_bo),
    names(attributes(attributes(mt_spln_knn_bo)$metrics)$metrics)
  )
  attr(mt_spln_knn_bo, "metrics") <- NULL
  expect_equal(.get_tune_metric_names(mt_spln_knn_bo), character(0))

  expect_equal(
    .get_tune_outcome_names(mt_spln_knn_bo),
    attributes(mt_spln_knn_bo)$outcomes
  )
  attr(mt_spln_knn_bo, "outcomes") <- NULL
  expect_equal(.get_tune_outcome_names(mt_spln_knn_bo), character(0))
})

test_that("accessor functions", {
  skip_if(utils::packageVersion("dials") <= "0.0.7")

  load(test_path("data", "test_objects.RData"))

  expect_equal(
    tibble::as_tibble(.get_tune_parameters(mt_knn_bo)),
    tibble::as_tibble(attributes(mt_knn_bo)$parameters)
  )
  attr(mt_knn_bo, "parameters") <- NULL
  expect_equal(.get_tune_parameters(mt_knn_bo), tibble::tibble())
})

# ------------------------------------------------------------------------------

test_that("rsample fingerprinting", {
  expect_equal(
    .get_fingerprint(ames_grid_search),
    "bfb2d02564c955d27ed78316b820e8ff"
  )
  expect_equal(
    .get_fingerprint(ames_iter_search),
    "bfb2d02564c955d27ed78316b820e8ff"
  )
})
