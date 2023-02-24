# ------------------------------------------------------------------------------
# .use_case_weights_with_yardstick()

test_that("knows about importance weights", {
  x <- hardhat::importance_weights(1)
  expect_false(.use_case_weights_with_yardstick(x))
})

test_that("knows about frequency weights", {
  x <- hardhat::frequency_weights(1)
  expect_true(.use_case_weights_with_yardstick(x))
})

test_that("gives informative default error", {
  expect_snapshot(error = TRUE, {
    .use_case_weights_with_yardstick(1)
  })
})

# ------------------------------------------------------------------------------
# extract_case_weights()

test_that("`extract_case_weights()` errors if `col` doesn't exist", {
  skip_if(packageVersion("rlang") < "1.0.6.9000")
  wf <- workflows::workflow()

  expect_snapshot(error = TRUE, {
    extract_case_weights(mtcars, wf)
  })
})

test_that("`extract_case_weights()` errors if case weights column isn't the right class", {
  mtcars$weight <- 1L

  wf <- workflows::workflow()
  wf <- workflows::add_case_weights(wf, weight)

  expect_snapshot(error = TRUE, {
    extract_case_weights(mtcars, wf)
  })
})

# ------------------------------------------------------------------------------
# Passed on during tuning

test_that("weights are used during tuning", {
  set.seed(1)

  mtcars$weight <- hardhat::frequency_weights(1:32)

  folds <- rsample::vfold_cv(mtcars, v = 2)

  spec <- parsnip::linear_reg()
  metrics <- yardstick::metric_set(yardstick::rmse)

  wf <- workflows::workflow()
  wf <- workflows::add_variables(wf, mpg, c(disp, cyl))
  wf <- workflows::add_model(wf, spec)
  wf <- workflows::add_case_weights(wf, weight)

  res <- fit_resamples(
    object = wf,
    resamples = folds,
    metrics = metrics,
    control = control_resamples(save_pred = TRUE)
  )

  predictions <- res$.predictions[[1]]

  new_data <- rsample::assessment(res$splits[[1]])
  new_data[["predictions"]] <- predictions$.pred

  expected_metric <- res$.metrics[[1]]
  expect_true(nrow(expected_metric) == 1)
  expected_metric <- expected_metric$.estimate

  actual_metric <- yardstick::rmse(new_data, mpg, predictions, case_weight = weight)
  actual_metric <- actual_metric$.estimate

  expect_identical(actual_metric, expected_metric)
})

test_that("weights work with multi-predict", {
  # glmnet depends on >= 3.6.0 so we don't test on CRAN
  skip_if_not_installed("glmnet")

  set.seed(1)

  mtcars$weight <- hardhat::frequency_weights(1:32)

  folds <- rsample::vfold_cv(mtcars, v = 2)

  spec <- parsnip::linear_reg(penalty = tune(), mixture = tune())
  spec <- parsnip::set_engine(spec, "glmnet")

  grid <- expand.grid(penalty = 1:3, mixture = (1:5) / 5)

  metrics <- yardstick::metric_set(yardstick::rmse)

  wf <- workflows::workflow()
  wf <- workflows::add_variables(wf, mpg, c(disp, cyl))
  wf <- workflows::add_model(wf, spec)
  wf <- workflows::add_case_weights(wf, weight)

  res <- tune_grid(
    object = wf,
    resamples = folds,
    grid = grid,
    control = control_grid(save_pred = TRUE),
    metrics = metrics
  )

  penalty <- grid$penalty[[1]]
  mixture <- grid$mixture[[1]]

  predictions <- res$.predictions[[1]]
  predictions <- dplyr::filter(predictions, penalty == !!penalty, mixture == !!mixture)

  new_data <- rsample::assessment(res$splits[[1]])
  new_data[["predictions"]] <- predictions$.pred

  expected_metric <- res$.metrics[[1]]
  expected_metric <- dplyr::filter(expected_metric, penalty == !!penalty, mixture == !!mixture)
  expect_true(nrow(expected_metric) == 1)
  expected_metric <- expected_metric$.estimate

  actual_metric <- yardstick::rmse(new_data, mpg, predictions, case_weight = weight)
  actual_metric <- actual_metric$.estimate

  expect_identical(actual_metric, expected_metric)
})
