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

test_that("`extract_case_weights()` works", {
  set.seed(1)

  mtcars$weight <- hardhat::frequency_weights(1:32)

  folds <- rsample::vfold_cv(mtcars, v = 2)

  spec <- parsnip::linear_reg()
  metrics <- yardstick::metric_set(yardstick::rmse)

  wf <- workflows::workflow()
  wf <- workflows::add_variables(wf, mpg, c(disp, cyl))
  wf <- workflows::add_model(wf, spec)
  wf <- workflows::add_case_weights(wf, weight)

  extracted <- tune:::extract_case_weights(mtcars, wf)
  expted <-  mtcars |>
    tibble::as_tibble() |>
    dplyr::select(.case_weights = weight)
  expect_equal(extracted, expted)
})

# ------------------------------------------------------------------------------
# Passed on during tuning

test_that("weights are used during resampling", {
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

  manual_with_wts <-
    predictions |>
    yardstick::rmse(mpg, .pred, case_weight = .case_weights)

  manual_with_no_wts <-
    predictions |>
    yardstick::rmse(mpg, .pred)

  res_metric <- res$.metrics[[1]] |> dplyr::select(-.config)

  expect_equal(res_metric, manual_with_wts)
  expect_true(!identical(res_metric, manual_with_no_wts))

})


test_that("weights are used during tuning", {
  skip_if_not_installed("splines2")
  set.seed(1)

  mtcars$weight <- hardhat::frequency_weights(1:32)

  folds <- rsample::vfold_cv(mtcars, v = 2)

  rec <-
    recipes::recipe(mpg ~ ., data = mtcars) |>
    recipes::step_spline_natural(disp, deg_free = tune())

  spec <- parsnip::linear_reg()
  metrics <- yardstick::metric_set(yardstick::rmse)

  wf <- workflows::workflow()
  wf <- workflows::add_recipe(wf, rec)
  wf <- workflows::add_model(wf, spec)
  wf <- workflows::add_case_weights(wf, weight)

  df_grid <- tibble(deg_free = 3:4)

  res <- tune_grid(
    object = wf,
    resamples = folds,
    metrics = metrics,
    grid = df_grid,
    control = control_resamples(save_pred = TRUE)
  )

  predictions <- res$.predictions[[1]] |> group_by(.config)

  manual_with_wts <-
    predictions |>
    yardstick::rmse(mpg, .pred, case_weight = .case_weights)

  manual_with_no_wts <-
    predictions |>
    yardstick::rmse(mpg, .pred)

  res_metric <- res$.metrics[[1]] |> dplyr::select(all_of(names(manual_with_wts)))

  expect_equal(res_metric, manual_with_wts)
  expect_true(!identical(res_metric, manual_with_no_wts))
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

  predictions <- res$.predictions[[1]] |> group_by(.config)

  manual_with_wts <-
    predictions |>
    yardstick::rmse(mpg, .pred, case_weight = .case_weights)

  manual_with_no_wts <-
    predictions |>
    yardstick::rmse(mpg, .pred)

  res_metric <- res$.metrics[[1]] |> dplyr::select(all_of(names(manual_with_wts)))

  expect_equal(res_metric, manual_with_wts)
  expect_true(!identical(res_metric, manual_with_no_wts))
})

test_that("importance weights are *not* used during prediction", {
  skip_if_not_installed("splines2")
  set.seed(1)

  mtcars$weight <- hardhat::importance_weights((1:32)/32)

  folds <- rsample::vfold_cv(mtcars, v = 2)

  rec <-
    recipes::recipe(mpg ~ ., data = mtcars) |>
    recipes::step_spline_natural(disp, deg_free = tune())

  spec <- parsnip::linear_reg()
  metrics <- yardstick::metric_set(yardstick::rmse)

  wf <- workflows::workflow()
  wf <- workflows::add_recipe(wf, rec)
  wf <- workflows::add_model(wf, spec)
  wf <- workflows::add_case_weights(wf, weight)

  df_grid <- tibble(deg_free = 3:4)

  res <- tune_grid(
    object = wf,
    resamples = folds,
    metrics = metrics,
    grid = df_grid,
    control = control_resamples(save_pred = TRUE)
  )

  predictions <- res$.predictions[[1]] |> group_by(.config)

  expect_true(!any(names(predictions) == ".case_weights"))

  manual_with_no_wts <-
    predictions |>
    yardstick::rmse(mpg, .pred)

  res_metric <- res$.metrics[[1]] |> dplyr::select(all_of(names(manual_with_no_wts)))

  expect_equal(res_metric, manual_with_no_wts)
})

