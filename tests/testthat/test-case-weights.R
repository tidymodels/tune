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

  folds <- rsample::vfold_cv(mtcars)

  spec <- parsnip::linear_reg()
  metrics <- yardstick::metric_set(yardstick::rmse)

  wf <- workflows::workflow()
  wf <- workflows::add_variables(wf, mpg, c(disp, cyl))
  wf <- workflows::add_model(wf, spec)

  fit1 <- fit_resamples(wf, resamples = folds, metrics = metrics)
  fit2 <- fit_resamples(wf, resamples = folds, metrics = metrics)

  wf <- workflows::add_case_weights(wf, weight)

  fit_weighted <- fit_resamples(wf, resamples = folds, metrics = metrics)

  # Baseline
  expect_identical(
    fit1$.metrics[[1]]$.estimate[[1]],
    fit2$.metrics[[1]]$.estimate[[1]]
  )

  # Weighted results are different
  expect_true(
    fit1$.metrics[[1]]$.estimate[[1]] != fit_weighted$.metrics[[1]]$.estimate[[1]]
  )
})
