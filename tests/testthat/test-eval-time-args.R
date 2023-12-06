test_that("eval time inputs are checked for regression models", {
  library(parsnip)
  library(workflows)
  library(yardstick)
  library(rsample)

  # ------------------------------------------------------------------------------

  wflow <- workflow(mpg ~ ., linear_reg())
  knn_spec <- nearest_neighbor(neighbors = tune()) %>% set_mode("regression")
  wflow_tune <- workflow(mpg ~ ., knn_spec)

  set.seed(1)
  split <- initial_split(mtcars)
  rs <- vfold_cv(mtcars)

  times <- c(1, 1:3)

  # ------------------------------------------------------------------------------
  # setup metric sets

  met_reg <- metric_set(rmse)

  # ------------------------------------------------------------------------------
  # check inputs

  expect_snapshot(check_eval_time_arg(NULL, met_reg))
  expect_snapshot(check_eval_time_arg(times, met_reg))

  # ------------------------------------------------------------------------------
  # resampling

  expect_snapshot(
    res <- fit_resamples(wflow, rs, eval_time = times)
  )

  # ------------------------------------------------------------------------------
  # tuning

  expect_snapshot({
    set.seed(1)
    res <- tune_grid(wflow_tune, rs, eval_time = times)
  })

  expect_snapshot({
    set.seed(1)
    res <- tune_bayes(wflow_tune, rs, iter = 1, eval_time = times)
  })

  # ------------------------------------------------------------------------------
  # final fit

  expect_snapshot(
    res <- last_fit(wflow, split, eval_time = times)
  )

})

test_that("eval time are checked for classification models", {
  library(parsnip)
  library(workflows)
  library(yardstick)
  library(rsample)

  data(two_class_dat, package = "modeldata")
  wflow <- workflow(Class ~ A + B, logistic_reg())
  knn_spec <- nearest_neighbor(neighbors = tune()) %>% set_mode("classification")
  wflow_tune <- workflow(Class ~ A + B, knn_spec)

  set.seed(1)
  split <- initial_split(two_class_dat)
  rs <- vfold_cv(two_class_dat)

  times <- c(1, 1:3)

  # ------------------------------------------------------------------------------
  # setup metric sets

  met_cls <- metric_set(brier_class)

  # ------------------------------------------------------------------------------
  # check inputs

  expect_snapshot(check_eval_time_arg(NULL, met_cls))
  expect_snapshot(check_eval_time_arg(times, met_cls))

  # ------------------------------------------------------------------------------
  # resampling

  expect_snapshot(
    res <- fit_resamples(wflow, rs, eval_time = times)
  )

  # ------------------------------------------------------------------------------
  # tuning

  expect_snapshot({
    set.seed(1)
    res <- tune_grid(wflow_tune, rs, eval_time = times)
  })

  expect_snapshot({
    set.seed(1)
    res <- tune_bayes(wflow_tune, rs, iter = 1, eval_time = times)
  })

  # ------------------------------------------------------------------------------
  # final fit

  expect_snapshot(
    res <- last_fit(wflow, split, eval_time = times)
  )

})
