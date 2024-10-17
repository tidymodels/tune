test_that("eval time inputs are checked for regression models", {
  skip_if_not_installed("kknn")

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
  skip_if_not_installed("modeldata")
  skip_if_not_installed("kknn")

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

test_that("eval time inputs are checked for censored regression models", {
  skip_if_not_installed("censored")

  library(parsnip)
  library(workflows)
  library(yardstick)
  library(rsample)
  suppressPackageStartupMessages(library(censored))

  stanford2$event_time <- Surv(stanford2$time, stanford2$status)
  stanford2 <- stanford2[, c("event_time", "age")]

  wflow <- workflow(event_time ~ age, survival_reg())
  sr_spec <- survival_reg(dist = tune())
  wflow_tune <- workflow(event_time ~ age, sr_spec)

  set.seed(1)
  split <- initial_split(stanford2)
  rs <- vfold_cv(stanford2)

  # ------------------------------------------------------------------------------
  # setup metric sets

  met_stc <- metric_set(concordance_survival)
  met_dyn <- metric_set(brier_survival)
  met_int <- metric_set(brier_survival_integrated)
  met_stc_dyn <- metric_set(concordance_survival, brier_survival)
  met_stc_int <- metric_set(concordance_survival, brier_survival_integrated)
  met_dyn_stc <- metric_set(brier_survival, concordance_survival)
  met_dyn_int <- metric_set(brier_survival, brier_survival_integrated)
  met_int_stc <- metric_set(brier_survival_integrated, concordance_survival)
  met_int_dyn <- metric_set(brier_survival_integrated, brier_survival)

  # ------------------------------------------------------------------------------
  # check inputs when eval_time left out

  expect_snapshot(check_eval_time_arg(NULL, met_stc))
  expect_snapshot(check_eval_time_arg(NULL, met_dyn), error = TRUE)
  expect_snapshot(check_eval_time_arg(NULL, met_int), error = TRUE)

  expect_snapshot(check_eval_time_arg(NULL, met_stc_dyn), error = TRUE)
  expect_snapshot(check_eval_time_arg(NULL, met_stc_int), error = TRUE)
  expect_snapshot(check_eval_time_arg(NULL, met_dyn_stc), error = TRUE)

  expect_snapshot(check_eval_time_arg(NULL, met_dyn_int), error = TRUE)
  expect_snapshot(check_eval_time_arg(NULL, met_int_stc), error = TRUE)
  expect_snapshot(check_eval_time_arg(NULL, met_int_dyn), error = TRUE)

  # ------------------------------------------------------------------------------
  # check inputs with single eval times

  expect_snapshot(check_eval_time_arg(2, met_stc))
  expect_snapshot(check_eval_time_arg(2, met_dyn))
  expect_snapshot(check_eval_time_arg(2, met_int), error = TRUE)

  expect_snapshot(check_eval_time_arg(2, met_stc_dyn))
  expect_snapshot(check_eval_time_arg(2, met_stc_int), error = TRUE)

  expect_snapshot(check_eval_time_arg(2, met_dyn_stc))
  expect_snapshot(check_eval_time_arg(2, met_dyn_int), error = TRUE)

  expect_snapshot(check_eval_time_arg(2, met_int_stc), error = TRUE)
  expect_snapshot(check_eval_time_arg(2, met_int_dyn), error = TRUE)

  # ------------------------------------------------------------------------------
  # check inputs with multiple eval times

  expect_snapshot(check_eval_time_arg(1:3, met_stc))
  expect_snapshot(check_eval_time_arg(1:3, met_dyn))
  expect_snapshot(check_eval_time_arg(1:3, met_int))

  expect_snapshot(check_eval_time_arg(1:3, met_stc_dyn))
  expect_snapshot(check_eval_time_arg(1:3, met_stc_int))
  expect_snapshot(check_eval_time_arg(1:3, met_dyn_stc))

  expect_snapshot(check_eval_time_arg(1:3, met_dyn_int))
  expect_snapshot(check_eval_time_arg(1:3, met_int_stc))
  expect_snapshot(check_eval_time_arg(1:3, met_int_dyn))

  # ------------------------------------------------------------------------------
  # resampling

  # no eval time
  expect_snapshot(res <- fit_resamples(wflow, rs, metrics = met_stc))
  expect_snapshot(fit_resamples(wflow, rs, metrics = met_dyn), error = TRUE)
  expect_snapshot(fit_resamples(wflow, rs, metrics = met_int), error = TRUE)

  expect_snapshot(fit_resamples(wflow, rs, metrics = met_stc_dyn), error = TRUE)
  expect_snapshot(fit_resamples(wflow, rs, metrics = met_stc_int), error = TRUE)
  expect_snapshot(fit_resamples(wflow, rs, metrics = met_dyn_stc), error = TRUE)

  expect_snapshot(fit_resamples(wflow, rs, metrics = met_dyn_int), error = TRUE)
  expect_snapshot(fit_resamples(wflow, rs, metrics = met_int_stc), error = TRUE)
  expect_snapshot(fit_resamples(wflow, rs, metrics = met_int_dyn), error = TRUE)

  # one eval time
  expect_snapshot(res <- fit_resamples(wflow, rs, metrics = met_stc, eval_time = 2))
  expect_snapshot(res <- fit_resamples(wflow, rs, metrics = met_dyn, eval_time = 2))
  expect_snapshot(fit_resamples(wflow, rs, metrics = met_int, eval_time = 2), error = TRUE)

  expect_snapshot(res <- fit_resamples(wflow, rs, metrics = met_stc_dyn, eval_time = 2))
  expect_snapshot(fit_resamples(wflow, rs, metrics = met_stc_int, eval_time = 2), error = TRUE)

  expect_snapshot(res <- fit_resamples(wflow, rs, metrics = met_dyn_stc, eval_time = 2))
  expect_snapshot(fit_resamples(wflow, rs, metrics = met_dyn_int, eval_time = 2), error = TRUE)

  expect_snapshot(fit_resamples(wflow, rs, metrics = met_int_stc, eval_time = 2), error = TRUE)
  expect_snapshot(fit_resamples(wflow, rs, metrics = met_int_dyn, eval_time = 2), error = TRUE)

  # multiple eval times
  expect_snapshot(res <- fit_resamples(wflow, rs, metrics = met_stc, eval_time = 1:3))
  expect_snapshot(res <- fit_resamples(wflow, rs, metrics = met_dyn, eval_time = 1:3))
  expect_snapshot(res <- fit_resamples(wflow, rs, metrics = met_int, eval_time = 1:3))

  expect_snapshot(res <- fit_resamples(wflow, rs, metrics = met_stc_dyn, eval_time = 1:3))
  expect_snapshot(res <- fit_resamples(wflow, rs, metrics = met_stc_int, eval_time = 1:3))

  expect_snapshot(res <- fit_resamples(wflow, rs, metrics = met_dyn_stc, eval_time = 1:3))
  expect_snapshot(res <- fit_resamples(wflow, rs, metrics = met_dyn_int, eval_time = 1:3))

  expect_snapshot(res <- fit_resamples(wflow, rs, metrics = met_int_stc, eval_time = 1:3))
  expect_snapshot(res <- fit_resamples(wflow, rs, metrics = met_int_dyn, eval_time = 1:3))

  # ------------------------------------------------------------------------------
  # grid tuning (tune bayes tests in extratests repo)

  # no eval time
  expect_snapshot(res <- tune_grid(wflow_tune, rs, metrics = met_stc))
  expect_snapshot(tune_grid(wflow_tune, rs, metrics = met_dyn), error = TRUE)
  expect_snapshot(tune_grid(wflow_tune, rs, metrics = met_int), error = TRUE)

  expect_snapshot(tune_grid(wflow_tune, rs, metrics = met_stc_dyn), error = TRUE)
  expect_snapshot(tune_grid(wflow_tune, rs, metrics = met_stc_int), error = TRUE)
  expect_snapshot(tune_grid(wflow_tune, rs, metrics = met_dyn_stc), error = TRUE)

  expect_snapshot(tune_grid(wflow_tune, rs, metrics = met_dyn_int), error = TRUE)
  expect_snapshot(tune_grid(wflow_tune, rs, metrics = met_int_stc), error = TRUE)
  expect_snapshot(tune_grid(wflow_tune, rs, metrics = met_int_dyn), error = TRUE)

  # one eval time
  expect_snapshot(res <- tune_grid(wflow_tune, rs, metrics = met_stc, eval_time = 2))
  expect_snapshot(res <- tune_grid(wflow_tune, rs, metrics = met_dyn, eval_time = 2))
  expect_snapshot(tune_grid(wflow_tune, rs, metrics = met_int, eval_time = 2), error = TRUE)

  expect_snapshot(res <- tune_grid(wflow_tune, rs, metrics = met_stc_dyn, eval_time = 2))
  expect_snapshot(tune_grid(wflow_tune, rs, metrics = met_stc_int, eval_time = 2), error = TRUE)

  expect_snapshot(res <- tune_grid(wflow_tune, rs, metrics = met_dyn_stc, eval_time = 2))
  expect_snapshot(tune_grid(wflow_tune, rs, metrics = met_dyn_int, eval_time = 2), error = TRUE)

  expect_snapshot(tune_grid(wflow_tune, rs, metrics = met_int_stc, eval_time = 2), error = TRUE)
  expect_snapshot(tune_grid(wflow_tune, rs, metrics = met_int_dyn, eval_time = 2), error = TRUE)

  # multiple eval times
  expect_snapshot(res <- tune_grid(wflow_tune, rs, metrics = met_stc, eval_time = 1:3))
  expect_snapshot(res <- tune_grid(wflow_tune, rs, metrics = met_dyn, eval_time = 1:3))
  expect_snapshot(res <- tune_grid(wflow_tune, rs, metrics = met_int, eval_time = 1:3))

  expect_snapshot(res <- tune_grid(wflow_tune, rs, metrics = met_stc_dyn, eval_time = 1:3))
  expect_snapshot(res <- tune_grid(wflow_tune, rs, metrics = met_stc_int, eval_time = 1:3))

  expect_snapshot(res <- tune_grid(wflow_tune, rs, metrics = met_dyn_stc, eval_time = 1:3))
  expect_snapshot(res <- tune_grid(wflow_tune, rs, metrics = met_dyn_int, eval_time = 1:3))

  expect_snapshot(res <- tune_grid(wflow_tune, rs, metrics = met_int_stc, eval_time = 1:3))
  expect_snapshot(res <- tune_grid(wflow_tune, rs, metrics = met_int_dyn, eval_time = 1:3))

  # ------------------------------------------------------------------------------
  # last fit

  # no eval time
  expect_silent(res <- last_fit(wflow, split, metrics = met_stc))
  expect_snapshot(last_fit(wflow, split, metrics = met_dyn), error = TRUE)
  expect_snapshot(last_fit(wflow, split, metrics = met_int), error = TRUE)

  expect_snapshot(last_fit(wflow, split, metrics = met_stc_dyn), error = TRUE)
  expect_snapshot(last_fit(wflow, split, metrics = met_stc_int), error = TRUE)
  expect_snapshot(last_fit(wflow, split, metrics = met_dyn_stc), error = TRUE)

  expect_snapshot(last_fit(wflow, split, metrics = met_dyn_int), error = TRUE)
  expect_snapshot(last_fit(wflow, split, metrics = met_int_stc), error = TRUE)
  expect_snapshot(last_fit(wflow, split, metrics = met_int_dyn), error = TRUE)

  # one eval time
  expect_snapshot(res <- last_fit(wflow, split, metrics = met_stc, eval_time = 2))
  expect_snapshot(res <- last_fit(wflow, split, metrics = met_dyn, eval_time = 2))
  expect_snapshot(last_fit(wflow, split, metrics = met_int, eval_time = 2), error = TRUE)

  expect_snapshot(res <- last_fit(wflow, split, metrics = met_stc_dyn, eval_time = 2))
  expect_snapshot(last_fit(wflow, split, metrics = met_stc_int, eval_time = 2), error = TRUE)

  expect_snapshot(res <- last_fit(wflow, split, metrics = met_dyn_stc, eval_time = 2))
  expect_snapshot(last_fit(wflow, split, metrics = met_dyn_int, eval_time = 2), error = TRUE)

  expect_snapshot(last_fit(wflow, split, metrics = met_int_stc, eval_time = 2), error = TRUE)
  expect_snapshot(last_fit(wflow, split, metrics = met_int_dyn, eval_time = 2), error = TRUE)

  # multiple eval times
  expect_snapshot(res <- last_fit(wflow, split, metrics = met_stc, eval_time = 1:3))
  expect_snapshot(res <- last_fit(wflow, split, metrics = met_dyn, eval_time = 1:3))
  expect_snapshot(res <- last_fit(wflow, split, metrics = met_int, eval_time = 1:3))

  expect_snapshot(res <- last_fit(wflow, split, metrics = met_stc_dyn, eval_time = 1:3))
  expect_snapshot(res <- last_fit(wflow, split, metrics = met_stc_int, eval_time = 1:3))

  expect_snapshot(res <- last_fit(wflow, split, metrics = met_dyn_stc, eval_time = 1:3))
  expect_snapshot(res <- last_fit(wflow, split, metrics = met_dyn_int, eval_time = 1:3))

  expect_snapshot(res <- last_fit(wflow, split, metrics = met_int_stc, eval_time = 1:3))
  expect_snapshot(res <- last_fit(wflow, split, metrics = met_int_dyn, eval_time = 1:3))


})

