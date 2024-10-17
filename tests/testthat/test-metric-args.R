test_that("metric inputs are checked for regression models", {
  skip_if_not_installed("kknn")


  library(parsnip)
  library(workflows)
  library(yardstick)
  library(rsample)

  wflow <- workflow(y ~ X1 + X2, linear_reg())
  knn_spec <- nearest_neighbor(neighbors = tune()) %>% set_mode("regression")
  wflow_tune <- workflow(y ~ X1 + X2, knn_spec)

  set.seed(1)
  split <- initial_split(mtcars)
  rs <- vfold_cv(mtcars)

  # ------------------------------------------------------------------------------
  # setup metric sets

  met_mix_int <-
    metric_set(brier_survival_integrated,
               brier_survival,
               concordance_survival)
  met_reg <- metric_set(rmse)
  met_cls <- metric_set(brier_class)

  # ------------------------------------------------------------------------------
  # check inputs

  expect_snapshot(check_metrics_arg(NULL, wflow))

  expect_snapshot(check_metrics_arg(met_reg, wflow))
  expect_snapshot(check_metrics_arg(met_cls, wflow),     error = TRUE)
  expect_snapshot(check_metrics_arg(met_mix_int, wflow), error = TRUE)

  # ------------------------------------------------------------------------------
  # resampling

  expect_snapshot(fit_resamples(wflow, rs, metrics = met_cls),     error = TRUE)
  expect_snapshot(fit_resamples(wflow, rs, metrics = met_mix_int), error = TRUE)

  # ------------------------------------------------------------------------------
  # tuning

  expect_snapshot(tune_grid(wflow_tune, rs, metrics = met_cls),     error = TRUE)
  expect_snapshot(tune_grid(wflow_tune, rs, metrics = met_mix_int), error = TRUE)

  expect_snapshot(tune_bayes(wflow_tune, rs, metrics = met_cls),     error = TRUE)
  expect_snapshot(tune_bayes(wflow_tune, rs, metrics = met_mix_int), error = TRUE)

  # ------------------------------------------------------------------------------
  # final fit

  expect_snapshot(last_fit(wflow, split, metrics = met_cls),     error = TRUE)
  expect_snapshot(last_fit(wflow, split, metrics = met_mix_int), error = TRUE)

})

test_that("metric inputs are checked for classification models", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("kknn")

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

  # ------------------------------------------------------------------------------
  # setup metric sets

  met_mix_int <-
    metric_set(brier_survival_integrated,
               brier_survival,
               concordance_survival)
  met_reg <- metric_set(rmse)
  met_cls <- metric_set(brier_class)

  # ------------------------------------------------------------------------------
  # check inputs

  expect_snapshot(check_metrics_arg(NULL, wflow))

  expect_snapshot(check_metrics_arg(met_reg, wflow),     error = TRUE)
  expect_snapshot(check_metrics_arg(met_cls, wflow))
  expect_snapshot(check_metrics_arg(met_mix_int, wflow), error = TRUE)

  # ------------------------------------------------------------------------------
  # resampling

  expect_snapshot(fit_resamples(wflow, rs, metrics = met_reg),     error = TRUE)
  expect_snapshot(fit_resamples(wflow, rs, metrics = met_mix_int), error = TRUE)

  # ------------------------------------------------------------------------------
  # tuning

  expect_snapshot(tune_grid(wflow_tune, rs, metrics = met_reg),     error = TRUE)
  expect_snapshot(tune_grid(wflow_tune, rs, metrics = met_mix_int), error = TRUE)

  expect_snapshot(tune_bayes(wflow_tune, rs, metrics = met_reg),     error = TRUE)
  expect_snapshot(tune_bayes(wflow_tune, rs, metrics = met_mix_int), error = TRUE)

  # ------------------------------------------------------------------------------
  # final fit

  expect_snapshot(last_fit(wflow, split, metrics = met_reg),     error = TRUE)
  expect_snapshot(last_fit(wflow, split, metrics = met_mix_int), error = TRUE)
})


test_that("metric inputs are checked for censored regression models", {
  skip_if_not_installed("censored")
  library(parsnip)
  library(workflows)
  library(yardstick)
  library(rsample)
  library(censored)

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

  met_srv <- metric_set(concordance_survival)
  met_reg <- metric_set(rmse)
  met_cls <- metric_set(brier_class)

  # ------------------------------------------------------------------------------
  # check inputs

  expect_snapshot(check_metrics_arg(NULL, wflow))

  expect_snapshot(check_metrics_arg(met_reg, wflow), error = TRUE)
  expect_snapshot(check_metrics_arg(met_cls, wflow), error = TRUE)
  expect_snapshot(check_metrics_arg(met_srv, wflow))

  # ------------------------------------------------------------------------------
  # resampling

  expect_snapshot(fit_resamples(wflow, rs, metrics = met_cls), error = TRUE)
  expect_snapshot(fit_resamples(wflow, rs, metrics = met_reg), error = TRUE)

  # ------------------------------------------------------------------------------
  # tuning

  expect_snapshot(tune_grid(wflow_tune, rs, metrics = met_cls), error = TRUE)
  expect_snapshot(tune_grid(wflow_tune, rs, metrics = met_reg), error = TRUE)

  expect_snapshot(tune_bayes(wflow_tune, rs, metrics = met_cls), error = TRUE)
  expect_snapshot(tune_bayes(wflow_tune, rs, metrics = met_reg), error = TRUE)

  # ------------------------------------------------------------------------------
  # final fit

  expect_snapshot(last_fit(wflow, split, metrics = met_cls), error = TRUE)
  expect_snapshot(last_fit(wflow, split, metrics = met_reg), error = TRUE)

})


