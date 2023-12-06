test_that("metric inputs are checked for regression models", {
  library(parsnip)
  library(workflows)
  library(yardstick)

  wflow <- workflow(y ~ X1 + X2, linear_reg())

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
  expect_snapshot(check_metrics_arg(met_cls, wflow), error = TRUE)
  expect_snapshot(check_metrics_arg(met_mix_int, wflow), error = TRUE)
})

test_that("metric inputs are checked for classification models", {
  library(parsnip)
  library(workflows)
  library(yardstick)

  wflow <- workflow(y ~ X1 + X2, logistic_reg())

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

  expect_snapshot(check_metrics_arg(met_reg, wflow), error = TRUE)
  expect_snapshot(check_metrics_arg(met_cls, wflow))
  expect_snapshot(check_metrics_arg(met_mix_int, wflow), error = TRUE)
})
