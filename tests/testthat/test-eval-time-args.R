test_that("eval time inputs are checked for regression models", {
  library(parsnip)
  library(workflows)
  library(yardstick)

  times <- c(1, 1:3)

  met_reg <- metric_set(rmse)

  expect_snapshot(check_eval_time_arg(NULL, met_reg))
  expect_snapshot(check_eval_time_arg(times, met_reg))
})

test_that("eval time are checked for classification models", {
  library(parsnip)
  library(workflows)
  library(yardstick)

  times <- c(1, 1:3)

  met_cls <- metric_set(brier_class)

  expect_snapshot(check_eval_time_arg(NULL, met_cls))
  expect_snapshot(check_eval_time_arg(times, met_cls))
})
