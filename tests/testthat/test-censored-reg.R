test_that("evaluation time", {
  skip_if_not_installed("censored")
  skip_if_not_installed("yardstick", minimum_version = "1.1.0.9000")

  suppressPackageStartupMessages(library(tune))
  suppressPackageStartupMessages(library(censored))
  suppressPackageStartupMessages(library(yardstick))
  suppressPackageStartupMessages(library(rsample))

  spec <- survival_reg()
  set.seed(1)
  rs <- vfold_cv(stanford2, strata = status)
  .time <- seq(1, 1000, length = 5)
  mtr <- metric_set(brier_survival)
  reg_mtr <- metric_set(rmse)

  expect_snapshot(error = TRUE,
    spec %>% tune_grid(Surv(time, status) ~ ., resamples = rs, metrics = mtr)
  )
  expect_snapshot(error = TRUE,
    spec %>% tune_grid(Surv(time, status) ~ ., resamples = rs, metrics = reg_mtr)
  )
  expect_snapshot(
    linear_reg() %>% tune_grid(age ~ ., resamples = rs, metrics = reg_mtr, eval_time = 1)
  )

  expect_snapshot(error = TRUE,
    no_usable_times <-
      spec %>%
      tune_grid(Surv(time, status) ~ ., resamples = rs, metrics = mtr, eval_time = c(-1, Inf))
  )

  times <- 4:1
  expect_equal(get_metric_time(metric_set(brier_survival), times), 4)
  expect_equal(get_metric_time(metric_set(concordance_survival), times), NULL)
  expect_equal(get_metric_time(metric_set(brier_survival_integrated), times), NULL)
  expect_equal(
    get_metric_time(
      metric_set(brier_survival, brier_survival_integrated, concordance_survival),
      times
    ),
    4
  )

})
