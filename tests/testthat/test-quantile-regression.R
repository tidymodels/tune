test_that("resampling quantile regression models", {
  skip_if_not_installed("parsnip", minimum_version = "1.4.1.9002")
  skip_if_not_installed("xgboost", minimum_version = "3.1.3.1")
  skip_if_not_installed("modeldata")

  set.seed(1)
  sim_dat <- modeldata::sim_regression(200)
  sim_rs <- rsample::bootstraps(sim_dat, times = 3)

  bst_spec <-
    parsnip::boost_tree(min_n = 10, trees = 10) |>
    set_mode("quantile regression", quantile_levels = (0:10) / 10)

  set.seed(2)
  bst_res <-
    bst_spec |>
    fit_resamples(
      outcome ~ .,
      resamples = sim_rs,
      control = control_resamples(save_pred = TRUE)
    )

  expect_s3_class(bst_res, "tune_results")

  bst_mtr <- collect_metrics(bst_res)
  expect_true(bst_mtr$.metric == "weighted_interval_score")
  expect_true(nrow(bst_mtr) == 1)

  bst_pred_ind <- collect_predictions(bst_res, summarize = FALSE)
  expect_s3_class(
    bst_pred_ind$.pred_quantile,
    c("quantile_pred", "vctrs_rcrd", "vctrs_vctr")
  )

  bst_pred_mean <- collect_predictions(bst_res, summarize = TRUE)
  expect_s3_class(
    bst_pred_mean$.pred_quantile,
    c("quantile_pred", "vctrs_rcrd", "vctrs_vctr")
  )
  expect_true(nrow(bst_pred_mean) < nrow(bst_pred_ind))
  expect_true(all(table(bst_pred_mean$.row) == 1))
})

test_that("tuning quantile regression models", {
  skip_if_not_installed("parsnip", minimum_version = "1.4.1.9002")
  skip_if_not_installed("xgboost", minimum_version = "3.1.3.1")
  skip_if_not_installed("modeldata")

  set.seed(1)
  sim_dat <- modeldata::sim_regression(200)
  sim_rs <- rsample::bootstraps(sim_dat, times = 3)

  bst_spec <-
    parsnip::boost_tree(min_n = tune(), trees = 10) |>
    set_mode("quantile regression", quantile_levels = (0:10) / 10)

  set.seed(2)
  bst_res <-
    bst_spec |>
    tune_grid(
      outcome ~ .,
      resamples = sim_rs,
      control = control_grid(save_pred = TRUE, save_workflow = TRUE),
      grid = tibble::tibble(min_n = c(2, 5, 10))
    )

  expect_s3_class(bst_res, "tune_results")

  bst_mtr <- collect_metrics(bst_res)
  expect_true(all(bst_mtr$.metric == "weighted_interval_score"))
  expect_true(nrow(bst_mtr) == 3)

  bst_pred_ind <- collect_predictions(bst_res, summarize = FALSE)
  expect_s3_class(
    bst_pred_ind$.pred_quantile,
    c("quantile_pred", "vctrs_rcrd", "vctrs_vctr")
  )

  bst_pred_mean <- collect_predictions(bst_res, summarize = TRUE)
  expect_s3_class(
    bst_pred_mean$.pred_quantile,
    c("quantile_pred", "vctrs_rcrd", "vctrs_vctr")
  )
  expect_true(nrow(bst_pred_mean) < nrow(bst_pred_ind))
  expect_true(all(table(bst_pred_mean$.row) == 3))

  bst_best <- select_best(bst_res, metric = "weighted_interval_score")
  expect_equal(bst_best$min_n, 10L)

  set.seed(3)
  bst_fit_best <- fit_best(bst_res, metric = "weighted_interval_score")
  expect_s3_class(bst_fit_best, "workflow")
  expect_s3_class(bst_fit_best |> extract_fit_engine(), "xgb.Booster")

  bst_plot <- autoplot(bst_res)
  expect_s3_class(bst_plot, "ggplot")
})


test_that("bootstrap intervals for quantile regression models", {
  skip_if_not_installed("parsnip", minimum_version = "1.4.1.9002")
  skip_if_not_installed("xgboost", minimum_version = "3.1.3.1")
  skip_if_not_installed("modeldata")

  set.seed(1)
  sim_dat <- modeldata::sim_regression(200)
  sim_rs <- rsample::bootstraps(sim_dat, times = 3)

  bst_spec <-
    parsnip::boost_tree(min_n = tune(), trees = 10) |>
    set_mode("quantile regression", quantile_levels = (0:10) / 10)

  set.seed(2)
  bst_res <-
    bst_spec |>
    tune_grid(
      outcome ~ .,
      resamples = sim_rs,
      control = control_grid(save_pred = TRUE),
      grid = tibble::tibble(min_n = c(2, 5, 10))
    )

  set.seed(3)
  expect_warning(
    bst_int <- rsample::int_pctl(bst_res, alpha = 0.1, times = 100)
  )
  expect_true(tibble::is_tibble(bst_int))
  expect_equal(nrow(bst_int), 3L)
})
