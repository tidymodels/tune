test_that("gives same output as collect_metrics() when metrics match", {
  skip_on_cran()
  skip_if_not_installed("kknn")

  library(parsnip)
  library(rsample)
  library(yardstick)

  m_set <- metric_set(rmse)

  res <-
    tune_grid(
      nearest_neighbor("regression", neighbors = tune()),
      mpg ~ cyl + hp,
      bootstraps(mtcars, 5),
      grid = 3,
      metrics = m_set,
      control = control_grid(save_pred = TRUE)
    )

  collected_sum <- collect_metrics(res)
  computed_sum <- compute_metrics(res, m_set)

  expect_equal(collected_sum, computed_sum)

  collected_unsum <- collect_metrics(res, summarize = FALSE)
  computed_unsum <- compute_metrics(res, m_set, summarize = FALSE)

  expect_equal(collected_unsum, computed_unsum)
})

test_that("gives same output as collect_metrics() when metrics match (apparent)", {
  skip_on_cran()
  skip_if_not_installed("kknn")

  library(parsnip)
  library(rsample)
  library(yardstick)

  m_set <- metric_set(rmse)

  res <-
    fit_resamples(
      nearest_neighbor("regression"),
      mpg ~ cyl + hp,
      bootstraps(mtcars, 5, apparent = TRUE),
      metrics = m_set,
      control = control_grid(save_pred = TRUE)
    )

  collected_sum <- collect_metrics(res)
  computed_sum <- compute_metrics(res, m_set)

  expect_equal(collected_sum, computed_sum)

  collected_unsum <- collect_metrics(res, summarize = FALSE)
  computed_unsum <- compute_metrics(res, m_set, summarize = FALSE)

  expect_equal(collected_unsum, computed_unsum)
})

test_that("`metrics` argument works (numeric metrics)", {
  skip_on_cran()
  skip_if_not_installed("kknn")

  library(parsnip)
  library(rsample)
  library(yardstick)

  m_set_rmse <- metric_set(rmse)
  m_set_rsq  <- metric_set(rsq)

  set.seed(1)

  res_rmse <-
    tune_grid(
      nearest_neighbor("regression", neighbors = tune()),
      mpg ~ cyl + hp,
      bootstraps(mtcars, 5),
      grid = 3,
      metrics = m_set_rmse,
      control = control_grid(save_pred = TRUE)
    )

  set.seed(1)

  res_rsq <-
    tune_grid(
      nearest_neighbor("regression", neighbors = tune()),
      mpg ~ cyl + hp,
      bootstraps(mtcars, 5),
      grid = 3,
      metrics = m_set_rsq,
      control = control_grid(save_pred = TRUE)
    )

  collected_sum_rsq <- collect_metrics(res_rsq)
  computed_sum_rsq  <- compute_metrics(res_rmse, m_set_rsq)

  expect_equal(collected_sum_rsq, computed_sum_rsq)

  collected_sum_rmse <- collect_metrics(res_rmse)
  computed_sum_rmse  <- compute_metrics(res_rsq, m_set_rmse)

  expect_equal(collected_sum_rsq, computed_sum_rsq)

  collected_unsum_rsq <- collect_metrics(res_rsq, summarize = FALSE)
  computed_unsum_rsq  <- compute_metrics(res_rmse, m_set_rsq, summarize = FALSE)

  expect_equal(collected_unsum_rsq, computed_unsum_rsq)

  collected_unsum_rmse <- collect_metrics(res_rmse, summarize = FALSE)
  computed_unsum_rmse  <- compute_metrics(res_rsq, m_set_rmse, summarize = FALSE)

  expect_equal(collected_unsum_rmse, computed_unsum_rmse)
})

test_that("`metrics` argument works (compatible class metric types)", {
  skip_on_cran()
  skip_if_not_installed("kknn")
  skip_if_not_installed("modeldata")

  library(parsnip)
  library(rsample)
  library(yardstick)
  library(modeldata)

  set.seed(1)

  res_acc_auc <-
    tune_grid(
      nearest_neighbor("classification", neighbors = tune()),
      Class ~ .,
      vfold_cv(two_class_dat, 5),
      metrics = metric_set(accuracy, roc_auc),
      grid = 3,
      control = control_grid(save_pred = TRUE)
    )

  set.seed(1)

  res_class <-
    tune_grid(
      nearest_neighbor("classification", neighbors = tune()),
      Class ~ .,
      vfold_cv(two_class_dat, 5),
      metrics = metric_set(precision),
      grid = 3,
      control = control_grid(save_pred = TRUE)
    )

  set.seed(1)

  res_prob <-
    tune_grid(
      nearest_neighbor("classification", neighbors = tune()),
      Class ~ .,
      vfold_cv(two_class_dat, 5),
      metrics = metric_set(mn_log_loss),
      grid = 3,
      control = control_grid(save_pred = TRUE)
    )

  collected_sum_class <- collect_metrics(res_class)
  computed_sum_class  <- compute_metrics(res_acc_auc, metric_set(precision))

  expect_equal(collected_sum_class, computed_sum_class)

  collected_unsum_class <- collect_metrics(res_class, summarize = FALSE)
  computed_unsum_class  <- compute_metrics(res_acc_auc, metric_set(precision),
                                           summarize = FALSE)

  expect_equal(collected_unsum_class, computed_unsum_class)

  collected_sum_prob <- collect_metrics(res_prob)
  computed_sum_prob  <- compute_metrics(res_acc_auc, metric_set(mn_log_loss))

  expect_equal(collected_sum_prob, computed_sum_prob)

  collected_unsum_prob <- collect_metrics(res_prob, summarize = FALSE)
  computed_unsum_prob  <- compute_metrics(res_acc_auc, metric_set(mn_log_loss),
                                        summarize = FALSE)

  expect_equal(collected_unsum_prob, computed_unsum_prob)
})

test_that("`metrics` argument works (differing class metric types)", {
  skip_on_cran()
  skip_if_not_installed("kknn")
  skip_if_not_installed("modeldata")

  library(parsnip)
  library(rsample)
  library(yardstick)
  library(modeldata)

  # class metric
  m_set_acc <- metric_set(accuracy)
  # prob metric
  m_set_auc  <- metric_set(roc_auc)

  set.seed(1)

  res_acc <-
    tune_grid(
      nearest_neighbor("classification", neighbors = tune()),
      Class ~ .,
      vfold_cv(two_class_dat, 5),
      metrics = m_set_acc,
      grid = 3,
      control = control_grid(save_pred = TRUE)
    )

  set.seed(1)

  res_auc <-
    tune_grid(
      nearest_neighbor("classification", neighbors = tune()),
      Class ~ .,
      vfold_cv(two_class_dat, 5),
      metrics = m_set_auc,
      grid = 3,
      control = control_grid(save_pred = TRUE)
    )

  expect_snapshot(
    compute_metrics(res_auc, m_set_acc),
    error = TRUE
  )

  expect_snapshot(
    compute_metrics(res_acc, m_set_auc),
    error = TRUE
  )
})

test_that("`metrics` argument works (iterative tuning)", {
  # the pattern of tuning first with one metric, then with another,
  # and then testing equality of collected vs computed metrics is a bit
  # more complicated here, as a different metric will lead to different
  # proposed hyperparameters in the gaussian process.

  skip_on_cran()
  skip_if_not_installed("kknn")

  library(parsnip)
  library(rsample)
  library(yardstick)

  m_set_rmse <- metric_set(rmse)
  m_set_rsq  <- metric_set(rsq)
  m_set_both  <- metric_set(rmse, rsq)

  set.seed(1)

  res_rmse <-
    tune_bayes(
      nearest_neighbor("regression", neighbors = tune()),
      mpg ~ .,
      vfold_cv(mtcars, v = 3),
      metrics = m_set_rmse,
      control = tune::control_bayes(save_pred = TRUE),
      iter = 2, initial = 3
    )

  set.seed(1)

  res_both <-
    tune_bayes(
      nearest_neighbor("regression", neighbors = tune()),
      mpg ~ .,
      vfold_cv(mtcars, v = 3),
      metrics = m_set_both,
      control = tune::control_bayes(save_pred = TRUE),
      iter = 2, initial = 3
    )

  collected_sum_rmse <- collect_metrics(res_rmse)
  computed_sum_rmse  <- compute_metrics(res_both, m_set_rmse)

  expect_equal(collected_sum_rmse, computed_sum_rmse)

  collected_unsum_rmse <- collect_metrics(res_rmse, summarize = FALSE)
  computed_unsum_rmse  <- compute_metrics(res_both, m_set_rmse, summarize = FALSE)

  expect_equal(collected_unsum_rmse, computed_unsum_rmse)
})

test_that("errors informatively with bad input", {
  skip_on_cran()
  skip_if_not_installed("kknn")

  library(parsnip)
  library(rsample)
  library(yardstick)

  set.seed(1)

  res_rmse <-
    tune_grid(
      nearest_neighbor("regression", neighbors = tune()),
      mpg ~ cyl + hp,
      bootstraps(mtcars, 5),
      grid = 3,
      metrics = metric_set(rmse)
    )

  res_rmse_save_pred <-
    tune_grid(
      nearest_neighbor("regression", neighbors = tune()),
      mpg ~ cyl + hp,
      bootstraps(mtcars, 5),
      grid = 3,
      metrics = metric_set(rmse),
      control = control_grid(save_pred = TRUE)
    )

  expect_snapshot(
    compute_metrics(res_rmse, metric_set(rsq)),
    error = TRUE
  )

  expect_snapshot(
    compute_metrics("boop", metric_set(rsq)),
    error = TRUE
  )

  expect_snapshot(
    compute_metrics(res_rmse_save_pred, "wheee"),
    error = TRUE
  )
})
