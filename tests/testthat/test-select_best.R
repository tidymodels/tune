# ------------------------------------------------------------------------------
# objects for these tests are created using inst/test_objects.R

# ------------------------------------------------------------------------------

test_that("select_best()", {
  skip_if_not_installed("dials", minimum_version = "1.4.0")
  options(width = 200, pillar.advice = FALSE, pillar.min_title_chars = Inf)

  rcv_results <- readRDS(test_path("data", "rcv_results.rds"))

  expect_true(
    tibble::is_tibble(select_best(rcv_results, metric = "rmse"))
  )
  best_rmse <-
    tibble::tribble(
      ~deg_free, ~degree, ~`wt df`, ~`wt degree`,
      6L,        2L,      2L,       1L
    )
  best_rsq <-
    tibble::tribble(
      ~deg_free, ~degree, ~`wt df`, ~`wt degree`,
      10L,       2L,      2L,       2L
    )

  expect_equal(
    select_best(rcv_results, metric = "rmse") %>% select(-.config),
    best_rmse
  )
  expect_equal(
    select_best(rcv_results, metric = "rsq") %>% select(-.config),
    best_rsq
  )

  expect_snapshot(error = TRUE, {
    select_best(rcv_results, metric = "random")
  })
  expect_snapshot(
    select_best(rcv_results, metric = c("rmse", "rsq"))
  )
  expect_snapshot({
    best_default_metric <- select_best(rcv_results)
    best_rmse <- select_best(rcv_results, metric = "rmse")
  })
  expect_equal(best_default_metric, best_rmse)

  expect_snapshot(error = TRUE, {
    select_best(mtcars, metric = "disp")
  })
})


test_that("show_best()", {
  rcv_results <- readRDS(test_path("data", "rcv_results.rds"))

  rcv_rmse <-
    rcv_results %>%
    collect_metrics() %>%
    dplyr::filter(.metric == "rmse") %>%
    dplyr::arrange(mean)

  expect_equal(
    show_best(rcv_results, metric = "rmse", n = 1),
    rcv_rmse %>% slice(1)
  )
  expect_equal(
    show_best(rcv_results, metric = "rmse", n = nrow(rcv_rmse) + 1),
    rcv_rmse
  )
  expect_equal(
    show_best(rcv_results, metric = "rmse", n = 1) %>% names(),
    rcv_rmse %>% names()
  )
  expect_snapshot({
    best_default_metric <- show_best(rcv_results)
    best_rmse <- show_best(rcv_results, metric = "rmse")
  })
  expect_equal(best_default_metric, best_rmse)

  expect_snapshot(error = TRUE, {
    show_best(mtcars, metric = "disp")
  })
})

test_that("one-std error rule", {
  skip_if_not_installed("dials", minimum_version = "1.4.0")
  options(width = 200, pillar.advice = FALSE, pillar.min_title_chars = Inf)

  rcv_results <- readRDS(test_path("data", "rcv_results.rds"))
  knn_results <- readRDS(test_path("data", "knn_results.rds"))

  expect_true(
    tibble::is_tibble(select_by_one_std_err(knn_results, metric = "accuracy", K))
  )

  expect_equal(
    select_by_one_std_err(rcv_results, metric = "rmse", deg_free, `wt degree`)$.config,
    "Preprocessor19_Model1"
  )
  expect_equal(
    select_by_one_std_err(knn_results, metric = "accuracy", K)$K,
    25L
  )

  expect_snapshot(error = TRUE, {
    select_by_one_std_err(rcv_results, metric = "random", deg_free)
  })
  expect_snapshot(
    select_by_one_std_err(rcv_results, metric = c("rmse", "rsq"), deg_free)
  )
  expect_snapshot({
    select_via_default_metric <- select_by_one_std_err(knn_results, K)
    select_via_roc <- select_by_one_std_err(knn_results, K, metric = "roc_auc")
  })
  expect_equal(select_via_default_metric, select_via_roc)

  expect_snapshot(error = TRUE, {
    select_by_one_std_err(rcv_results, metric = "random")
  })

  expect_snapshot(error = TRUE, {
    select_by_one_std_err(mtcars, metric = "disp")
  })
  expect_snapshot(error = TRUE, {
    select_by_one_std_err(knn_results, metric = "roc_auc", weight_funk)
  })
  expect_snapshot(error = TRUE, {
    select_by_one_std_err(knn_results, metric = "roc_auc", weight_funk, K)
  })
  expect_snapshot(error = TRUE, {
    select_by_one_std_err(knn_results, metric = "roc_auc", weight_funk, Kay)
  })
  expect_snapshot(error = TRUE, {
    select_by_one_std_err(knn_results, metric = "roc_auc", weight_funk, desc(K))
  })
})


test_that("percent loss", {
  skip_if_not_installed("dials", minimum_version = "1.4.0")
  options(width = 200, pillar.advice = FALSE, pillar.min_title_chars = Inf)

  rcv_results <- readRDS(test_path("data", "rcv_results.rds"))
  knn_results <- readRDS(test_path("data", "knn_results.rds"))

  expect_true(
    tibble::is_tibble(select_by_pct_loss(knn_results, metric = "accuracy", K))
  )
  expect_equal(
    select_by_pct_loss(rcv_results, metric = "rmse", deg_free, `wt degree`)$.config,
    "Preprocessor19_Model1"
  )
  expect_equal(
    select_by_pct_loss(knn_results, metric = "accuracy", K)$K,
    12L
  )

  expect_snapshot(error = TRUE, {
    select_by_pct_loss(rcv_results, metric = "random", deg_free)
  })
  expect_snapshot(
    select_by_pct_loss(rcv_results, metric = c("rmse", "rsq"), deg_free)
  )
  expect_snapshot({
    select_via_default_metric <- select_by_pct_loss(knn_results, K)
    select_via_roc <- select_by_pct_loss(knn_results, K, metric = "roc_auc")
  })
  expect_equal(select_via_default_metric, select_via_roc)

  expect_snapshot(error = TRUE, {
    select_by_pct_loss(rcv_results, metric = "random")
  })

  expect_snapshot(error = TRUE, {
    select_by_pct_loss(mtcars, metric = "disp")
  })
  expect_snapshot(error = TRUE, {
    select_by_pct_loss(knn_results, metric = "roc_auc", weight_funk)
  })
  expect_snapshot(error = TRUE, {
    select_by_pct_loss(knn_results, metric = "roc_auc", weight_funk, K)
  })
  expect_snapshot(error = TRUE, {
    select_by_pct_loss(knn_results, metric = "roc_auc", weight_funk, Kay)
  })
  expect_snapshot(error = TRUE, {
    select_by_pct_loss(knn_results, metric = "roc_auc", weight_funk, desc(K))
  })

  data("example_ames_knn")
  expect_equal(
    select_by_pct_loss(ames_grid_search, metric = "rmse", limit = 10, desc(K))$K,
    40
  )
})

test_that("select_by_* can handle metrics with direction == 'zero'", {
  skip_on_cran()
  skip_if_not_installed("kknn")

  set.seed(1)
  resamples <- rsample::bootstraps(mtcars, times = 5)

  set.seed(1)
  tune_res <-
    tune::tune_grid(
      parsnip::nearest_neighbor(mode = "regression", neighbors = tune()),
      mpg ~ .,
      resamples,
      metrics = yardstick::metric_set(yardstick::mpe, yardstick::msd)
    )

  tune_res_metrics <- tune_res %>% collect_metrics()

  expect_equal(
    select_best(tune_res, metric = "msd")$.config,
    tune_res_metrics %>%
      filter(.metric == "msd") %>%
      arrange(abs(mean)) %>%
      slice(1) %>%
      select(.config) %>%
      pull()
  )

  expect_equal(
    select_best(tune_res, metric = "mpe")$.config,
    tune_res_metrics %>%
      filter(.metric == "mpe") %>%
      arrange(abs(mean)) %>%
      slice(1) %>%
      select(.config) %>%
      pull()
  )

  expect_equal(
    show_best(tune_res, metric = "msd", n = 5)$.config,
    tune_res_metrics %>%
      filter(.metric == "msd") %>%
      arrange(abs(mean)) %>%
      slice(1:5) %>%
      select(.config) %>%
      pull()
  )

  expect_equal(
    show_best(tune_res, metric = "mpe", n = 5)$.config,
    tune_res_metrics %>%
      filter(.metric == "mpe") %>%
      arrange(abs(mean)) %>%
      slice(1:5) %>%
      select(.config) %>%
      pull()
  )

  # one std error, msd ----------
  best <-
    tune_res_metrics %>%
    filter(.metric == "msd") %>%
    arrange(min(abs(mean))) %>%
    slice(1)

  bound_lower <- -abs(best$mean) - abs(best$std_err)
  bound_upper <- abs(best$mean) + abs(best$std_err)
  expect_equal(bound_lower, -bound_upper)

  simplest_within_bound <-
    tune_res_metrics %>%
    filter(.metric == "msd") %>%
    filter(abs(mean) < bound_upper) %>%
    arrange(desc(neighbors)) %>%
    slice(1)

  expect_equal(
    select_by_one_std_err(tune_res, metric = "msd", desc(neighbors))$.config,
    simplest_within_bound$.config
  )

  # one std error, mpe ----------
  best <-
    tune_res_metrics %>%
    filter(.metric == "mpe") %>%
    arrange(min(abs(mean))) %>%
    slice(1)

  bound_lower <- -abs(best$mean) - abs(best$std_err)
  bound_upper <- abs(best$mean) + abs(best$std_err)
  expect_equal(bound_lower, -bound_upper)

  simplest_within_bound <-
    tune_res_metrics %>%
    filter(.metric == "mpe") %>%
    filter(abs(mean) < bound_upper) %>%
    arrange(desc(neighbors)) %>%
    slice(1)

  expect_equal(
    select_by_one_std_err(tune_res, metric = "mpe", desc(neighbors))$.config,
    simplest_within_bound$.config
  )

  # pct loss, msd ----------
  best <-
    tune_res_metrics %>%
    filter(.metric == "msd") %>%
    arrange(abs(mean)) %>%
    slice(1)

  expect_equal(
    select_by_pct_loss(tune_res, metric = "msd", limit = 10, desc(neighbors))$.config,
    tune_res_metrics %>%
      filter(.metric == "msd") %>%
      dplyr::rowwise() %>%
      mutate(loss = abs((abs(mean) - abs(best$mean)) / best$mean) * 100) %>%
      ungroup() %>%
      arrange(desc(neighbors)) %>%
      slice(1:which(.config == best$.config)) %>%
      filter(loss < 10) %>%
      slice(1) %>%
      select(.config) %>%
      pull()
  )

  # pct loss, mpe ----------
  best <-
    tune_res_metrics %>%
    filter(.metric == "mpe") %>%
    arrange(abs(mean)) %>%
    slice(1)

  expect_equal(
    select_by_pct_loss(tune_res, metric = "mpe", limit = 10, desc(neighbors))$.config,
    tune_res_metrics %>%
      filter(.metric == "mpe") %>%
      dplyr::rowwise() %>%
      mutate(loss = abs((abs(mean) - abs(best$mean)) / best$mean) * 100) %>%
      ungroup() %>%
      arrange(desc(neighbors)) %>%
      slice(1:which(.config == best$.config)) %>%
      filter(loss < 10) %>%
      slice(1) %>%
      select(.config) %>%
      pull()
  )
})

test_that("show_best with survival models", {
  surv_res <- readRDS(test_path("data", "surv_boost_tree_res.rds"))

  expect_snapshot(
    show_best(surv_res)
  )
  expect_snapshot(
    show_best(surv_res, metric = "concordance_survival")
  )
  expect_snapshot(
    show_best(surv_res, metric = "brier_survival_integrated")
  )
  expect_snapshot(
    show_best(surv_res, metric = "brier_survival")
  )
  expect_snapshot(
    show_best(surv_res, metric = c("brier_survival", "roc_auc_survival"))
  )
  expect_snapshot(
    show_best(surv_res, metric = "brier_survival", eval_time = 1)
  )
  expect_snapshot(
    show_best(surv_res, metric = "concordance_survival", eval_time = 1)
  )
  expect_snapshot(
    show_best(surv_res, metric = "concordance_survival", eval_time = 1.1)
  )
  expect_snapshot(
    show_best(surv_res, metric = "brier_survival", eval_time = 1.1),
    error = TRUE
  )
  expect_snapshot(
    show_best(surv_res, metric = "brier_survival", eval_time = 1:2)
  )
  expect_snapshot(
    show_best(surv_res, metric = "brier_survival", eval_time = 3:4),
    error = TRUE
  )

})

test_that("select_best with survival models", {
  surv_res <- readRDS(test_path("data", "surv_boost_tree_res.rds"))

  expect_snapshot(
    select_best(surv_res)
  )
  expect_snapshot(
    select_best(surv_res, metric = "concordance_survival")
  )
  expect_snapshot(
    select_best(surv_res, metric = "brier_survival_integrated")
  )
  expect_snapshot(
    select_best(surv_res, metric = "brier_survival")
  )
  expect_snapshot(
    select_best(surv_res, metric = c("brier_survival", "roc_auc_survival"))
  )
  expect_snapshot(
    select_best(surv_res, metric = "brier_survival", eval_time = 1)
  )
  expect_snapshot(
    select_best(surv_res, metric = "concordance_survival", eval_time = 1)
  )
  expect_snapshot(
    select_best(surv_res, metric = "concordance_survival", eval_time = 1.1)
  )
  expect_snapshot(
    select_best(surv_res, metric = "brier_survival", eval_time = 1.1),
    error = TRUE
  )
  expect_snapshot(
    select_best(surv_res, metric = "brier_survival", eval_time = 1:2)
  )
  expect_snapshot(
    select_best(surv_res, metric = "brier_survival", eval_time = 3:4),
    error = TRUE
  )

})

test_that("select_by_one_std_err with survival models", {
  surv_res <- readRDS(test_path("data", "surv_boost_tree_res.rds"))

  expect_snapshot(
    select_by_one_std_err(surv_res, trees)
  )
  expect_snapshot(
    select_by_one_std_err(surv_res, metric = "concordance_survival", trees)
  )
  expect_snapshot(
    select_by_one_std_err(surv_res, metric = "brier_survival_integrated", trees)
  )
  expect_snapshot(
    select_by_one_std_err(surv_res, metric = "brier_survival", trees)
  )
  expect_snapshot(
    select_by_one_std_err(surv_res, metric = c("brier_survival", "roc_auc_survival"),
                          trees)
  )
  expect_snapshot(
    select_by_one_std_err(surv_res, metric = "brier_survival",
                          eval_time = 1, trees)
  )
  expect_snapshot(
    select_by_one_std_err(surv_res, metric = "concordance_survival",
                          eval_time = 1, trees)
  )
  expect_snapshot(
    select_by_one_std_err(surv_res, metric = "concordance_survival",
                          eval_time = 1.1, trees)
  )
  expect_snapshot(
    select_by_one_std_err(surv_res, metric = "brier_survival",
                          eval_time = 1.1, trees),
    error = TRUE
  )
  expect_snapshot(
    select_by_one_std_err(surv_res, metric = "brier_survival",
                          eval_time = 1:2, trees)
  )
  expect_snapshot(
    select_by_one_std_err(surv_res, metric = "brier_survival",
                          eval_time = 3:4, trees),
    error = TRUE
  )

})

test_that("select_by_pct_loss with survival models", {
  surv_res <- readRDS(test_path("data", "surv_boost_tree_res.rds"))

  expect_snapshot(
    select_by_pct_loss(surv_res, trees)
  )
  expect_snapshot(
    select_by_pct_loss(surv_res, metric = "concordance_survival", trees)
  )
  expect_snapshot(
    select_by_pct_loss(surv_res, metric = "brier_survival_integrated", trees)
  )
  expect_snapshot(
    select_by_pct_loss(surv_res, metric = "brier_survival", trees)
  )
  expect_snapshot(
    select_by_pct_loss(surv_res, metric = c("brier_survival", "roc_auc_survival"),
                       trees)
  )
  expect_snapshot(
    select_by_pct_loss(surv_res, metric = "brier_survival",
                       eval_time = 1, trees)
  )
  expect_snapshot(
    select_by_pct_loss(surv_res, metric = "concordance_survival",
                       eval_time = 1, trees)
  )
  expect_snapshot(
    select_by_pct_loss(surv_res, metric = "concordance_survival",
                       eval_time = 1.1, trees)
  )
  expect_snapshot(
    select_by_pct_loss(surv_res, metric = "brier_survival",
                       eval_time = 1.1, trees),
    error = TRUE
  )
  expect_snapshot(
    select_by_pct_loss(surv_res, metric = "brier_survival",
                       eval_time = 1:2, trees)
  )
  expect_snapshot(
    select_by_pct_loss(surv_res, metric = "brier_survival",
                       eval_time = 3:4, trees),
    error = TRUE
  )

})



