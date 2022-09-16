# ------------------------------------------------------------------------------
# library(tidymodels)
# set.seed(7898)
# data_folds <- vfold_cv(mtcars, repeats = 2)
#
# base_rec <-
#   recipe(mpg ~ ., data = mtcars) %>%
#   step_normalize(all_predictors())
#
# disp_rec <-
#   base_rec %>%
#   step_bs(disp, degree = tune(), deg_free = tune()) %>%
#   step_bs(wt, degree = tune("wt degree"), deg_free = tune("wt df"))
#
# lm_model <-
#   linear_reg(mode = "regression") %>%
#   set_engine("lm")
#
# cars_wflow <-
#   workflow() %>%
#   add_recipe(disp_rec) %>%
#   add_model(lm_model)
#
# cars_set <-
#   cars_wflow %>%
#   parameters %>%
#   update(degree = degree_int(1:2)) %>%
#   update(deg_free = deg_free(c(2, 10))) %>%
#   update(`wt degree` = degree_int(1:2)) %>%
#   update(`wt df` = deg_free(c(2, 10)))
#
# set.seed(255)
# cars_grid <-
#   cars_set %>%
#   grid_regular(levels = c(3, 2, 3, 2))
#
#
# cars_res <- tune_grid(cars_wflow,
#                       resamples = data_folds,
#                       grid = cars_grid,
#                       control = control_grid(verbose = TRUE, save_pred = TRUE))
# saveRDS(cars_res,
#         file = testthat::test_path("data", "rcv_results.rds"),
#         version = 2, compress = "xz")

# ------------------------------------------------------------------------------

test_that("select_best()", {
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
  expect_snapshot(
    select_best(rcv_results, metric = "rsq", maximize = TRUE)
  )

  expect_snapshot(error = TRUE, {
    select_best(rcv_results, metric = "random")
  })
  expect_snapshot(error = TRUE, {
    select_best(rcv_results, metric = c("rmse", "rsq"))
  })
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
  options(width = 200, pillar.advice = FALSE, pillar.min_title_chars = Inf)

  rcv_results <- readRDS(test_path("data", "rcv_results.rds"))
  knn_results <- readRDS(test_path("data", "knn_results.rds"))

  expect_true(
    tibble::is_tibble(select_by_one_std_err(knn_results, metric = "accuracy", K))
  )

  expect_equal(
    select_by_one_std_err(rcv_results, metric = "rmse", deg_free, `wt degree`)$mean,
    2.94252798698909
  )
  expect_equal(
    select_by_one_std_err(knn_results, metric = "accuracy", K)$K,
    25L
  )

  expect_snapshot(
    select_by_one_std_err(knn_results, metric = "accuracy", K, maximize = TRUE)
  )

  expect_snapshot(error = TRUE, {
    select_by_one_std_err(rcv_results, metric = "random", deg_free)
  })
  expect_snapshot(error = TRUE, {
    select_by_one_std_err(rcv_results, metric = c("rmse", "rsq"), deg_free)
  })
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
})


test_that("percent loss", {
  options(width = 200, pillar.advice = FALSE, pillar.min_title_chars = Inf)

  rcv_results <- readRDS(test_path("data", "rcv_results.rds"))
  knn_results <- readRDS(test_path("data", "knn_results.rds"))

  expect_true(
    tibble::is_tibble(select_by_pct_loss(knn_results, metric = "accuracy", K))
  )
  expect_equal(
    select_by_pct_loss(rcv_results, metric = "rmse", deg_free, `wt degree`)$mean,
    2.94252798698909
  )
  expect_equal(
    select_by_pct_loss(knn_results, metric = "accuracy", K)$K,
    12L
  )

  expect_snapshot(
    select_by_pct_loss(knn_results, metric = "accuracy", K, maximize = TRUE)
  )

  expect_snapshot(error = TRUE, {
    select_by_pct_loss(rcv_results, metric = "random", deg_free)
  })
  expect_snapshot(error = TRUE, {
    select_by_pct_loss(rcv_results, metric = c("rmse", "rsq"), deg_free)
  })
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
})
