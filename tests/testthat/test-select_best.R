context("`select_best()` and `show_best()`")

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
#         file = testthat::test_path("rcv_results.rds"),
#         version = 2, compress = "xz")

# ------------------------------------------------------------------------------

rcv_results <- readRDS(test_path("rcv_results.rds"))
knn_results <- readRDS(test_path("knn_results.rds"))
source(test_path("../helper-objects.R"))

# ------------------------------------------------------------------------------


test_that("select_best()", {

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
    select_best(rcv_results, metric = "rmse"),
    best_rmse
  )
  expect_equal(
    select_best(rcv_results, metric = "rsq"),
    best_rsq
  )
  expect_warning(
    select_best(rcv_results, metric = "rsq", maximize = TRUE),
    "The `maximize` argument is no longer"
  )

  expect_error(
    select_best(rcv_results, metric = "random"),
    "Please check the value of `metric`"
  )
  expect_error(
    select_best(rcv_results, metric = c("rmse", "rsq")),
    "Please specify a single character"
  )
  expect_warning(
    expect_equal(
      select_best(rcv_results),
      select_best(rcv_results, metric = "rmse")
    ),
    "metric 'rmse' will be used"
  )
})


test_that("show_best()", {

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
  expect_warning(
    expect_equal(
      show_best(rcv_results),
      show_best(rcv_results, metric = "rmse")
    ),
    "metric 'rmse' will be used"
  )
})

test_that("one-std error rule", {

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

  expect_warning(
    select_by_one_std_err(knn_results, metric = "accuracy", K, maximize = TRUE),
    "The `maximize` argument is no longer"
  )

  expect_error(
    select_by_one_std_err(rcv_results, metric = "random", deg_free),
    "Please check the value of `metric`"
  )
  expect_error(
    select_by_one_std_err(rcv_results, metric = c("rmse", "rsq"), deg_free),
    "Please specify a single character"
  )
  expect_warning(
    expect_equal(
      select_by_one_std_err(knn_results, K),
      select_by_one_std_err(knn_results, K, metric = "roc_auc")
    ),
    "metric 'roc_auc' will be used"
  )
  expect_error(
    select_by_one_std_err(rcv_results, metric = "random"),
    "Please choose at least one tuning parameter to sort"
  )
})


test_that("percent loss", {

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

  expect_warning(
    select_by_pct_loss(knn_results, metric = "accuracy", K, maximize = TRUE),
    "The `maximize` argument is no longer"
  )

  expect_error(
    select_by_pct_loss(rcv_results, metric = "random", deg_free),
    "Please check the value of `metric`"
  )
  expect_error(
    select_by_pct_loss(rcv_results, metric = c("rmse", "rsq"), deg_free),
    "Please specify a single character"
  )
  expect_warning(
    expect_equal(
      select_by_pct_loss(knn_results, K),
      select_by_pct_loss(knn_results, K, metric = "roc_auc")
    ),
    "metric 'roc_auc' will be used"
  )
  expect_error(
    select_by_pct_loss(rcv_results, metric = "random"),
    "Please choose at least one tuning parameter to sort"
  )
})


