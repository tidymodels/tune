context("`select_best()` and `show_best()`")

# ------------------------------------------------------------------------------

load(test_path("rcv_results.RData"))
load(test_path("knn_results.RData"))
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

  expect_error(
    select_best(rcv_results, metric = "random"),
    "Please check the value of `metric`"
    )
  expect_error(
    select_best(rcv_results, metric = c("rmse", "rsq")),
    "Please specify a single character"
  )
  expect_error(
    select_best(rcv_results),
    'argument "metric" is missing, with no default'
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

  expect_error(
    select_by_one_std_err(rcv_results, metric = "random", deg_free),
    "Please check the value of `metric`"
  )
  expect_error(
    select_by_one_std_err(rcv_results, metric = c("rmse", "rsq"), deg_free),
    "Please specify a single character"
  )
  expect_error(
    select_by_one_std_err(rcv_results, deg_free),
    'argument "metric" is missing, with no default'
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

  expect_error(
    select_by_pct_loss(rcv_results, metric = "random", deg_free),
    "Please check the value of `metric`"
  )
  expect_error(
    select_by_pct_loss(rcv_results, metric = c("rmse", "rsq"), deg_free),
    "Please specify a single character"
  )
  expect_error(
    select_by_pct_loss(rcv_results, deg_free),
    'argument "metric" is missing, with no default'
  )
  expect_error(
    select_by_pct_loss(rcv_results, metric = "random"),
    "Please choose at least one tuning parameter to sort"
  )
})


