context("`select_best()` and `show_best()`")

# ------------------------------------------------------------------------------

load("rcv_results.RData")
library(tibble)

# ------------------------------------------------------------------------------


testthat::test_that("select_best()", {
  expect_true(
    is_tibble(select_best(rcv_results, metric = "rmse", maximize = FALSE))
    )
  expect_warning(
    expect_true(
      is_tibble(select_best(rcv_results, metric = "rmse"))
    ),
    "Did you mean to maximize rmse?"
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
    select_best(rcv_results, metric = "rmse", maximize = FALSE),
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
  expect_error(
    select_best(rcv_results, metric = "rsq", maximize = "yes"),
    "Please specify a single logical value for `maximize`"
  )

})


testthat::test_that("show_best()", {
  rcv_rmse <-
    rcv_results %>%
    estimate() %>%
    dplyr::filter(.metric == "rmse") %>%
    dplyr::arrange(mean)

  expect_equal(
    show_best(rcv_results, metric = "rmse", n_top = 1, maximize = FALSE),
    rcv_rmse %>% slice(1)
  )
  expect_equal(
    show_best(rcv_results, metric = "rmse", n_top = nrow(rcv_rmse) + 1, maximize = FALSE),
    rcv_rmse
  )
  expect_equal(
    show_best(rcv_results, metric = "rmse", n_top = 1, maximize = FALSE) %>% names(),
    rcv_rmse %>% names()
  )
})
