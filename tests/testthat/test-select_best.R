context("`select_best` methods")

# ------------------------------------------------------------------------------

load("rcv_results.RData")

# ------------------------------------------------------------------------------


testthat::test_that("select_best method", {
  expect_true(tibble::is_tibble(select_best(rcv_results, metric = "rmse")))
  expect_true(tibble::is_tibble(select_best(rcv_results, metric = "rsq", performance = FALSE)))
  expect_error(select_best(rcv_results)) # when no metric is specified
  expect_error(select_best(rcv_results, metric = "random")) # when metric doesn't exist
  expect_error(select_best(rcv_results, metric = c("rmse", "rsq"))) # when more than 1 metric is specified
  expect_error(select_best(rcv_results, metric = "rsq", maximize = "some invalid value")) # only asc or desc is acceptable as a value
})
