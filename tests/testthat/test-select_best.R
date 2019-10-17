context("`select_best` methods")

# ------------------------------------------------------------------------------

load("rcv_results.RData")

# ------------------------------------------------------------------------------


testthat::test_that('select_best method' ,{
  expect_true(tibble::is_tibble(select_best(rcv_results,metric = "rmse")))
  expect_true(tibble::is_tibble(select_best(rcv_results,metric = "rsq",performance = FALSE)))
  expect_error(select_best(rcv_results))
  expect_error(select_best(rcv_results,metric = "random"))
  expect_error(select_best(rcv_results,metric = c("rmse","rsq")))
  expect_error(select_best(rcv_results,metric = "rsq", sort = "some invalid value"))
})
