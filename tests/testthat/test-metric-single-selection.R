
test_that("selecting the first metric", {
  library(yardstick)

  met_1 <- metric_set(rmse)
  tbl_1 <- tibble::as_tibble(met_1)[1,]
  met_2 <- metric_set(rmse, ccc)
  tbl_2 <- tibble::as_tibble(met_2)[1,]

  expect_equal(first_metric(met_1), tbl_1)
  expect_equal(first_metric(met_2), tbl_2)
})

test_that("selecting a metric", {
  # much of this is indirectly tested in show/select best

  data("example_ames_knn")
  expect_snapshot(
    choose_metric(ames_grid_search, "rmse")
  )
  expect_snapshot(
    choose_metric(ames_grid_search, NULL)
  )
  expect_snapshot(
    choose_metric(ames_grid_search, "potato"),
    error = TRUE
  )
  expect_snapshot(
    choose_metric(ames_grid_search, c("rmse", "ccc"))
  )
})

test_that("identify survival metrics", {
  library(yardstick)

  expect_false(
    metric_set(rmse) %>%
      tibble::as_tibble() %>%
      tune:::contains_survival_metric()
  )

  expect_true(
    metric_set(brier_survival_integrated) %>%
      tibble::as_tibble() %>%
      tune:::contains_survival_metric()
  )

  expect_true(
    metric_set(brier_survival, concordance_survival) %>%
      tibble::as_tibble() %>%
      tune:::contains_survival_metric()
  )

})



