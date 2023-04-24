test_that("predict errors informatively when supplied tuning results", {
  skip_on_cran()

  data("example_ames_knn")

  expect_snapshot(predict(ames_grid_search), error = TRUE)
})

test_that("predict errors informatively when supplied last_fit() results", {
  skip_on_cran()

  library(rsample)
  library(parsnip)

  set.seed(6735)
  tr_te_split <- initial_split(mtcars)

  spline_res <- last_fit(linear_reg(), mpg ~ ., split = tr_te_split)

  expect_snapshot(predict(spline_res), error = TRUE)
})
