test_that("pretty tune objects", {
  expect_equal(
    readRDS(test_path("data", "knn_results.rds")) %>% pretty(),
    "10-fold cross-validation repeated 5 times"
  )
  expect_equal(
    ames_grid_search %>% pretty(),
    "10-fold cross-validation using stratification"
  )
  expect_equal(
    ames_iter_search %>% pretty(),
    "10-fold cross-validation using stratification"
  )
})
