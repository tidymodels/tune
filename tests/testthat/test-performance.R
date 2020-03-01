context("object extraction")

# ------------------------------------------------------------------------------

source(test_path("../helper-objects.R"))

# ------------------------------------------------------------------------------

test_that('prediction types', {

  expect_equal(tune:::pred_type(accuracy), "class")
  expect_equal(tune:::pred_type(roc_auc), "prob")
  expect_equal(tune:::pred_type(rsq), "numeric")
  expect_equal(tune:::pred_type(metric_set(accuracy, roc_auc)), "unknown")

})

test_that("metric information", {
  set_1 <- tune:::metrics_info(metric_set(accuracy, kap, roc_auc))
  set_2 <- tune:::metrics_info(metric_set(rmse, rsq))
  expect_equal(
    set_1,
    tibble::tibble(
      .metric = c("accuracy", "kap", "roc_auc"),
      direction = rep("maximize", 3),
      type = c("class", "class", "prob")
      )
    )
  expect_equal(
    set_2,
    tibble::tibble(
      .metric = c("rmse", "rsq"),
      direction = c("minimize", "maximize"),
      type = c("numeric", "numeric")
    )
  )
})


