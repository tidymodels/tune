test_that("prediction types", {
  expect_equal(tune:::pred_type(accuracy), "class")
  expect_equal(tune:::pred_type(roc_auc), "prob")
  expect_equal(tune:::pred_type(rsq), "numeric")
  expect_equal(
    tune:::pred_type(yardstick::metric_set(yardstick::accuracy, yardstick::roc_auc)),
    "unknown"
  )
})

test_that("metric information", {
  set_1 <- tune:::metrics_info(
    yardstick::metric_set(yardstick::accuracy, yardstick::kap, yardstick::roc_auc)
  )
  set_2 <- tune:::metrics_info(
    yardstick::metric_set(yardstick::rmse, yardstick::rsq)
  )
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
