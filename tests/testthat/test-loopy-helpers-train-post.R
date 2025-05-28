test_that("tailor trains calibrator", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("probably")

  cls <- make_post_data()

  wflow <- workflow(class ~ ., logistic_reg())

  wflow_cal <- workflow(class ~ ., logistic_reg(), cls_est_post)
  wflow_fit <- .fit_pre(wflow_cal, cls$data) |>
    .fit_model(control = control_workflow())

  predictions <- augment(wflow_fit$fit$fit, cls$data)
  res <- tune:::finalize_fit_post(wflow_fit, predictions, grid = tibble())
  expect_s3_class(res, "tailor")
  expect_true(res$adjustments[[1]]$trained)
  expect_equal(
    res$adjustments[[1]]$results$fit$estimates[[1]]$estimate$df.null,
    nrow(cls$data) - 1
  )
})

test_that("tailor updated with grid and fit", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("probably")
  skip_if_not_installed("mgcv")

  cls <- make_post_data()

  wflow <- workflow(class ~ ., logistic_reg())

  cal_and_cut <- tailor::tailor() |>
    tailor::adjust_probability_calibration() |>
    tailor::adjust_probability_threshold(threshold = tune("cut"))

  wflow_cal <- workflow(class ~ ., logistic_reg(), cal_and_cut)
  wflow_fit <- .fit_pre(wflow_cal, cls$data) |>
    .fit_model(control = control_workflow())

  predictions <- augment(wflow_fit$fit$fit, cls$data)
  res <- tune:::finalize_fit_post(
    wflow_fit,
    predictions,
    grid = tibble(cut = 0)
  )
  re_predicted <- predict(res, predictions)

  expect_s3_class(res, "tailor")
  expect_true(res$adjustments[[1]]$trained)
  expect_equal(
    res$adjustments[[1]]$results$fit$estimates[[1]]$estimate$df.null,
    nrow(cls$data) - 1
  )
  expect_true(all(re_predicted$.pred_class == "class_1"))
})
