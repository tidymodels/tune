test_that("maker static object", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("probably")

  data("two_class_dat", package = "modeldata")
  two_class_rs <- mc_cv(two_class_dat, times = 2)
  mc_cv_args <- rsample::.get_split_args(two_class_rs)

  wflow <- workflow(Class ~ ., dt_spec, cls_est_post)

  res <- tune:::make_static(
    wflow,
    param_info = wflow |> extract_parameter_set_dials(),
    metrics = metric_set(accuracy),
    eval_time = NULL,
    split_args = mc_cv_args,
    control = control_resamples()
  )

  expect_true(is.list(res))
  expect_named(
    res,
    c(
      "wflow",
      "param_info",
      "post_estimation",
      "metrics",
      "metric_info",
      "pred_types",
      "eval_time",
      "split_args",
      "control",
      "data"
    )
  )

  expect_snapshot(
    tune:::make_static(
      1,
      param_info = wflow |> extract_parameter_set_dials(),
      metrics = metric_set(accuracy),
      eval_time = NULL,
      split_args = mc_cv_args,
      control = control_resamples()
    ),
    error = TRUE
  )

  expect_snapshot(
    tune:::make_static(
      wflow,
      param_info = 2,
      metrics = metric_set(accuracy),
      eval_time = NULL,
      split_args = mc_cv_args,
      control = control_resamples()
    ),
    error = TRUE
  )

  expect_snapshot(
    tune:::make_static(
      wflow,
      param_info = wflow |> extract_parameter_set_dials(),
      metrics = 3,
      eval_time = NULL,
      split_args = mc_cv_args,
      control = control_resamples()
    ),
    error = TRUE
  )

  expect_snapshot(
    tune:::make_static(
      wflow,
      param_info = wflow |> extract_parameter_set_dials(),
      metrics = metric_set(accuracy),
      eval_time = "four",
      split_args = mc_cv_args,
      control = control_resamples()
    ),
    error = TRUE
  )

  partitions <- tune:::get_data_subsets(wflow, two_class_rs$splits[[1]])

  res_with_data <- tune:::update_static(res, partitions)
  expect_true(is.list(res_with_data))
  expect_named(
    res_with_data,
    c(
      "wflow",
      "param_info",
      "post_estimation",
      "metrics",
      "metric_info",
      "pred_types",
      "eval_time",
      "split_args",
      "control",
      "data"
    )
  )

  bad_part <- partitions
  bad_part$cal$data <- 1
  expect_snapshot(tune:::update_static(res, bad_part), error = TRUE)

  bad_part <- partitions
  bad_part$cal$ind <- "two"
  expect_snapshot(tune:::update_static(res, bad_part), error = TRUE)
})
