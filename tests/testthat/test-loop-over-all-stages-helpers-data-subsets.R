test_that("extract data subsets - no postprocessing", {
  skip_if_not_installed("modeldata")

  data("two_class_dat", package = "modeldata")
  two_class_rs <- mc_cv(two_class_dat, times = 2)
  rs_split <- two_class_rs$splits[[1]]

  wflow_1 <- workflow(Class ~ ., dt_spec)
  data_1 <- tune:::get_data_subsets(wflow_1, rs_split)
  expect_named(data_1, c("fit", "pred", "cal"))
  expect_equal(data_1$fit$data, analysis(rs_split))
  expect_equal(data_1$pred$data, assessment(rs_split))
  expect_equal(data_1$fit$ind, as.integer(rs_split))
  expect_equal(data_1$pred$ind, as.integer(rs_split, data = "assessment"))
})

test_that("extract data subsets - no estimated postprocessing", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("probably")

  data("two_class_dat", package = "modeldata")
  two_class_rs <- mc_cv(two_class_dat, times = 2)
  rs_split <- two_class_rs$splits[[1]]

  wflow_1 <- workflow(Class ~ ., dt_spec, cls_post)
  data_1 <- tune:::get_data_subsets(wflow_1, rs_split)
  expect_named(data_1, c("fit", "pred", "cal"))
  expect_equal(data_1$fit$data, analysis(rs_split))
  expect_equal(data_1$pred$data, assessment(rs_split))
  expect_equal(data_1$fit$ind, as.integer(rs_split))
  expect_equal(data_1$pred$ind, as.integer(rs_split, data = "assessment"))
})

test_that("extract data subsets - estimated postprocessing", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("probably")

  data("two_class_dat", package = "modeldata")
  two_class_rs <- mc_cv(two_class_dat, times = 2)
  mc_cv_args <- rsample::.get_split_args(two_class_rs)

  rs_split <- two_class_rs$splits[[1]]

  set.seed(1)
  rs_sub_split <- rsample::inner_split(rs_split, mc_cv_args)

  wflow_1 <- workflow(Class ~ ., dt_spec, cls_est_post)

  set.seed(1)
  data_1 <- tune:::get_data_subsets(wflow_1, rs_split, mc_cv_args)

  expect_named(
    data_1,
    c("fit", "pred", "cal")
  )
  expect_equal(data_1$fit$data, analysis(rs_sub_split))
  expect_equal(data_1$cal$data, assessment(rs_sub_split))
  expect_equal(data_1$pred$data, assessment(rs_split))

  expect_equal(data_1$fit$ind, as.integer(rs_sub_split))
  expect_equal(data_1$cal$ind, as.integer(rs_sub_split, data = "assessment"))
  expect_equal(data_1$pred$ind, as.integer(rs_split, data = "assessment"))
})
