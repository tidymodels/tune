
test_that("augment fit_resamples", {
  skip_if_not_installed("modeldata")
  data(two_class_dat, package = "modeldata")

  # ------------------------------------------------------------------------------

  lr_spec <- parsnip::logistic_reg() %>% parsnip::set_engine("glm")

  set.seed(1)
  two_class_dat <- as.data.frame(two_class_dat)
  bt1 <- rsample::bootstraps(two_class_dat, times = 30)

  set.seed(1)
  fit_1 <- fit_resamples(
    lr_spec,
    Class ~ .,
    bt1,
    control = control_resamples(save_pred = TRUE)
  )
  aug_1 <- augment(fit_1)
  expect_true(nrow(aug_1) == nrow(two_class_dat))
  expect_equal(aug_1[["A"]], two_class_dat[["A"]])
  expect_true(sum(!is.na(aug_1$.pred_class)) == nrow(two_class_dat))
  expect_true(sum(names(aug_1) == ".pred_class") == 1)
  expect_true(sum(names(aug_1) == ".pred_Class1") == 1)
  expect_true(sum(names(aug_1) == ".pred_Class2") == 1)
  expect_true(sum(names(aug_1) == ".resid") == 0)
  expect_s3_class_bare_tibble(aug_1)

  expect_true(names(aug_1)[1] == ".pred_class")
  expect_true(names(aug_1)[2] == ".pred_Class1")
  expect_true(names(aug_1)[3] == ".pred_Class2")

  expect_identical(
    rle(grepl("^\\.", names(aug_1)))$values,
    c(TRUE, FALSE)
  )

  expect_snapshot(error = TRUE, augment(fit_1, hey = "you"))
})


test_that("augment fit_resamples", {
  skip_if_not_installed("modeldata")
  data(two_class_dat, package = "modeldata")

  # ------------------------------------------------------------------------------

  skip_if(new_rng_snapshots)
  lr_spec <- parsnip::logistic_reg() %>% parsnip::set_engine("glm")

  set.seed(1)
  two_class_dat <- as.data.frame(two_class_dat)
  bt2 <- rsample::bootstraps(two_class_dat, times = 3)

  set.seed(1)
  fit_2 <- fit_resamples(
    lr_spec,
    Class ~ .,
    bt2,
    control = control_resamples(save_pred = TRUE)
  )
  expect_snapshot(aug_2 <- augment(fit_2))

  expect_true(nrow(aug_2) == nrow(two_class_dat))
  expect_equal(aug_2[["A"]], two_class_dat[["A"]])
  expect_true(sum(!is.na(aug_2$.pred_class)) < nrow(two_class_dat))
  expect_true(sum(names(aug_2) == ".pred_class") == 1)
  expect_true(sum(names(aug_2) == ".pred_Class1") == 1)
  expect_true(sum(names(aug_2) == ".pred_Class2") == 1)
  expect_s3_class_bare_tibble(aug_2)

  expect_true(names(aug_2)[1] == ".pred_class")
  expect_true(names(aug_2)[2] == ".pred_Class1")
  expect_true(names(aug_2)[3] == ".pred_Class2")

  expect_identical(
    rle(grepl("^\\.", names(aug_2)))$values,
    c(TRUE, FALSE)
  )
})

# ------------------------------------------------------------------------------

test_that("augment tune_grid", {
  skip_if_not_installed("kernlab")

  svm_spec <- parsnip::svm_linear(cost = tune(), margin = 0.1) %>%
    parsnip::set_engine("kernlab") %>%
    parsnip::set_mode("regression")
  set.seed(1)
  cv1 <- rsample::vfold_cv(mtcars)

  set.seed(1)
  fit_1 <- tune_grid(
    svm_spec,
    mpg ~ .,
    cv1,
    grid = data.frame(cost = 1:3),
    control = control_grid(save_pred = TRUE)
  )
  aug_1 <- augment(fit_1)
  expect_true(nrow(aug_1) == nrow(mtcars))
  expect_equal(aug_1[["wt"]], mtcars[["wt"]])
  expect_true(sum(!is.na(aug_1$.pred)) == nrow(mtcars))
  expect_true(sum(names(aug_1) == ".pred") == 1)
  expect_true(sum(names(aug_1) == ".resid") == 1)
  expect_s3_class_bare_tibble(aug_1)

  expect_true(names(aug_1)[1] == ".pred")

  expect_identical(
    rle(grepl("^\\.", names(aug_1)))$values,
    c(TRUE, FALSE)
  )

  aug_2 <- augment(fit_1, parameters = data.frame(cost = 3))
  expect_true(any(abs(aug_1$.pred - aug_2$.pred) > 1))

  expect_snapshot(error = TRUE, {
    augment(fit_1, parameters = list(cost = 3))
  })

  expect_snapshot(error = TRUE, {
    augment(fit_1, parameters = data.frame(cost = 3:4))
  })

  expect_snapshot(error = TRUE, {
    augment(fit_1, cost = 4)
  })

  # ------------------------------------------------------------------------------

  suppressMessages({
    set.seed(1)
    fit_2 <- tune_bayes(
      svm_spec,
      mpg ~ .,
      cv1,
      initial = fit_1,
      iter = 2,
      param_info = parameters(dials::cost(c(-10, 5))),
      control = control_bayes(save_pred = TRUE)
    )
  })
  aug_3 <- augment(fit_2)
  expect_true(nrow(aug_3) == nrow(mtcars))
  expect_equal(aug_3[["wt"]], mtcars[["wt"]])
  expect_true(sum(!is.na(aug_3$.pred)) == nrow(mtcars))
  expect_true(sum(names(aug_3) == ".pred") == 1)
  expect_true(sum(names(aug_3) == ".resid") == 1)
  expect_s3_class_bare_tibble(aug_3)

  expect_true(names(aug_3)[1] == ".pred")
})


# ------------------------------------------------------------------------------

test_that("augment last_fit", {
  skip_if_not_installed("modeldata")
  data(two_class_dat, package = "modeldata")

  # ------------------------------------------------------------------------------

  lr_spec <- parsnip::logistic_reg() %>% parsnip::set_engine("glm")
  set.seed(1)
  split <- rsample::initial_split(two_class_dat)
  fit_1 <- last_fit(lr_spec, Class ~ ., split = split)

  aug_1 <- augment(fit_1)
  expect_true(nrow(aug_1) == nrow(rsample::assessment(split)))
  expect_equal(aug_1[["A"]], rsample::assessment(split)[["A"]])
  expect_true(sum(!is.na(aug_1$.pred_class)) == nrow(rsample::assessment(split)))
  expect_true(sum(names(aug_1) == ".pred_class") == 1)
  expect_true(sum(names(aug_1) == ".pred_Class1") == 1)
  expect_true(sum(names(aug_1) == ".pred_Class2") == 1)
  expect_s3_class_bare_tibble(aug_1)

  expect_true(names(aug_1)[1] == ".pred_class")
  expect_true(names(aug_1)[2] == ".pred_Class1")
  expect_true(names(aug_1)[3] == ".pred_Class2")

  expect_identical(
    rle(grepl("^\\.", names(aug_1)))$values,
    c(TRUE, FALSE)
  )

  expect_snapshot(error = TRUE, augment(fit_1, potato = TRUE))
})
