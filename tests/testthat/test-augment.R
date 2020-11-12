context("augment")

library(parsnip)
library(rsample)

data(two_class_dat, package = "modeldata")

lr_spec <- logistic_reg() %>% set_engine("glm")
rf_spec <- rand_forest(trees = 20, min_n = tune()) %>% set_engine("randomForest") %>% set_mode("regression")

set.seed(1)
bt1 <- bootstraps(two_class_dat, times = 30)
set.seed(1)
bt2 <- bootstraps(two_class_dat, times = 3)

set.seed(1)
cv1 <- vfold_cv(mtcars)

# ------------------------------------------------------------------------------

test_that("augment fit_resamples", {
  set.seed(1)
  fit_1 <- fit_resamples(lr_spec, Class ~ ., bt1,
                         control = control_resamples(save_pred = TRUE))
  aug_1 <- augment(fit_1)
  expect_true(nrow(aug_1) == nrow(two_class_dat))
  expect_equal(aug_1[["A"]], two_class_dat[["A"]])
  expect_true(sum(!is.na(aug_1$.pred_class)) == nrow(two_class_dat))
  expect_true(sum(names(aug_1) == ".pred_class")  == 1)
  expect_true(sum(names(aug_1) == ".pred_Class1") == 1)
  expect_true(sum(names(aug_1) == ".pred_Class2") == 1)
  expect_true(sum(names(aug_1) == ".resid") == 0)

  set.seed(1)
  fit_2 <- fit_resamples(lr_spec, Class ~ ., bt2,
                         control = control_resamples(save_pred = TRUE))
  expect_warning(aug_2 <- augment(fit_2), "hold-out predictions")

  expect_true(nrow(aug_2) == nrow(two_class_dat))
  expect_equal(aug_2[["A"]], two_class_dat[["A"]])
  expect_true(sum(!is.na(aug_2$.pred_class)) < nrow(two_class_dat))
  expect_true(sum(names(aug_2) == ".pred_class")  == 1)
  expect_true(sum(names(aug_2) == ".pred_Class1") == 1)
  expect_true(sum(names(aug_2) == ".pred_Class2") == 1)

  expect_error(
    augment(fit_1, hey = "you"),
    "The only argument for"
  )
})


# ------------------------------------------------------------------------------

test_that("augment tune_grid", {
  set.seed(1)
  fit_1 <- tune_grid(rf_spec, mpg ~ ., cv1, grid = data.frame(min_n = 4:6),
                         control = control_grid(save_pred = TRUE))
  aug_1 <- augment(fit_1)
  expect_true(nrow(aug_1) == nrow(mtcars))
  expect_equal(aug_1[["wt"]], mtcars[["wt"]])
  expect_true(sum(!is.na(aug_1$.pred)) == nrow(mtcars))
  expect_true(sum(names(aug_1) == ".pred")  == 1)
  expect_true(sum(names(aug_1) == ".resid")  == 1)

  aug_2 <- augment(fit_1, parameters = data.frame(min_n = 6))
  expect_true(any(abs(aug_1$.pred - aug_2$.pred) > 1))

  expect_error(
    augment(fit_1, parameters = list(min_n = 6)),
    "should be a single row data frame"
  )

  expect_error(
    augment(fit_1, parameters = data.frame(min_n = 5:6)),
    "should be a single row data frame"
  )

  expect_error(
    augment(fit_1, min_n = 6),
    "The only two arguments for"
  )

  # ------------------------------------------------------------------------------

  suppressMessages({
    set.seed(1)
    fit_2 <- tune_bayes(rf_spec, mpg ~ ., cv1, initial = fit_1, iter = 2,
                        param_info = parameters(min_n(c(2, 10))),
                        control = control_bayes(save_pred = TRUE))
  })
  aug_3 <- augment(fit_2)
  expect_true(nrow(aug_3) == nrow(mtcars))
  expect_equal(aug_3[["wt"]], mtcars[["wt"]])
  expect_true(sum(!is.na(aug_3$.pred)) == nrow(mtcars))
  expect_true(sum(names(aug_3) == ".pred")  == 1)
  expect_true(sum(names(aug_3) == ".resid")  == 1)


})


# ------------------------------------------------------------------------------

test_that("augment last_fit", {

  set.seed(1)
  split <- initial_split(two_class_dat)
  fit_1 <- last_fit(lr_spec, Class ~ ., split = split)

  aug_1 <- augment(fit_1)
  expect_true(nrow(aug_1) == nrow(assessment(split)))
  expect_equal(aug_1[["A"]], assessment(split)[["A"]])
  expect_true(sum(!is.na(aug_1$.pred_class)) == nrow(assessment(split)))
  expect_true(sum(names(aug_1) == ".pred_class")  == 1)
  expect_true(sum(names(aug_1) == ".pred_Class1") == 1)
  expect_true(sum(names(aug_1) == ".pred_Class2") == 1)

  expect_error(
    augment(fit_1, potato = TRUE),
    "The only argument for"
  )
})
