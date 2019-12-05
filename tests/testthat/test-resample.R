context("resample")

library(parsnip)
library(rsample)

# ------------------------------------------------------------------------------
# fit_resamples()

test_that("`fit_resamples()` returns a `resample_result` object", {
  set.seed(6735)
  folds <- vfold_cv(mtcars, v = 2)

  lin_mod <- linear_reg() %>%
    set_engine("lm")

  result <- fit_resamples(mpg ~ ., lin_mod, folds)

  expect_is(result, "resample_results")
})

test_that("can use `fit_resamples()` with a formula", {
  set.seed(6735)
  folds <- vfold_cv(mtcars, v = 2)

  lin_mod <- linear_reg() %>%
    set_engine("lm")

  expect_warning(
    result <- fit_resamples(mpg ~ ., lin_mod, folds),
    NA
  )

  expect_equal(nrow(result$.metrics[[1]]), 2L)
})

test_that("can use `fit_resamples()` with a recipe", {
  set.seed(6735)
  folds <- vfold_cv(mtcars, v = 2)

  rec <- recipe(mpg ~ ., data = mtcars) %>%
    step_ns(disp) %>%
    step_ns(wt)

  lin_mod <- linear_reg() %>%
    set_engine("lm")

  # Ensure the recipes are prepped and returned
  control <- control_resamples(extract = function(x) x)

  result <- fit_resamples(rec, lin_mod, folds, control = control)

  prepped_rec <- extract_recipe(result$.extracts[[1]][[1]][[1]])

  expect_true(prepped_rec$steps[[1]]$trained)
  expect_true(prepped_rec$steps[[2]]$trained)

  expect_equal(nrow(result$.metrics[[1]]), 2L)
})

test_that("can use `fit_resamples()` with a workflow", {
  set.seed(6735)
  folds <- vfold_cv(mtcars, v = 2)

  rec <- recipe(mpg ~ ., data = mtcars) %>%
    step_ns(disp) %>%
    step_ns(wt)

  lin_mod <- linear_reg() %>%
    set_engine("lm")

  workflow <- workflow() %>%
    add_recipe(rec) %>%
    add_model(lin_mod)

  expect <- fit_resamples(rec, lin_mod, folds)

  result <- fit_resamples(workflow, folds)

  expect_equal(as.data.frame(expect), as.data.frame(result))
})

# ------------------------------------------------------------------------------
# Error capture

test_that("failure in recipe is caught elegantly", {
  set.seed(6735)
  folds <- vfold_cv(mtcars, v = 2)

  rec <- recipe(mpg ~ ., data = mtcars) %>%
    step_ns(disp, deg_free = NA_real_)

  lin_mod <- linear_reg() %>%
    set_engine("lm")

  control <- control_resamples(extract = function(x) x, save_pred = TRUE)

  expect_warning(
    result <- fit_resamples(rec, lin_mod, folds, control = control),
    "All models failed"
  )

  notes <- result$.notes
  note <- notes[[1]]$.notes

  extract <- result$.extracts
  predictions <- result$.predictions

  expect_length(notes, 2L)

  # Known failure in the recipe
  expect_true(any(grepl("recipe", note)))

  expect_equal(extract, list(NULL, NULL))
  expect_equal(predictions, list(NULL, NULL))
})

# ------------------------------------------------------------------------------
# tune_grid() fallback

test_that("`tune_grid()` falls back to resamples if there are no tuning parameters", {
  set.seed(6735)
  folds <- vfold_cv(mtcars, v = 2)

  lin_mod <- linear_reg() %>%
    set_engine("lm")

  expect <- fit_resamples(mpg ~ ., lin_mod, folds)

  expect_warning(
    result <- tune_grid(mpg ~ ., lin_mod, folds),
    "No tuning parameters have been detected"
  )

  expect_equal(as.data.frame(result), as.data.frame(expect))
})

test_that("`tune_grid()` ignores `grid` if there are no tuning parameters", {
  set.seed(6735)
  folds <- vfold_cv(mtcars, v = 2)

  lin_mod <- linear_reg() %>%
    set_engine("lm")

  expect <- fit_resamples(mpg ~ ., lin_mod, folds)

  expect_warning(
    result <- tune_grid(mpg ~ ., lin_mod, grid = data.frame(x = 1), folds),
    "No tuning parameters have been detected"
  )

  expect_equal(as.data.frame(result), as.data.frame(expect))
})

# ------------------------------------------------------------------------------
# autoplot()

test_that("cannot autoplot `fit_resamples()` results", {
  set.seed(6735)
  folds <- vfold_cv(mtcars, v = 2)

  lin_mod <- linear_reg() %>%
    set_engine("lm")

  result <- fit_resamples(mpg ~ ., lin_mod, folds)

  expect_error(autoplot(result), "no `autoplot[(][])]` implementation for `resample_results`")
})
