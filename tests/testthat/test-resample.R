context("resample")

source(test_path("../helper-objects.R"))

# ------------------------------------------------------------------------------
# fit_resamples()

test_that("`fit_resamples()` returns a `resample_result` object", {
  set.seed(6735)
  folds <- vfold_cv(mtcars, v = 2)

  lin_mod <- linear_reg() %>%
    set_engine("lm")

  result <- lin_mod %>%
    fit_resamples(mpg ~ ., folds)

  expect_is(result, "resample_results")
})

test_that("can use `fit_resamples()` with a formula", {
  set.seed(6735)
  folds <- vfold_cv(mtcars, v = 2)

  lin_mod <- linear_reg() %>%
    set_engine("lm")

  expect_warning(
    result <- lin_mod %>%
      fit_resamples(mpg ~ ., folds),
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

  result <- fit_resamples(lin_mod, rec, folds, control = control)

  prepped_rec <- extract_recipe(result$.extracts[[1]][[1]][[1]])

  expect_true(prepped_rec$steps[[1]]$trained)
  expect_true(prepped_rec$steps[[2]]$trained)

  expect_equal(nrow(result$.metrics[[1]]), 2L)
})

test_that("can use `fit_resamples()` with a workflow - recipe", {
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

  expect <- fit_resamples(lin_mod, rec, folds)

  result <- fit_resamples(workflow, folds)

  expect_equal(collect_metrics(expect), collect_metrics(result))
})

test_that("can use `fit_resamples()` with a workflow - variables", {
  set.seed(6735)
  folds <- vfold_cv(mtcars, v = 2)

  lin_mod <- linear_reg() %>%
    set_engine("lm")

  workflow <- workflow() %>%
    add_variables(mpg, c(cyl, disp)) %>%
    add_model(lin_mod)

  expect <- fit_resamples(lin_mod, mpg ~ cyl + disp, folds)

  result <- fit_resamples(workflow, folds)

  expect_equal(collect_metrics(expect), collect_metrics(result))
})

test_that("can use `fit_resamples()` with a workflow - formula", {
  set.seed(6735)
  folds <- vfold_cv(mtcars, v = 2)

  lin_mod <- linear_reg() %>%
    set_engine("lm")

  workflow <- workflow() %>%
    add_formula(mpg ~ cyl + disp) %>%
    add_model(lin_mod)

  expect <- fit_resamples(lin_mod, mpg ~ cyl + disp, folds)

  result <- fit_resamples(workflow, folds)

  expect_equal(collect_metrics(expect), collect_metrics(result))
})

test_that("extracted workflow is finalized", {
  set.seed(6735)
  folds <- vfold_cv(mtcars, v = 2)

  lin_mod <- linear_reg() %>%
    set_engine("lm")

  workflow <- workflow() %>%
    add_variables(mpg, c(cyl, disp)) %>%
    add_model(lin_mod)

  control <- control_resamples(extract = identity)

  result <- fit_resamples(workflow, folds, control = control)
  result_workflow <- result$.extracts[[1]]$.extracts[[1]]

  expect_true(result_workflow$trained)
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
    result <- fit_resamples(lin_mod, rec, folds, control = control),
    "All models failed"
  )

  notes <- result$.notes
  note <- notes[[1]]$.notes

  extract <- result$.extracts
  predictions <- result$.predictions

  expect_length(notes, 2L)

  # Known failure in the recipe
  expect_true(any(grepl("preprocessor", note)))

  expect_equivalent(extract, list(NULL, NULL))
  expect_equivalent(predictions, list(NULL, NULL))
})

test_that("failure in variables tidyselect specification is caught elegantly", {
  set.seed(6735)
  folds <- vfold_cv(mtcars, v = 2)

  lin_mod <- linear_reg() %>%
    set_engine("lm")

  workflow <- workflow() %>%
    add_model(lin_mod) %>%
    add_variables(mpg, foobar)

  control <- control_resamples(extract = function(x) x, save_pred = TRUE)

  expect_warning(
    result <- fit_resamples(workflow, folds, control = control),
    "All models failed"
  )

  notes <- result$.notes
  note <- notes[[1]]$.notes

  extract <- result$.extracts
  predictions <- result$.predictions

  expect_length(notes, 2L)

  # Known failure in the variables part
  expect_true(any(grepl("preprocessor", note)))

  expect_equivalent(extract, list(NULL, NULL))
  expect_equivalent(predictions, list(NULL, NULL))
})

test_that("classification models generate correct error message", {
  set.seed(6735)
  folds <- vfold_cv(mtcars, v = 2)

  rec <- recipe(vs ~ ., data = mtcars)

  log_mod <- logistic_reg() %>%
    set_engine("glm")

  control <- control_resamples(extract = function(x) x, save_pred = TRUE)

  expect_warning(
    result <- fit_resamples(log_mod, rec, folds, control = control),
    "All models failed"
  )

  notes <- result$.notes
  note <- notes[[1]]$.notes

  extract <- result$.extracts
  predictions <- result$.predictions

  expect_length(notes, 2L)

  # Known failure in the recipe
  expect_true(all(grepl("outcome should be a factor", note)))

  expect_equivalent(extract, list(NULL, NULL))
  expect_equivalent(predictions, list(NULL, NULL))
})


# ------------------------------------------------------------------------------
# tune_grid() fallback

test_that("`tune_grid()` falls back to `fit_resamples()` - formula", {
  set.seed(6735)
  folds <- vfold_cv(mtcars, v = 2)

  lin_mod <- linear_reg() %>%
    set_engine("lm")

  expect <- fit_resamples(lin_mod, mpg ~ ., folds)

  expect_warning(
    result <- tune_grid(lin_mod, mpg ~ ., folds),
    "No tuning parameters have been detected"
  )

  expect_equal(collect_metrics(expect), collect_metrics(result))
})

test_that("`tune_grid()` falls back to `fit_resamples()` - workflow variables", {
  set.seed(6735)
  folds <- vfold_cv(mtcars, v = 2)

  lin_mod <- linear_reg() %>%
    set_engine("lm")

  wf <- workflow() %>%
    add_model(lin_mod) %>%
    add_variables(mpg, c(cyl, disp))

  expect <- fit_resamples(wf, folds)

  expect_warning(
    result <- tune_grid(wf, folds),
    "No tuning parameters have been detected"
  )

  expect_equal(collect_metrics(expect), collect_metrics(result))
})

test_that("`tune_grid()` ignores `grid` if there are no tuning parameters", {
  set.seed(6735)
  folds <- vfold_cv(mtcars, v = 2)

  lin_mod <- linear_reg() %>%
    set_engine("lm")

  expect <- lin_mod %>%
    fit_resamples(mpg ~ ., folds)

  expect_warning(
    result <- lin_mod %>% tune_grid(mpg ~ ., grid = data.frame(x = 1), folds),
    "No tuning parameters have been detected"
  )

  expect_equal(collect_metrics(expect), collect_metrics(result))
})

# ------------------------------------------------------------------------------
# autoplot()

test_that("cannot autoplot `fit_resamples()` results", {
  set.seed(6735)
  folds <- vfold_cv(mtcars, v = 2)

  lin_mod <- linear_reg() %>%
    set_engine("lm")

  result <- lin_mod %>%
    fit_resamples(mpg ~ ., folds)

  expect_error(autoplot(result), "no `autoplot[(][])]` implementation for `resample_results`")
})

test_that("ellipses with fit_resamples", {
  folds <- vfold_cv(mtcars, v = 2)

  lin_mod <- linear_reg() %>%
    set_engine("lm")

  expect_warning(
    lin_mod %>% fit_resamples(mpg ~ ., folds, something = "wrong"),
    "The `...` are not used in this function but one or more objects"
  )
})

test_that("argument order gives errors for recipe/formula", {
  set.seed(6735)
  folds <- vfold_cv(mtcars, v = 2)

  rec <- recipe(mpg ~ ., data = mtcars) %>%
    step_ns(disp) %>%
    step_ns(wt)

  lin_mod <- linear_reg() %>%
    set_engine("lm")

  expect_error(
    fit_resamples(rec, lin_mod, folds),
    "should be either a model or workflow"
  )
  expect_error(
    fit_resamples(mpg ~ ., lin_mod, folds),
    "should be either a model or workflow"
  )
})



test_that("retain extra attributes", {

  set.seed(6735)
  folds <- vfold_cv(mtcars, v = 2)

  lin_mod <- linear_reg() %>%
    set_engine("lm")

  res <- lin_mod %>%
    fit_resamples(mpg ~ ., folds)

  att <- attributes(res)
  att_names <- names(att)
  expect_true(any(att_names == "metrics"))
  expect_true(any(att_names == "outcomes"))
  expect_true(any(att_names == "parameters"))

  expect_true(is.character(att$outcomes))
  expect_true(att$outcomes == "mpg")
  expect_true(inherits(att$parameters, "parameters"))
  expect_true(inherits(att$metrics, "metric_set"))

  res2 <- lin_mod %>%
    fit_resamples(mpg ~ ., folds,
                  control = control_resamples(save_workflow = TRUE))
  expect_null(attr(res, "workflow"))
  expect_true(inherits(attr(res2, "workflow"), "workflow"))

  expect_message(
    fit_resamples(
      lin_mod,
      recipes::recipe(mpg ~ ., mtcars),
      folds,
      control = control_resamples(save_workflow = TRUE)
    ),
    "being saved contains a recipe, which is"
  )
})
