# fit_resamples() --------------------------------------------------------------

test_that("`fit_resamples()` returns a `resample_result` object", {
  set.seed(6735)
  folds <- rsample::vfold_cv(mtcars, v = 2)

  lin_mod <- parsnip::linear_reg() %>%
    parsnip::set_engine("lm")

  result <- lin_mod %>%
    fit_resamples(mpg ~ ., folds)

  expect_s3_class(result, "resample_results")

  expect_equal(result, .Last.tune.result)

  expect_null(.get_tune_eval_times(result))
  expect_null(.get_tune_eval_time_target(result))
})

test_that("can use `fit_resamples()` with a formula", {
  set.seed(6735)
  folds <- rsample::vfold_cv(mtcars, v = 2)

  lin_mod <- parsnip::linear_reg() %>%
    parsnip::set_engine("lm")

  expect_warning(
    result <- lin_mod %>%
      fit_resamples(mpg ~ ., folds),
    NA
  )

  expect_equal(nrow(result$.metrics[[1]]), 2L)
})

test_that("can use `fit_resamples()` with a recipe", {
  skip_if_not_installed("splines2")

  set.seed(6735)
  folds <- rsample::vfold_cv(mtcars, v = 2)

  rec <- recipes::recipe(mpg ~ ., data = mtcars) %>%
    recipes::step_spline_natural(disp) %>%
    recipes::step_spline_natural(wt)

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
  skip_if_not_installed("splines2")

  set.seed(6735)
  folds <- rsample::vfold_cv(mtcars, v = 2)

  rec <- recipes::recipe(mpg ~ ., data = mtcars) %>%
    recipes::step_spline_natural(disp) %>%
    recipes::step_spline_natural(wt)

  lin_mod <- parsnip::linear_reg() %>%
    parsnip::set_engine("lm")

  workflow <- workflow() %>%
    add_recipe(rec) %>%
    add_model(lin_mod)

  expect <- fit_resamples(lin_mod, rec, folds)

  result <- fit_resamples(workflow, folds)

  expect_equal(collect_metrics(expect), collect_metrics(result))
})

test_that("can use `fit_resamples()` with a workflow - variables", {
  set.seed(6735)
  folds <- rsample::vfold_cv(mtcars, v = 2)

  lin_mod <- parsnip::linear_reg() %>%
    parsnip::set_engine("lm")

  workflow <- workflow() %>%
    add_variables(mpg, c(cyl, disp)) %>%
    add_model(lin_mod)

  expect <- fit_resamples(lin_mod, mpg ~ cyl + disp, folds)

  result <- fit_resamples(workflow, folds)

  expect_equal(collect_metrics(expect), collect_metrics(result))
})

test_that("can use `fit_resamples()` with a workflow - formula", {
  set.seed(6735)
  folds <- rsample::vfold_cv(mtcars, v = 2)

  lin_mod <- parsnip::linear_reg() %>%
    parsnip::set_engine("lm")

  workflow <- workflow() %>%
    add_formula(mpg ~ cyl + disp) %>%
    add_model(lin_mod)

  expect <- fit_resamples(lin_mod, mpg ~ cyl + disp, folds)

  result <- fit_resamples(workflow, folds)

  expect_equal(collect_metrics(expect), collect_metrics(result))
})

test_that("extracted workflow is finalized", {
  set.seed(6735)
  folds <- rsample::vfold_cv(mtcars, v = 2)

  lin_mod <- parsnip::linear_reg() %>%
    parsnip::set_engine("lm")

  workflow <- workflow() %>%
    add_variables(mpg, c(cyl, disp)) %>%
    add_model(lin_mod)

  control <- control_resamples(extract = identity)

  result <- fit_resamples(workflow, folds, control = control)
  result_workflow <- result$.extracts[[1]]$.extracts[[1]]

  expect_true(result_workflow$trained)
})

test_that("can use `fit_resamples()` with a workflow - postprocessor (requires training)", {
  skip_if_not_installed("tailor", minimum_version = "0.0.0.9002")
  skip_if_not_installed("probably")

  y <- seq(0, 7, .001)
  dat <- data.frame(y = y, x = y + (y-3)^2)

  dat

  folds <- rsample::vfold_cv(dat, v = 2)

  wflow <-
    workflows::workflow(
      y ~ x,
      parsnip::linear_reg()
    ) %>%
    workflows::add_tailor(
      tailor::tailor() %>% tailor::adjust_numeric_calibration("linear")
    )

  set.seed(1)
  tune_res <-
    fit_resamples(
      wflow,
      folds,
      control = control_resamples(save_pred = TRUE, extract = identity)
    )

  tune_preds <-
    collect_predictions(tune_res) %>%
    dplyr::filter(id == "Fold1")

  tune_wflow <-
    collect_extracts(tune_res) %>%
    pull(.extracts)

  tune_wflow <- tune_wflow[[1]]

  # mock `tune::tune_grid_loop_iter`'s RNG scheme
  set.seed(1)
  seed <- generate_seeds(TRUE, 1)[[1]]
  old_kind <- RNGkind()[[1]]
  assign(".Random.seed", seed, envir = globalenv())
  withr::defer(RNGkind(kind = old_kind))

  inner_split_1 <-
    rsample::inner_split(
      folds$splits[[1]],
      split_args = list(v = 2, repeats = 1, breaks = 4, pool = 0.1)
    )

  wflow_res <-
    generics::fit(
      wflow,
      rsample::analysis(inner_split_1),
      calibration = rsample::assessment(inner_split_1)
    )
  wflow_preds <- predict(wflow_res, rsample::assessment(folds$splits[[1]]))

  tune_wflow$fit$fit$elapsed$elapsed <- wflow_res$fit$fit$elapsed$elapsed
  expect_equal(tune_preds$.pred, wflow_preds$.pred)
  expect_equal(tune_wflow, wflow_res)
})

test_that("can use `fit_resamples()` with a workflow - postprocessor (no training)", {
  skip_if_not_installed("tailor", minimum_version = "0.0.0.9002")
  skip_if_not_installed("probably")

  y <- seq(0, 7, .001)
  dat <- data.frame(y = y, x = y + (y-3)^2)

  folds <- rsample::vfold_cv(dat, v = 2)

  wflow <-
    workflows::workflow(
      y ~ x,
      parsnip::linear_reg()
    ) %>%
    workflows::add_tailor(
      tailor::tailor() %>% tailor::adjust_numeric_range(lower_limit = 1)
    )

  set.seed(1)
  tune_res <-
    fit_resamples(
      wflow,
      folds,
      control = control_resamples(save_pred = TRUE, extract = identity)
    )

  tune_preds <-
    collect_predictions(tune_res) %>%
    dplyr::filter(id == "Fold1")

  tune_wflow <-
    collect_extracts(tune_res) %>%
    pull(.extracts)
  tune_wflow <- tune_wflow[[1]]

  # mock `tune::tune_grid_loop_iter`'s RNG scheme
  set.seed(1)
  seed <- generate_seeds(TRUE, 1)[[1]]
  old_kind <- RNGkind()[[1]]
  assign(".Random.seed", seed, envir = globalenv())

  wflow_res <- generics::fit(wflow, rsample::analysis(folds$splits[[1]]))
  wflow_preds <- predict(wflow_res, rsample::assessment(folds$splits[[1]]))

  tune_wflow$fit$fit$elapsed$elapsed <- wflow_res$fit$fit$elapsed$elapsed
  expect_equal(tune_preds$.pred, wflow_preds$.pred)
  expect_equal(tune_wflow, wflow_res)
})

# Error capture ----------------------------------------------------------------

test_that("failure in recipe is caught elegantly", {
  skip_if_not_installed("splines2")

  set.seed(6735)
  folds <- rsample::vfold_cv(mtcars, v = 2)

  rec <- recipes::recipe(mpg ~ ., data = mtcars) %>%
    recipes::step_spline_natural(disp, deg_free = NA_real_)

  lin_mod <- parsnip::linear_reg() %>%
    parsnip::set_engine("lm")

  control <- control_resamples(extract = function(x) x, save_pred = TRUE)

  expect_snapshot(
    result <- fit_resamples(lin_mod, rec, folds, control = control)
  )

  notes <- result$.notes
  note <- notes[[1]]$note

  extract <- result$.extracts
  predictions <- result$.predictions

  expect_length(notes, 2L)

  expect_snapshot(note)

  expect_equal(extract, list(NULL, NULL))
  expect_equal(predictions, list(NULL, NULL))
})

test_that("failure in variables tidyselect specification is caught elegantly", {
  set.seed(6735)
  folds <- rsample::vfold_cv(mtcars, v = 2)

  lin_mod <- parsnip::linear_reg() %>%
    parsnip::set_engine("lm")

  workflow <- workflow() %>%
    add_model(lin_mod) %>%
    add_variables(mpg, foobar)

  control <- control_resamples(extract = function(x) x, save_pred = TRUE)

  expect_snapshot(
    result <- fit_resamples(workflow, folds, control = control)
  )

  notes <- result$.notes
  note <- notes[[1]]$note

  extract <- result$.extracts
  predictions <- result$.predictions

  expect_length(notes, 2L)

  expect_snapshot(note)

  expect_equal(extract, list(NULL, NULL))
  expect_equal(predictions, list(NULL, NULL))
})

test_that("classification models generate correct error message", {
  set.seed(6735)
  folds <- rsample::vfold_cv(mtcars, v = 2)

  rec <- recipes::recipe(vs ~ ., data = mtcars)

  log_mod <- parsnip::logistic_reg() %>%
    parsnip::set_engine("glm")

  control <- control_resamples(extract = function(x) x, save_pred = TRUE)

  expect_snapshot(
    result <- fit_resamples(log_mod, rec, folds, control = control)
  )

  notes <- result$.notes
  note <- notes[[1]]$note

  extract <- result$.extracts
  predictions <- result$.predictions

  expect_length(notes, 2L)

  expect_snapshot(note)

  expect_equal(extract, list(NULL, NULL))
  expect_equal(predictions, list(NULL, NULL))
})


# tune_grid() fallback ---------------------------------------------------------

test_that("`tune_grid()` falls back to `fit_resamples()` - formula", {
  set.seed(6735)
  folds <- rsample::vfold_cv(mtcars, v = 2)

  lin_mod <- parsnip::linear_reg() %>%
    parsnip::set_engine("lm")

  expect <- fit_resamples(lin_mod, mpg ~ ., folds)

  expect_snapshot(
    result <- tune_grid(lin_mod, mpg ~ ., folds)
  )

  expect_equal(collect_metrics(expect), collect_metrics(result))
})

test_that("`tune_grid()` falls back to `fit_resamples()` - workflow variables", {
  set.seed(6735)
  folds <- rsample::vfold_cv(mtcars, v = 2)

  lin_mod <- parsnip::linear_reg() %>%
    parsnip::set_engine("lm")

  wf <- workflow() %>%
    add_model(lin_mod) %>%
    add_variables(mpg, c(cyl, disp))

  expect <- fit_resamples(wf, folds)

  expect_snapshot(
    result <- tune_grid(wf, folds)
  )

  expect_equal(collect_metrics(expect), collect_metrics(result))
})

test_that("`tune_grid()` ignores `grid` if there are no tuning parameters", {
  set.seed(6735)
  folds <- rsample::vfold_cv(mtcars, v = 2)

  lin_mod <- parsnip::linear_reg() %>%
    parsnip::set_engine("lm")

  expect <- lin_mod %>%
    fit_resamples(mpg ~ ., folds)

  expect_snapshot(
    result <- lin_mod %>% tune_grid(mpg ~ ., grid = data.frame(x = 1), folds)
  )

  expect_equal(collect_metrics(expect), collect_metrics(result))
})


# autoplot() -------------------------------------------------------------------

test_that("cannot autoplot `fit_resamples()` results", {
  set.seed(6735)
  folds <- rsample::vfold_cv(mtcars, v = 2)

  lin_mod <- parsnip::linear_reg() %>%
    parsnip::set_engine("lm")

  result <- lin_mod %>%
    fit_resamples(mpg ~ ., folds)

  expect_snapshot(error = TRUE, {
    autoplot(result)
  })
})

test_that("ellipses with fit_resamples", {
  folds <- rsample::vfold_cv(mtcars, v = 2)

  lin_mod <- parsnip::linear_reg() %>%
    parsnip::set_engine("lm")

  expect_snapshot(
    lin_mod %>% fit_resamples(mpg ~ ., folds, something = "wrong")
  )
})

test_that("argument order gives errors for recipe/formula", {
  skip_if_not_installed("splines2")

  set.seed(6735)
  folds <- rsample::vfold_cv(mtcars, v = 2)

  rec <- recipes::recipe(mpg ~ ., data = mtcars) %>%
    recipes::step_spline_natural(disp) %>%
    recipes::step_spline_natural(wt)

  lin_mod <- parsnip::linear_reg() %>%
    parsnip::set_engine("lm")

  expect_snapshot(error = TRUE, {
    fit_resamples(rec, lin_mod, folds)
  })
  expect_snapshot(error = TRUE, {
    fit_resamples(mpg ~ ., lin_mod, folds)
  })
})



test_that("retain extra attributes", {
  skip_if_not_installed("splines2")

  set.seed(6735)
  folds <- rsample::vfold_cv(mtcars, v = 2)

  lin_mod <- parsnip::linear_reg() %>%
    parsnip::set_engine("lm")

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
    fit_resamples(
      mpg ~ .,
      folds,
      control = control_resamples(save_workflow = TRUE)
    )
  expect_null(attr(res, "workflow"))
  expect_true(inherits(attr(res2, "workflow"), "workflow"))

  expect_snapshot(
    fit_resamples(
      lin_mod,
      recipes::recipe(mpg ~ ., mtcars[rep(1:32, 3000), ]),
      folds,
      control = control_resamples(save_workflow = TRUE)
    )
  )
})


test_that("`fit_resamples()` when objects need tuning", {
  rec <- recipe(mpg ~ ., data = mtcars) %>% step_spline_natural(disp, deg_free = tune())
  spec_1 <- linear_reg(penalty = tune()) %>% set_engine("glmnet")
  spec_2 <- linear_reg()
  wflow_1 <- workflow(rec, spec_1)
  wflow_2 <- workflow(mpg ~ ., spec_1)
  wflow_3 <- workflow(rec, spec_2)
  rs <- rsample::vfold_cv(mtcars)

  expect_snapshot_error(fit_resamples(wflow_1, rs))
  expect_snapshot_error(fit_resamples(wflow_2, rs))
  expect_snapshot_error(fit_resamples(wflow_3, rs))
})

