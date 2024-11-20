rec_tune_1 <-
  recipes::recipe(mpg ~ ., data = mtcars) %>%
  recipes::step_normalize(recipes::all_predictors()) %>%
  recipes::step_pca(recipes::all_predictors(), num_comp = tune())

rec_no_tune_1 <-
  recipes::recipe(mpg ~ ., data = mtcars) %>%
  recipes::step_normalize(recipes::all_predictors())

lm_mod <- parsnip::linear_reg() %>% parsnip::set_engine("lm")

svm_mod <- parsnip::svm_rbf(mode = "regression", cost = tune()) %>%
  parsnip::set_engine("kernlab")

iter1 <- 2
iter2 <- 2
iterT <- iter1 + iter2

# ------------------------------------------------------------------------------

test_that("tune recipe only", {
  set.seed(4400)
  wflow <- workflow() %>%
    add_recipe(rec_tune_1) %>%
    add_model(lm_mod)
  pset <- extract_parameter_set_dials(wflow) %>% update(num_comp = dials::num_comp(c(1, 5)))
  folds <- rsample::vfold_cv(mtcars)
  control <- control_bayes(extract = identity)

  suppressMessages({
    res <- tune_bayes(
      wflow,
      resamples = folds,
      param_info = pset,
      initial = iter1,
      iter = iter2,
      control = control
    )
  })
  res_est <- collect_metrics(res)
  res_workflow <- res$.extracts[[1]]$.extracts[[1]]

  expect_equal(res, .Last.tune.result)

  # Ensure tunable parameters in recipe are finalized
  num_comp <- res_workflow$pre$actions$recipe$recipe$steps[[2]]$num_comp

  expect_equal(unique(res$id), folds$id)
  expect_equal(nrow(res_est), iterT * 2)
  expect_equal(sum(res_est$.metric == "rmse"), iterT)
  expect_equal(sum(res_est$.metric == "rsq"), iterT)
  expect_equal(dplyr::n_distinct(res_est$.config), iterT)
  expect_equal(res_est$n, rep(10, iterT * 2))
  expect_false(identical(num_comp, expr(tune())))
  expect_true(res_workflow$trained)
  expect_null(.get_tune_eval_times(res))
  expect_null(.get_tune_eval_time_target(res))

  set.seed(1)
  expect_no_error(
    suppressMessages(
      tune_bayes(
        wflow,
        resamples = folds,
        param_info = pset,
        initial = iter1,
        iter = iter2,
        corr = list(type = "matern", nu = 3 / 2)
      )
    )
  )


  # test verbose options
  set.seed(1)
  expect_snapshot(
    tune_bayes(
      wflow,
      resamples = folds,
      param_info = pset,
      initial = iter1,
      iter = iter2,
      control = control_bayes(verbose = TRUE)
    )
  )

  set.seed(1)
  expect_snapshot(
    tune_bayes(
      wflow,
      resamples = folds,
      param_info = pset,
      initial = iter1,
      iter = iter2,
      control = control_bayes(verbose_iter = TRUE)
    )
  )

  set.seed(1)
  expect_snapshot(
    tune_bayes(
      wflow,
      resamples = folds,
      param_info = pset,
      initial = iter1,
      iter = iter2,
      control = control_bayes(verbose_iter = TRUE, verbose = TRUE)
    )
  )
})

# ------------------------------------------------------------------------------

test_that("tune model only (with recipe)", {
  skip_if_not_installed("kernlab")

  set.seed(4400)
  wflow <- workflow() %>%
    add_recipe(rec_no_tune_1) %>%
    add_model(svm_mod)
  pset <- extract_parameter_set_dials(wflow)
  folds <- rsample::vfold_cv(mtcars)
  suppressMessages({
    res <- tune_bayes(
      wflow,
      resamples = folds,
      param_info = pset,
      initial = iter1,
      iter = iter2
    )
  })

  expect_equal(res, .Last.tune.result)
  expect_equal(unique(res$id), folds$id)
  res_est <- collect_metrics(res)
  expect_equal(nrow(res_est), iterT * 2)
  expect_equal(sum(res_est$.metric == "rmse"), iterT)
  expect_equal(sum(res_est$.metric == "rsq"), iterT)
  expect_equal(dplyr::n_distinct(res_est$.config), iterT)
  expect_equal(res_est$n, rep(10, iterT * 2))
})

# ------------------------------------------------------------------------------

test_that("tune model only (with variables)", {
  skip_if_not_installed("kernlab")

  set.seed(4400)

  wflow <- workflow() %>%
    add_variables(mpg, everything()) %>%
    add_model(svm_mod)

  pset <- extract_parameter_set_dials(wflow)

  folds <- rsample::vfold_cv(mtcars)

  suppressMessages({
    res <- tune_bayes(
      wflow,
      resamples = folds,
      param_info = pset,
      initial = iter1,
      iter = iter2
    )
  })

  expect_equal(unique(res$id), folds$id)

  res_est <- collect_metrics(res)

  expect_equal(nrow(res_est), iterT * 2)
  expect_equal(sum(res_est$.metric == "rmse"), iterT)
  expect_equal(sum(res_est$.metric == "rsq"), iterT)
  expect_equal(dplyr::n_distinct(res_est$.config), iterT)
  expect_equal(res_est$n, rep(10, iterT * 2))
})

# ------------------------------------------------------------------------------

test_that("tune model only (with recipe, multi-predict)", {
  skip_if_not_installed("kernlab")

  skip_on_cran()

  set.seed(4400)
  wflow <- workflow() %>%
    add_recipe(rec_no_tune_1) %>%
    add_model(svm_mod)
  pset <- extract_parameter_set_dials(wflow)
  folds <- rsample::vfold_cv(mtcars)
  suppressMessages({
    res <- tune_bayes(
      wflow,
      resamples = folds,
      param_info = pset,
      initial = iter1,
      iter = iter2
    )
  })

  expect_equal(unique(res$id), folds$id)
  expect_equal(
    colnames(res$.metrics[[1]]),
    c("cost", ".metric", ".estimator", ".estimate", ".config")
  )
  res_est <- collect_metrics(res)
  expect_equal(nrow(res_est), iterT * 2)
  expect_equal(sum(res_est$.metric == "rmse"), iterT)
  expect_equal(sum(res_est$.metric == "rsq"), iterT)
  expect_equal(dplyr::n_distinct(res_est$.config), iterT)
  expect_equal(res_est$n, rep(10, iterT * 2))
})

# ------------------------------------------------------------------------------

test_that("tune model and recipe", {
  skip_if_not_installed("kernlab")

  set.seed(4400)
  wflow <- workflow() %>%
    add_recipe(rec_tune_1) %>%
    add_model(svm_mod)
  pset <- extract_parameter_set_dials(wflow) %>% update(num_comp = dials::num_comp(c(1, 3)))
  folds <- rsample::vfold_cv(mtcars)
  suppressMessages({
    res <- tune_bayes(
      wflow,
      resamples = folds,
      param_info = pset,
      initial = iter1,
      iter = iter2
    )
  })

  expect_equal(unique(res$id), folds$id)
  expect_equal(
    colnames(res$.metrics[[1]]),
    c("cost", "num_comp", ".metric", ".estimator", ".estimate", ".config")
  )
  res_est <- collect_metrics(res)
  expect_equal(nrow(res_est), iterT * 2)
  expect_equal(sum(res_est$.metric == "rmse"), iterT)
  expect_equal(sum(res_est$.metric == "rsq"), iterT)
  expect_equal(dplyr::n_distinct(res_est$.config), iterT)
  expect_equal(res_est$n, rep(10, iterT * 2))
})

# ------------------------------------------------------------------------------

test_that("tune model and recipe (multi-predict)", {
  skip_if_not_installed("kernlab")
  skip_on_cran()

  set.seed(4400)
  wflow <- workflow() %>%
    add_recipe(rec_tune_1) %>%
    add_model(svm_mod)
  pset <- extract_parameter_set_dials(wflow) %>% update(num_comp = dials::num_comp(c(2, 3)))
  grid <- dials::grid_regular(pset, levels = c(3, 2))
  folds <- rsample::vfold_cv(mtcars)
  suppressMessages({
    res <- tune_bayes(
      wflow,
      resamples = folds,
      param_info = pset,
      initial = iter1,
      iter = iter2
    )
  })

  expect_equal(unique(res$id), folds$id)
  res_est <- collect_metrics(res)
  expect_equal(nrow(res_est), iterT * 2)
  expect_equal(sum(res_est$.metric == "rmse"), iterT)
  expect_equal(sum(res_est$.metric == "rsq"), iterT)
  expect_equal(dplyr::n_distinct(res_est$.config), iterT)
  expect_equal(res_est$n, rep(10, iterT * 2))
})

# ------------------------------------------------------------------------------

test_that("tune recipe only - failure in recipe is caught elegantly", {
  skip("test is not implemented for tune_bayes()")
  skip_if_not_installed("splines2")

  # With tune_grid() this tests for NA values in the grid.
  # This is not applicable for tune_bayes().

  set.seed(7898)
  data_folds <- rsample::vfold_cv(mtcars, v = 2)

  rec <- recipes::recipe(mpg ~ ., data = mtcars) %>%
    recipes::step_spline_b(disp, deg_free = tune())

  model <- parsnip::linear_reg(mode = "regression") %>%
    parsnip::set_engine("lm")

  # NA values not allowed in recipe
  cars_grid <- tibble(deg_free = c(3, NA_real_, 4))

  # ask for predictions and extractions
  control <- control_bayes(
    save_pred = TRUE,
    extract = function(x) 1L
  )

  suppressMessages({
    cars_res <- tune_bayes(
      model,
      preprocessor = rec,
      resamples = data_folds,
      control = control
    )
  })

  notes <- cars_res$.notes
  note <- notes[[1]]$note

  extract <- cars_res$.extracts[[1]]

  predictions <- cars_res$.predictions[[1]]
  used_deg_free <- sort(unique(predictions$deg_free))

  expect_length(notes, 2L)

  # failing rows are not in the output
  expect_equal(nrow(extract), 2L)
  expect_equal(extract$deg_free, c(3, 4))

  expect_equal(used_deg_free, c(3, 4))
})

test_that("tune model only - failure in recipe is caught elegantly", {
  skip_if_not_installed("splines2")
  skip_if_not_installed("kernlab")

  set.seed(7898)
  data_folds <- rsample::vfold_cv(mtcars, v = 2)

  # NA values not allowed in recipe
  rec <- recipes::recipe(mpg ~ ., data = mtcars) %>%
    recipes::step_spline_b(disp, deg_free = NA_real_)

  expect_snapshot({
    cars_res <- tune_bayes(
      svm_mod,
      preprocessor = rec,
      resamples = data_folds
    )
  })

  expect_s3_class(cars_res, "iteration_results")
})

test_that("tune model only - failure in formula is caught elegantly", {
  skip_if_not_installed("kernlab")

  set.seed(7898)
  data_folds <- rsample::vfold_cv(mtcars, v = 2)

  # these terms don't exist!
  wflow <- workflow() %>%
    add_formula(y ~ z) %>%
    add_model(svm_mod)

  expect_snapshot({
    cars_res <- tune_bayes(
      wflow,
      resamples = data_folds,
      control = control_bayes(extract = function(x) {1}, save_pred = TRUE)
    )
  })

  expect_s3_class(cars_res, "iteration_results")
})

test_that("tune model and recipe - failure in recipe is caught elegantly", {
  skip("test is not implemented for tune_bayes()")
  skip_if_not_installed("splines2")
  skip_if_not_installed("kernlab")

  # With tune_grid() this tests for NA values in the grid.
  # This is not applicable for tune_bayes().

  set.seed(7898)
  data_folds <- rsample::vfold_cv(mtcars, v = 2)

  rec <- recipes::recipe(mpg ~ ., data = mtcars) %>%
    recipes::step_spline_b(disp, deg_free = tune())


  # NA values not allowed in recipe
  cars_grid <- tibble(deg_free = c(NA_real_, 10L), cost = 0.01)

  suppressMessages({
    cars_res <- tune_bayes(
      svm_mod,
      preprocessor = rec,
      resamples = data_folds,
      control = control_bayes(extract = function(x) {1}, save_pred = TRUE)
    )
  })

  notes <- cars_res$.notes
  note <- notes[[1]]$note

  extract <- cars_res$.extracts[[1]]
  prediction <- cars_res$.predictions[[1]]

  expect_length(notes, 2L)

  # recipe failed half of the time, only 1 model passed
  expect_equal(nrow(extract), 1L)
  expect_equal(extract$deg_free, 10L)
  expect_equal(extract$cost, 0.01)

  expect_equal(
    unique(prediction[, c("deg_free", "cost")]),
    tibble(deg_free = 10, cost = 0.01)
  )
})

test_that("argument order gives an error for recipes", {
  expect_snapshot(error = TRUE, {
    tune_bayes(
      rec_tune_1,
      model = lm_mod,
      resamples = rsample::vfold_cv(mtcars, v = 2),
      param_info = extract_parameter_set_dials(rec_tune_1),
      iter = iter1,
      initial = iter2
    )
  })
})

test_that("argument order gives an error for formula", {
  skip_if_not_installed("kernlab")

  expect_snapshot(error = TRUE, {
    tune_bayes(
      mpg ~ .,
      svm_mod,
      resamples = rsample::vfold_cv(mtcars, v = 2),
      param_info = extract_parameter_set_dials(svm_mod),
      initial = iter1,
      iter = iter2
    )
  })
})

test_that("retain extra attributes and saved GP candidates", {
  set.seed(4400)
  wflow <- workflow() %>%
    add_recipe(rec_tune_1) %>%
    add_model(lm_mod)
  pset <- extract_parameter_set_dials(wflow) %>%
    update(num_comp = dials::num_comp(c(1, 5)))
  folds <- rsample::vfold_cv(mtcars)
  ctrl <- control_bayes(save_gp_scoring = TRUE)
  suppressMessages({
    res <- tune_bayes(
      wflow,
      resamples = folds,
      param_info = pset,
      initial = iter1,
      iter = iter2,
      control = ctrl
    )
  })
  att <- attributes(res)
  att_names <- names(att)
  expect_true(any(att_names == "metrics"))
  expect_true(any(att_names == "outcomes"))
  expect_true(any(att_names == "parameters"))

  expect_true(is.character(att$outcomes))
  expect_true(att$outcomes == "mpg")
  expect_true(inherits(att$parameters, "parameters"))
  expect_true(inherits(att$metrics, "metric_set"))

  files <- list.files(path = tempdir(), pattern = "^gp_candidates")
  expect_true(length(files) == iter2)

  current_objs <- c(ls(), "current_objs")
  load(file.path(tempdir(), "gp_candidates_1.RData"))
  new_obj <- ls()
  expect_snapshot(setdiff(new_obj, current_objs))

  expect_snapshot(
    res2 <- tune_bayes(
      wflow,
      resamples = folds,
      param_info = pset,
      initial = iter1,
      iter = iter2,
      control = control_bayes(save_workflow = TRUE)
    )
  )
  expect_null(attr(res, "workflow"))
  expect_true(inherits(attr(res2, "workflow"), "workflow"))
})

# ------------------------------------------------------------------------------

test_that("too few starting values", {
  options(width = 120)
  # TODO Add specific checks with racing objects once finetune is released
  expect_silent(tune:::check_bayes_initial_size(5, 30, FALSE))

  expect_snapshot(tune:::check_bayes_initial_size(5, 3, FALSE))
  expect_snapshot(tune:::check_bayes_initial_size(5, 3, TRUE))
  expect_snapshot(tune:::check_bayes_initial_size(2, 2, FALSE))

  expect_snapshot(error = TRUE, tune:::check_bayes_initial_size(5, 1, FALSE))
  expect_snapshot(error = TRUE, tune:::check_bayes_initial_size(5, 1, TRUE))

  expect_snapshot(error = TRUE, tune:::check_bayes_initial_size(1, 1, FALSE))
})

# ------------------------------------------------------------------------------

test_that("missing performance values", {
  skip_if(new_rng_snapshots)
  skip_if(packageVersion("dplyr") < "1.1.1")
  skip_if_not_installed("modeldata")

  data(ames, package = "modeldata")

  mod <- parsnip::decision_tree(cost_complexity = tune()) %>%
    parsnip::set_mode("regression")

  set.seed(1)
  split <- rsample::initial_validation_split(ames)
  folds <- rsample::validation_set(split)

  expect_snapshot({
    set.seed(1)
    res <-
      mod %>%
      tune_bayes(
        Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type +
          Latitude + Longitude,
        resamples = folds,
        initial = 3,
        metrics = yardstick::metric_set(rsq),
        param_info = parameters(dials::cost_complexity(c(-2, 0)))
      )
  })

  expect_snapshot(error = TRUE, {
    set.seed(2)
    res_fail <-
      mod %>%
      tune_bayes(
        Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type +
          Latitude + Longitude,
        resamples = folds,
        initial = 5,
        metrics = yardstick::metric_set(rsq),
        param_info = parameters(dials::cost_complexity(c(0.5, 0)))
      )
  })
})

# ------------------------------------------------------------------------------
test_that("tune_bayes() output for `iter` edge cases (#721)", {
  skip_if_not_installed("kknn")

  # for `iter = 0`, ought to match `tune_grid()`
  boots <- rsample::bootstraps(mtcars)
  wf <-
    workflows::workflow(
      mpg ~ .,
      parsnip::nearest_neighbor("regression", "kknn", neighbors = tune())
    )

  ctrl_bayes <- control_bayes(seed = 1)

  set.seed(1)
  res_bayes <- tune_bayes(wf, boots, iter = 0, initial = 10,
                          control = ctrl_bayes)

  set.seed(1)
  res_grid <- tune_grid(wf, boots)

  expect_equal(
    collect_metrics(res_bayes) %>% dplyr::select(-.iter),
    collect_metrics(res_grid)
  )

  # for `iter < 0`, ought to error
  expect_snapshot(
    error = TRUE,
    tune_bayes(wf, boots, iter = -1)
  )

  expect_snapshot(
    error = TRUE,
    tune_bayes(wf, boots, iter = c(-1, 0, 1))
  )

  expect_snapshot(
    error = TRUE,
    tune_bayes(wf, boots, iter = c(0, 1, 2))
  )

  expect_snapshot(
    error = TRUE,
    tune_bayes(wf, boots, iter = NA)
  )

  expect_snapshot(
    error = TRUE,
    tune_bayes(wf, boots, iter = NULL)
  )
})
