context("Bayesian search")

# ------------------------------------------------------------------------------

source(test_path("../helper-objects.R"))

# ------------------------------------------------------------------------------

rec_tune_1 <-
  recipe(mpg ~ ., data = mtcars) %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors(), num_comp = tune())

rec_no_tune_1 <-
  recipe(mpg ~ ., data = mtcars) %>%
  step_normalize(all_predictors())

lm_mod <- linear_reg() %>% set_engine("lm")

svm_mod <- svm_rbf(mode = "regression", cost = tune()) %>% set_engine("kernlab")

rf_mod <- rand_forest(mode = "regression", mtry = tune()) %>% set_engine("randomForest")

iter1 <- 2
iter2 <- 2
iterT <- iter1 + iter2

# ------------------------------------------------------------------------------

test_that('tune recipe only', {
  set.seed(4400)
  wflow <- workflow() %>% add_recipe(rec_tune_1) %>% add_model(lm_mod)
  pset <- dials::parameters(wflow) %>% update(num_comp = num_comp(c(1, 5)))
  folds <- vfold_cv(mtcars)
  control <- control_bayes(extract = identity)

  res <- tune_bayes(wflow, resamples = folds, param_info = pset,
                    initial = iter1, iter = iter2, control = control)
  res_est <- collect_metrics(res)
  res_workflow <- res$.extracts[[1]]$.extracts[[1]]

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

  expect_error(
    tune_bayes(wflow, resamples = folds, param_info = pset,
               initial = iter1, iter = iter2,
               corr = list(type = "matern", nu = 3/2)),
    regexp = NA
  )
})

# ------------------------------------------------------------------------------

test_that('tune model only (with recipe)', {

  set.seed(4400)
  wflow <- workflow() %>% add_recipe(rec_no_tune_1) %>% add_model(svm_mod)
  pset <- dials::parameters(wflow)
  folds <- vfold_cv(mtcars)
  res <- tune_bayes(wflow, resamples = folds, param_info = pset,
                    initial = iter1, iter = iter2)
  expect_equal(unique(res$id), folds$id)
  res_est <- collect_metrics(res)
  expect_equal(nrow(res_est), iterT * 2)
  expect_equal(sum(res_est$.metric == "rmse"), iterT)
  expect_equal(sum(res_est$.metric == "rsq"), iterT)
  expect_equal(dplyr::n_distinct(res_est$.config), iterT)
  expect_equal(res_est$n, rep(10, iterT * 2))
})

# ------------------------------------------------------------------------------

test_that('tune model only (with variables)', {
  set.seed(4400)

  wflow <- workflow() %>%
    add_variables(mpg, everything()) %>%
    add_model(svm_mod)

  pset <- dials::parameters(wflow)

  folds <- vfold_cv(mtcars)

  res <- tune_bayes(
    wflow,
    resamples = folds,
    param_info = pset,
    initial = iter1,
    iter = iter2
  )

  expect_equal(unique(res$id), folds$id)

  res_est <- collect_metrics(res)

  expect_equal(nrow(res_est), iterT * 2)
  expect_equal(sum(res_est$.metric == "rmse"), iterT)
  expect_equal(sum(res_est$.metric == "rsq"), iterT)
  expect_equal(dplyr::n_distinct(res_est$.config), iterT)
  expect_equal(res_est$n, rep(10, iterT * 2))
})

# ------------------------------------------------------------------------------

test_that('tune model only (with recipe, multi-predict)', {

  skip_if_not(has_multi_predict())

  set.seed(4400)
  wflow <- workflow() %>% add_recipe(rec_no_tune_1) %>% add_model(svm_mod)
  pset <- dials::parameters(wflow)
  folds <- vfold_cv(mtcars)
  res <- tune_bayes(wflow, resamples = folds, param_info = pset,
                    initial = iter1, iter = iter2)
  expect_equal(unique(res$id), folds$id)
  expect_equal(
    colnames(res$.metrics[[1]]),
    c("cost", ".metric", ".estimator", ".estimate")
  )
  res_est <- collect_metrics(res)
  expect_equal(nrow(res_est), iterT * 2)
  expect_equal(sum(res_est$.metric == "rmse"), iterT)
  expect_equal(sum(res_est$.metric == "rsq"), iterT)
  expect_equal(dplyr::n_distinct(res_est$.config), iterT)
  expect_equal(res_est$n, rep(10, iterT * 2))
})

# ------------------------------------------------------------------------------

test_that('tune model and recipe', {

  set.seed(4400)
  wflow <- workflow() %>% add_recipe(rec_tune_1) %>% add_model(svm_mod)
  pset <- dials::parameters(wflow) %>% update(num_comp = num_comp(c(1, 3)))
  folds <- vfold_cv(mtcars)
  res <- tune_bayes(wflow, resamples = folds, param_info = pset,
                    initial = iter1, iter = iter2)
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

test_that('tune model and recipe (multi-predict)', {

  skip_if_not(has_multi_predict())

  set.seed(4400)
  wflow <- workflow() %>% add_recipe(rec_tune_1) %>% add_model(svm_mod)
  pset <- dials::parameters(wflow) %>% update(num_comp = num_comp(c(2, 3)))
  grid <- grid_regular(pset, levels = c(3, 2))
  folds <- vfold_cv(mtcars)
  res <- tune_bayes(wflow, resamples = folds, param_info = pset,
                    initial = iter1, iter = iter2)
  expect_equal(unique(res$id), folds$id)
  res_est <- collect_metrics(res)
  expect_equal(nrow(res_est), iterT * 2)
  expect_equal(sum(res_est$.metric == "rmse"), iterT)
  expect_equal(sum(res_est$.metric == "rsq"), iterT)
  expect_equal(dplyr::n_distinct(res_est$.config), iterT)
  expect_equal(res_est$n, rep(10, iterT * 2))
})

# ------------------------------------------------------------------------------

test_that('tune recipe and model, which has_unknowns', {

  # This test needs a tuning parameter object with default 'unknown' parameter
  # values (e.g., mtry).

  skip_if_not_installed("randomForest")

  set.seed(4400)
  wflow <- workflow() %>% add_recipe(rec_tune_1) %>% add_model(rf_mod)
  pset <- dials::parameters(wflow) %>% update(num_comp = num_comp(c(3, 5)),
                                              mtry = mtry(c(1, 3)))
  expect_true(
    any(
      vapply(
        dials::parameters(wflow)$object,
        dials::has_unknowns,
        FUN.VALUE = TRUE
      )
    )
  )
  folds <- vfold_cv(mtcars)
  res <- tune_bayes(wflow, resamples = folds, param_info = pset,
                    initial = iter1, iter = iter2)
  expect_equal(unique(res$id), folds$id)
  expect_equal(
    colnames(res$.metrics[[1]]),
    c("mtry", "num_comp", ".metric", ".estimator", ".estimate", ".config")
  )
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

  # With tune_grid() this tests for NA values in the grid.
  # This is not applicable for tune_bayes().

  set.seed(7898)
  data_folds <- vfold_cv(mtcars, v = 2)

  rec <- recipe(mpg ~ ., data = mtcars) %>%
    step_bs(disp, deg_free = tune())

  model <- linear_reg(mode = "regression") %>%
    set_engine("lm")

  # NA values not allowed in recipe
  cars_grid <- tibble(deg_free = c(3, NA_real_, 4))

  # ask for predictions and extractions
  control <- control_bayes(
    save_pred = TRUE,
    extract = function(x) 1L
  )

  cars_res <- tune_bayes(
    model,
    preprocessor = rec,
    resamples = data_folds,
    control = control
  )

  notes <- cars_res$.notes
  note <- notes[[1]]$.notes

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

  set.seed(7898)
  data_folds <- vfold_cv(mtcars, v = 2)

  # NA values not allowed in recipe
  rec <- recipe(mpg ~ ., data = mtcars) %>%
    step_bs(disp, deg_free = NA_real_)

  expect_error(
    cars_res <- suppressWarnings(
      tune_bayes(
        svm_mod,
        preprocessor = rec,
        resamples = data_folds
      )),
    "All of the models failed"
  )
})

test_that("tune model only - failure in formula is caught elegantly", {

  set.seed(7898)
  data_folds <- vfold_cv(mtcars, v = 2)

  # these terms don't exist!
  wflow <- workflow() %>%
    add_formula(y ~ z) %>%
    add_model(svm_mod)

  expect_error(
    cars_res <- suppressWarnings(
      tune_bayes(
        wflow,
        resamples = data_folds,
        control = control_bayes(extract = function(x) {1}, save_pred = TRUE)
      )),
    "All of the models failed"
  )
})

test_that("tune model and recipe - failure in recipe is caught elegantly", {

  skip("test is not implemented for tune_bayes()")

  # With tune_grid() this tests for NA values in the grid.
  # This is not applicable for tune_bayes().

  set.seed(7898)
  data_folds <- vfold_cv(mtcars, v = 2)

  rec <- recipe(mpg ~ ., data = mtcars) %>%
    step_bs(disp, deg_free = tune())


  # NA values not allowed in recipe
  cars_grid <- tibble(deg_free = c(NA_real_, 10L), cost = 0.01)

  cars_res <- tune_bayes(
    svm_mod,
    preprocessor = rec,
    resamples = data_folds,
    control = control_bayes(extract = function(x) {1}, save_pred = TRUE)
  )

  notes <- cars_res$.notes
  note <- notes[[1]]$.notes

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

test_that("argument order gives warning for recipes", {
  expect_error(
    tune_bayes(rec_tune_1, model = lm_mod, resamples = vfold_cv(mtcars, v = 2),
               param_info = dials::parameters(rec_tune_1),
               iter = iter1, initial = iter2),
    "should be either a model or workflow"
  )
})

test_that("argument order gives warning for formula", {
  expect_error(
    tune_bayes(mpg ~ ., svm_mod, resamples = vfold_cv(mtcars, v = 2),
               param_info = dials::parameters(svm_mod),
               initial = iter1, iter = iter2),
    "should be either a model or workflow"
  )
})

test_that("retain extra attributes and saved GP candidates", {

  set.seed(4400)
  wflow <- workflow() %>% add_recipe(rec_tune_1) %>% add_model(lm_mod)
  pset <- dials::parameters(wflow) %>% update(num_comp = num_comp(c(1, 5)))
  folds <- vfold_cv(mtcars)
  ctrl <- control_bayes(save_gp_scoring = TRUE)
  res <- tune_bayes(wflow, resamples = folds, param_info = pset,
                    initial = iter1, iter = iter2, control = ctrl)

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


  expect_message(
    res2 <- tune_bayes(wflow, resamples = folds, param_info = pset,
                       initial = iter1, iter = iter2,
                       control = control_bayes(save_workflow = TRUE)),
    "being saved contains a recipe, which is"
  )
  expect_null(attr(res, "workflow"))
  expect_true(inherits(attr(res2, "workflow"), "workflow"))


})

# ------------------------------------------------------------------------------

test_that('too few starting values', {
  options(width = 120)
  # TODO Add specific checks with racing objects once finetune is released
  expect_silent(tune:::check_bayes_initial_size(5, 30, FALSE))

  expect_message(
    tune:::check_bayes_initial_size(5, 3, FALSE),
    "5 tuning parameters and 3 grid points were"
  )

  expect_message(
    tune:::check_bayes_initial_size(5, 3, TRUE),
    "numerical issues"
  )
  expect_message(
    tune:::check_bayes_initial_size(5, 3, TRUE),
    "With racing"
  )

  expect_error(
    tune:::check_bayes_initial_size(5, 1, FALSE),
    "requires 2+"
  )

  expect_error(
    tune:::check_bayes_initial_size(5, 1, TRUE),
    "requires 2+"
  )
  expect_error(
    tune:::check_bayes_initial_size(5, 1, TRUE),
    "With racing"
  )

  expect_error(
    tune:::check_bayes_initial_size(5, 1, FALSE),
    "a single grid point was"
  )
  expect_error(
    tune:::check_bayes_initial_size(1, 1, FALSE),
    "is one tuning parameter"
  )

})

