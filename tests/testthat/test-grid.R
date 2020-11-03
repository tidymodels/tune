context("grid search")

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

# ------------------------------------------------------------------------------

test_that('tune recipe only', {
  set.seed(4400)
  wflow <- workflow() %>% add_recipe(rec_tune_1) %>% add_model(lm_mod)
  pset <- dials::parameters(wflow) %>% update(num_comp = num_comp(c(1, 3)))
  grid <- grid_regular(pset, levels = 3)
  folds <- vfold_cv(mtcars)
  control <- control_grid(extract = identity)

  res <- tune_grid(wflow, resamples = folds, grid = grid, control = control)
  res_est <- collect_metrics(res)
  res_workflow <- res$.extracts[[1]]$.extracts[[1]]

  # Ensure tunable parameters in recipe are finalized
  num_comp <- res_workflow$pre$actions$recipe$recipe$steps[[2]]$num_comp

  expect_equal(res$id, folds$id)
  expect_equal(nrow(res_est), nrow(grid) * 2)
  expect_equal(sum(res_est$.metric == "rmse"), nrow(grid))
  expect_equal(sum(res_est$.metric == "rsq"), nrow(grid))
  expect_equal(res_est$n, rep(10, nrow(grid) * 2))
  expect_false(identical(num_comp, expr(tune())))
  expect_true(res_workflow$trained)
})

# ------------------------------------------------------------------------------

test_that('tune model only (with recipe)', {
  set.seed(4400)
  wflow <- workflow() %>% add_recipe(rec_no_tune_1) %>% add_model(svm_mod)
  pset <- dials::parameters(wflow)
  grid <- grid_regular(pset, levels = 3)
  folds <- vfold_cv(mtcars)
  control <- control_grid(extract = identity)

  res <- tune_grid(wflow, resamples = folds, grid = grid, control = control)
  res_est <- collect_metrics(res)
  res_workflow <- res$.extracts[[1]]$.extracts[[1]]

  # Ensure tunable parameters in spec are finalized
  cost_quo <- res_workflow$fit$fit$spec$args$cost
  cost <- quo_get_expr(cost_quo)

  expect_equal(res$id, folds$id)
  expect_equal(nrow(res_est), nrow(grid) * 2)
  expect_equal(sum(res_est$.metric == "rmse"), nrow(grid))
  expect_equal(sum(res_est$.metric == "rsq"), nrow(grid))
  expect_equal(res_est$n, rep(10, nrow(grid) * 2))
  expect_false(identical(cost, expr(tune())))
  expect_true(res_workflow$trained)
})

# ------------------------------------------------------------------------------

test_that('tune model only (with variables)', {
  set.seed(4400)

  wflow <- workflow() %>%
    add_variables(mpg, everything()) %>%
    add_model(svm_mod)

  pset <- dials::parameters(wflow)
  grid <- grid_regular(pset, levels = 3)

  folds <- vfold_cv(mtcars)

  res <- tune_grid(wflow, resamples = folds, grid = grid)

  expect_equal(res$id, folds$id)

  res_est <- collect_metrics(res)

  expect_equal(nrow(res_est), nrow(grid) * 2)
  expect_equal(sum(res_est$.metric == "rmse"), nrow(grid))
  expect_equal(sum(res_est$.metric == "rsq"), nrow(grid))
  expect_equal(res_est$n, rep(10, nrow(grid) * 2))
})

# ------------------------------------------------------------------------------

test_that('tune model only (with recipe, multi-predict)', {

  set.seed(4400)
  wflow <- workflow() %>% add_recipe(rec_no_tune_1) %>% add_model(svm_mod)
  pset <- dials::parameters(wflow)
  grid <- grid_regular(pset, levels = 3)
  folds <- vfold_cv(mtcars)
  res <- tune_grid(wflow, resamples = folds, grid = grid)
  expect_equal(res$id, folds$id)
  expect_equal(
    colnames(res$.metrics[[1]]),
    c("cost", ".metric", ".estimator", ".estimate", ".config")
  )
  res_est <- collect_metrics(res)
  expect_equal(nrow(res_est), nrow(grid) * 2)
  expect_equal(sum(res_est$.metric == "rmse"), nrow(grid))
  expect_equal(sum(res_est$.metric == "rsq"), nrow(grid))
  expect_equal(res_est$n, rep(10, nrow(grid) * 2))
})

# ------------------------------------------------------------------------------

test_that('tune model and recipe', {
  set.seed(4400)
  wflow <- workflow() %>% add_recipe(rec_tune_1) %>% add_model(svm_mod)
  pset <- dials::parameters(wflow) %>% update(num_comp = num_comp(c(1, 3)))
  grid <- grid_regular(pset, levels = 3)
  folds <- vfold_cv(mtcars)
  control <- control_grid(extract = identity)

  res <- tune_grid(wflow, resamples = folds, grid = grid, control = control)
  res_est <- collect_metrics(res)
  res_workflow <- res$.extracts[[1]]$.extracts[[1]]

  # Ensure tunable parameters in spec are finalized
  cost_quo <- res_workflow$fit$fit$spec$args$cost
  cost <- quo_get_expr(cost_quo)

  # Ensure tunable parameters in recipe are finalized
  num_comp <- res_workflow$pre$actions$recipe$recipe$steps[[2]]$num_comp

  expect_equal(res$id, folds$id)
  expect_equal(
    colnames(res$.metrics[[1]]),
    c("cost", "num_comp", ".metric", ".estimator", ".estimate", ".config")
  )
  expect_equal(nrow(res_est), nrow(grid) * 2)
  expect_equal(sum(res_est$.metric == "rmse"), nrow(grid))
  expect_equal(sum(res_est$.metric == "rsq"), nrow(grid))
  expect_equal(res_est$n, rep(10, nrow(grid) * 2))
  expect_false(identical(cost, expr(tune())))
  expect_false(identical(num_comp, expr(tune())))
  expect_true(res_workflow$trained)
})

# ------------------------------------------------------------------------------

test_that('tune model and recipe (multi-predict)', {

  set.seed(4400)
  wflow <- workflow() %>% add_recipe(rec_tune_1) %>% add_model(svm_mod)
  pset <- dials::parameters(wflow) %>% update(num_comp = num_comp(c(2, 3)))
  grid <- grid_regular(pset, levels = c(3, 2))
  folds <- vfold_cv(mtcars)
  res <- tune_grid(wflow, resamples = folds, grid = grid)
  expect_equal(res$id, folds$id)
  res_est <- collect_metrics(res)
  expect_equal(nrow(res_est), nrow(grid) * 2)
  expect_equal(sum(res_est$.metric == "rmse"), nrow(grid))
  expect_equal(sum(res_est$.metric == "rsq"), nrow(grid))
  expect_equal(res_est$n, rep(10, nrow(grid) * 2))
})

# ------------------------------------------------------------------------------

test_that('tune model and recipe (parallel_over = "everything")', {
  set.seed(4400)
  wflow <- workflow() %>% add_recipe(rec_tune_1) %>% add_model(svm_mod)
  pset <- dials::parameters(wflow) %>% update(num_comp = num_comp(c(1, 3)))
  grid <- grid_regular(pset, levels = 3)
  folds <- vfold_cv(mtcars)
  control <- control_grid(extract = identity, parallel_over = "everything")

  res <- tune_grid(wflow, resamples = folds, grid = grid, control = control)
  res_est <- collect_metrics(res)

  expect_equal(res$id, folds$id)
  expect_equal(
    colnames(res$.metrics[[1]]),
    c("cost", "num_comp", ".metric", ".estimator", ".estimate", ".config")
  )
  expect_equal(nrow(res_est), nrow(grid) * 2)
  expect_equal(sum(res_est$.metric == "rmse"), nrow(grid))
  expect_equal(sum(res_est$.metric == "rsq"), nrow(grid))
  expect_equal(res_est$n, rep(10, nrow(grid) * 2))
})

# ------------------------------------------------------------------------------

test_that("tune recipe only - failure in recipe is caught elegantly", {

  set.seed(7898)
  data_folds <- vfold_cv(mtcars, v = 2)

  rec <- recipe(mpg ~ ., data = mtcars) %>%
    step_bs(disp, deg_free = tune())

  model <- linear_reg(mode = "regression") %>%
    set_engine("lm")

  # NA values not allowed in recipe
  cars_grid <- tibble(deg_free = c(3, NA_real_, 4))

  # ask for predictions and extractions
  control <- control_grid(
    save_pred = TRUE,
    extract = function(x) 1L
  )

  cars_res <- tune_grid(
    model,
    preprocessor = rec,
    resamples = data_folds,
    grid = cars_grid,
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

  cars_grid <- tibble(cost = c(0.01, 0.02))

  expect_warning(
    cars_res <- tune_grid(
      svm_mod,
      preprocessor = rec,
      resamples = data_folds,
      grid = cars_grid,
      control = control_grid(extract = function(x) {1}, save_pred = TRUE)
    ),
    "All models failed"
  )

  notes <- cars_res$.notes
  note <- notes[[1]]$.notes

  extracts <- cars_res$.extracts
  predictions <- cars_res$.predictions

  expect_length(notes, 2L)

  # recipe failed - no models run
  expect_equivalent(extracts, list(NULL, NULL))
  expect_equivalent(predictions, list(NULL, NULL))
})

test_that("tune model only - failure in formula is caught elegantly", {

  set.seed(7898)
  data_folds <- vfold_cv(mtcars, v = 2)

  cars_grid <- tibble(cost = 0.01)

  # these terms don't exist!
  expect_warning(
    cars_res <- tune_grid(
      svm_mod,
      y ~ z,
      resamples = data_folds,
      grid = cars_grid,
      control = control_grid(extract = function(x) {1}, save_pred = TRUE)
    ),
    "All models failed"
  )

  notes <- cars_res$.notes
  note <- notes[[1]]$.notes

  extracts <- cars_res$.extracts
  predictions <- cars_res$.predictions

  expect_length(notes, 2L)

  # formula failed - no models run
  expect_equivalent(extracts, list(NULL, NULL))
  expect_equivalent(predictions, list(NULL, NULL))
})

test_that("tune model and recipe - failure in recipe is caught elegantly", {

  set.seed(7898)
  data_folds <- vfold_cv(mtcars, v = 2)

  rec <- recipe(mpg ~ ., data = mtcars) %>%
    step_bs(disp, deg_free = tune())


  # NA values not allowed in recipe
  cars_grid <- tibble(deg_free = c(NA_real_, 10L), cost = 0.01)

  cars_res <- tune_grid(
    svm_mod,
    preprocessor = rec,
    resamples = data_folds,
    grid = cars_grid,
    control = control_grid(extract = function(x) {1}, save_pred = TRUE)
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

test_that("argument order gives errors for recipes", {
  expect_error(
    tune_grid(rec_tune_1, lm_mod, vfold_cv(mtcars, v = 2)),
    "should be either a model or workflow"
  )
})

test_that("argument order gives errors for formula", {
  expect_error(
    tune_grid(mpg ~ ., lm_mod, vfold_cv(mtcars, v = 2)),
    "should be either a model or workflow"
  )
})

test_that("ellipses with tune_grid", {

  wflow <- workflow() %>% add_recipe(rec_tune_1) %>% add_model(lm_mod)
  folds <- vfold_cv(mtcars)
  expect_warning(
    tune_grid(wflow, resamples = folds, grid = 3, something = "wrong"),
    "The `...` are not used in this function but one or more objects"
  )
})


test_that("determining the grid type", {
  grid_1 <- expand.grid(a = 1:100, b = letters[1:2])
  expect_true(tune:::is_regular_grid(grid_1))
  expect_true(tune:::is_regular_grid(grid_1[-(1:10),]))
  expect_false(tune:::is_regular_grid(grid_1[-(1:100),]))
  set.seed(1932)
  grid_2 <- data.frame(a = runif(length(letters)), b = letters)
  expect_false(tune:::is_regular_grid(grid_2))
})



test_that("retain extra attributes", {

  set.seed(4400)
  wflow <- workflow() %>% add_recipe(rec_no_tune_1) %>% add_model(svm_mod)
  pset <- dials::parameters(wflow)
  grid <- grid_regular(pset, levels = 3)
  folds <- vfold_cv(mtcars)
  res <- tune_grid(wflow, resamples = folds, grid = grid)

  att <- attributes(res)
  att_names <- names(att)
  expect_true(any(att_names == "metrics"))
  expect_true(any(att_names == "outcomes"))
  expect_true(any(att_names == "parameters"))

  expect_true(is.character(att$outcomes))
  expect_true(att$outcomes == "mpg")
  expect_true(inherits(att$parameters, "parameters"))
  expect_true(inherits(att$metrics, "metric_set"))

  set.seed(4400)
  wflow <- workflow() %>% add_formula(mpg ~ .) %>% add_model(svm_mod)
  pset <- dials::parameters(wflow)
  grid <- grid_regular(pset, levels = 3)
  folds <- vfold_cv(mtcars)
  res <- tune_grid(wflow, resamples = folds, grid = grid)

  att <- attributes(res)
  att_names <- names(att)
  expect_true(any(att_names == "metrics"))
  expect_true(any(att_names == "outcomes"))
  expect_true(any(att_names == "parameters"))

  expect_true(is.character(att$outcomes))
  expect_true(att$outcomes == "mpg")
  expect_true(inherits(att$parameters, "parameters"))
  expect_true(inherits(att$metrics, "metric_set"))

  res2 <- tune_grid(wflow, resamples = folds, grid = grid,
                    control = control_grid(save_workflow = TRUE))
  expect_null(attr(res, "workflow"))
  expect_true(inherits(attr(res2, "workflow"), "workflow"))

  wflow2 <- workflow() %>%
    add_recipe(recipes::recipe(mpg ~ ., mtcars)) %>%
    add_model(svm_mod)
  pset2 <- dials::parameters(wflow2)
  grid2 <- grid_regular(pset2, levels = 3)

  expect_message(
    tune_grid(wflow2, resamples = folds, grid = grid2,
              control = control_grid(save_workflow = TRUE)),
    "being saved contains a recipe, which is"
  )
})

