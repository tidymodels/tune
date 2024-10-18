test_that("tune recipe only", {
  skip_if_not_installed("kernlab")

  helper_objects <- helper_objects_tune()

  set.seed(4400)
  wflow <- workflow() %>%
    add_recipe(helper_objects$rec_tune_1) %>%
    add_model(helper_objects$lm_mod)
  pset <- extract_parameter_set_dials(wflow) %>%
    update(num_comp = dials::num_comp(c(1, 3)))
  grid <- dials::grid_regular(pset, levels = 3)
  folds <- rsample::vfold_cv(mtcars)
  control <- control_grid(extract = identity)

  res <- tune_grid(wflow, resamples = folds, grid = grid, control = control)
  res_est <- collect_metrics(res)
  res_workflow <- res$.extracts[[1]]$.extracts[[1]]

  expect_equal(res, .Last.tune.result)

  # Ensure tunable parameters in recipe are finalized
  num_comp <- res_workflow$pre$actions$recipe$recipe$steps[[2]]$num_comp

  expect_equal(res$id, folds$id)
  expect_equal(nrow(res_est), nrow(grid) * 2)
  expect_equal(sum(res_est$.metric == "rmse"), nrow(grid))
  expect_equal(sum(res_est$.metric == "rsq"), nrow(grid))
  expect_equal(res_est$n, rep(10, nrow(grid) * 2))
  expect_false(identical(num_comp, expr(tune())))
  expect_true(res_workflow$trained)
  expect_null(.get_tune_eval_times(res))
  expect_null(.get_tune_eval_time_target(res))
})

# ------------------------------------------------------------------------------

test_that("tune model only (with recipe)", {
  skip_if_not_installed("kernlab")

  helper_objects <- helper_objects_tune()

  set.seed(4400)
  wflow <- workflow() %>%
    add_recipe(helper_objects$rec_no_tune_1) %>%
    add_model(helper_objects$svm_mod)
  pset <- extract_parameter_set_dials(wflow)
  grid <- dials::grid_regular(pset, levels = 3)
  folds <- rsample::vfold_cv(mtcars)
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

test_that("tune model only (with variables)", {
  skip_if_not_installed("kernlab")

  helper_objects <- helper_objects_tune()

  set.seed(4400)

  wflow <- workflow() %>%
    add_variables(mpg, everything()) %>%
    add_model(helper_objects$svm_mod)

  pset <- extract_parameter_set_dials(wflow)
  grid <- dials::grid_regular(pset, levels = 3)

  folds <- rsample::vfold_cv(mtcars)

  res <- tune_grid(wflow, resamples = folds, grid = grid)

  expect_equal(res$id, folds$id)

  res_est <- collect_metrics(res)

  expect_equal(nrow(res_est), nrow(grid) * 2)
  expect_equal(sum(res_est$.metric == "rmse"), nrow(grid))
  expect_equal(sum(res_est$.metric == "rsq"), nrow(grid))
  expect_equal(res_est$n, rep(10, nrow(grid) * 2))
})

# ------------------------------------------------------------------------------

test_that("tune model only (with recipe, multi-predict)", {
  skip_if_not_installed("kernlab")

  helper_objects <- helper_objects_tune()

  set.seed(4400)
  wflow <- workflow() %>%
    add_recipe(helper_objects$rec_no_tune_1) %>%
    add_model(helper_objects$svm_mod)
  pset <- extract_parameter_set_dials(wflow)
  grid <- dials::grid_regular(pset, levels = 3)
  folds <- rsample::vfold_cv(mtcars)
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

test_that("tune model only (without recipe, multi-predict. #695)", {
  skip_on_cran()
  skip_if_not_installed("kknn")

  knn_res <-
    tune_grid(
      parsnip::nearest_neighbor(mode = "regression", neighbors = tune("k")),
      mpg ~ .,
      resamples = rsample::bootstraps(mtcars, 5),
      grid = 4
    )

  expect_equal(nrow(knn_res$.notes[[1]]), 0)
})

# ------------------------------------------------------------------------------

test_that("tune model only (fairness - include `by` variable as predictor)", {
  skip_on_cran()
  skip_if_not_installed("yardstick", minimum_version = "1.2.0.9001")
  skip_if_not_installed("kknn")

  knn <- parsnip::nearest_neighbor("classification", "kknn", neighbors = tune())
  mtcars_fair <- mtcars
  mtcars_fair$vs <- as.factor(mtcars_fair$vs)
  mtcars_fair$cyl <- as.factor(mtcars_fair$cyl)
  mtcars_fair$am <- as.factor(mtcars_fair$am)
  set.seed(4400)
  boots <- rsample::bootstraps(mtcars_fair, 3)
  n_grid <- 3

  set.seed(1)
  res <- tune_grid(
    knn, vs ~ mpg + hp + cyl, resamples = boots, grid = n_grid,
    metrics =
      yardstick::metric_set(
        yardstick::roc_auc,
        yardstick::demographic_parity(cyl)
      )
  )

  expect_equal(
    colnames(res$.metrics[[1]]),
    c("neighbors", ".metric", ".by", ".estimator", ".estimate", ".config")
  )
  res_est <- collect_metrics(res)
  expect_equal(
    colnames(res_est),
    c("neighbors", ".metric", ".estimator", ".by", "mean", "n", "std_err", ".config")
  )
  expect_equal(nrow(res_est), n_grid * 2)
  expect_equal(sum(res_est$.by == "cyl", na.rm = TRUE), n_grid)
  expect_equal(sum(is.na(res_est$.by)), n_grid)
  expect_equal(sum(res_est$.metric == "roc_auc"), n_grid)
  expect_equal(sum(res_est$.metric == "demographic_parity"), n_grid)
  expect_equal(res_est$n, rep(3, n_grid * 2))
})

test_that("tune model only (fairness - don't include `by` variable as predictor)", {
  skip_on_cran()
  skip_if_not_installed("yardstick", minimum_version = "1.2.0.9001")
  skip_if_not_installed("kknn")

  knn <- parsnip::nearest_neighbor("classification", "kknn", neighbors = tune())
  mtcars_fair <- mtcars
  mtcars_fair$vs <- as.factor(mtcars_fair$vs)
  mtcars_fair$cyl <- as.factor(mtcars_fair$cyl)
  mtcars_fair$am <- as.factor(mtcars_fair$am)
  set.seed(4400)
  boots <- rsample::bootstraps(mtcars_fair, 3)
  n_grid <- 3

  set.seed(1)
  res2 <- tune_grid(
    knn, vs ~ mpg + hp, resamples = boots, grid = n_grid,
    metrics =
      yardstick::metric_set(
        yardstick::roc_auc,
        yardstick::demographic_parity(cyl)
      )
  )

  expect_equal(
    colnames(res2$.metrics[[1]]),
    c("neighbors", ".metric", ".by", ".estimator", ".estimate", ".config")
  )
  res_est2 <- collect_metrics(res2)
  expect_equal(
    colnames(res_est2),
    c("neighbors", ".metric", ".estimator", ".by", "mean", "n", "std_err", ".config")
  )
  expect_equal(nrow(res_est2), n_grid * 2)
  expect_equal(sum(res_est2$.by == "cyl", na.rm = TRUE), n_grid)
  expect_equal(sum(is.na(res_est2$.by)), n_grid)
  expect_equal(sum(res_est2$.metric == "roc_auc"), n_grid)
  expect_equal(sum(res_est2$.metric == "demographic_parity"), n_grid)
  expect_equal(res_est2$n, rep(3, n_grid * 2))
})

test_that("tune model only (fairness metrics - evaluate across multiple `by`)", {
  skip_on_cran()
  skip_if_not_installed("yardstick", minimum_version = "1.2.0.9001")
  skip_if_not_installed("kknn")

  knn <- parsnip::nearest_neighbor("classification", "kknn", neighbors = tune())
  mtcars_fair <- mtcars
  mtcars_fair$vs <- as.factor(mtcars_fair$vs)
  mtcars_fair$cyl <- as.factor(mtcars_fair$cyl)
  mtcars_fair$am <- as.factor(mtcars_fair$am)
  set.seed(4400)
  boots <- rsample::bootstraps(mtcars_fair, 3)
  n_grid <- 3

  set.seed(1)
  res3 <- tune_grid(
    knn, vs ~ mpg + hp, resamples = boots, grid = n_grid,
    metrics =
      yardstick::metric_set(
        yardstick::roc_auc,
        yardstick::demographic_parity(cyl),
        yardstick::equal_opportunity(am)
      )
  )

  expect_equal(
    colnames(res3$.metrics[[1]]),
    c("neighbors", ".metric", ".by", ".estimator", ".estimate", ".config")
  )
  res_est3 <- collect_metrics(res3)
  expect_equal(
    colnames(res_est3),
    c("neighbors", ".metric", ".estimator", ".by", "mean", "n", "std_err", ".config")
  )
  expect_equal(nrow(res_est3), n_grid * 3)
  expect_equal(sum(res_est3$.by == "cyl", na.rm = TRUE), n_grid)
  expect_equal(sum(is.na(res_est3$.by)), n_grid)
  expect_equal(sum(res_est3$.metric == "roc_auc"), n_grid)
  expect_equal(sum(res_est3$.metric == "demographic_parity"), n_grid)
  expect_equal(res_est3$n, rep(3, n_grid * 3))
})

test_that("tune model only (fairness - evaluate across multiple `by`, same metric)", {
  skip_on_cran()
  skip_if_not_installed("yardstick", minimum_version = "1.2.0.9001")
  skip_if_not_installed("kknn")

  knn <- parsnip::nearest_neighbor("classification", "kknn", neighbors = tune())
  mtcars_fair <- mtcars
  mtcars_fair$vs <- as.factor(mtcars_fair$vs)
  mtcars_fair$cyl <- as.factor(mtcars_fair$cyl)
  mtcars_fair$am <- as.factor(mtcars_fair$am)
  set.seed(4400)
  boots <- rsample::bootstraps(mtcars_fair, 3)
  n_grid <- 3

  set.seed(1)
  res4 <- tune_grid(
    knn, vs ~ mpg + hp, resamples = boots, grid = n_grid,
    metrics =
      yardstick::metric_set(
        yardstick::roc_auc,
        yardstick::demographic_parity(cyl),
        yardstick::demographic_parity(am)
      )
  )

  expect_equal(
    colnames(res4$.metrics[[1]]),
    c("neighbors", ".metric", ".by", ".estimator", ".estimate", ".config")
  )
  res_est4 <- collect_metrics(res4)
  expect_equal(
    colnames(res_est4),
    c("neighbors", ".metric", ".estimator", ".by", "mean", "n", "std_err", ".config")
  )
  expect_equal(nrow(res_est4), n_grid * 3)
  expect_equal(sum(res_est4$.by == "cyl", na.rm = TRUE), n_grid)
  expect_equal(sum(res_est4$.by == "am", na.rm = TRUE), n_grid)
  expect_equal(sum(is.na(res_est4$.by)), n_grid)
  expect_equal(sum(res_est4$.metric == "roc_auc"), n_grid)
  expect_equal(sum(res_est4$.metric == "demographic_parity"), n_grid * 2)
  expect_equal(res_est4$n, rep(3, n_grid * 3))
})

test_that("tune model only (fairness - evaluate only fairness metrics)", {
  skip_on_cran()
  skip_if_not_installed("yardstick", minimum_version = "1.2.0.9001")
  skip_if_not_installed("kknn")

  knn <- parsnip::nearest_neighbor("classification", "kknn", neighbors = tune())
  mtcars_fair <- mtcars
  mtcars_fair$vs <- as.factor(mtcars_fair$vs)
  mtcars_fair$cyl <- as.factor(mtcars_fair$cyl)
  mtcars_fair$am <- as.factor(mtcars_fair$am)
  set.seed(4400)
  boots <- rsample::bootstraps(mtcars_fair, 3)
  n_grid <- 3

  set.seed(1)
  res5 <- tune_grid(
    knn, vs ~ mpg + hp, resamples = boots, grid = n_grid,
    metrics =
      yardstick::metric_set(
        yardstick::demographic_parity(cyl)
      )
  )

  expect_equal(
    colnames(res5$.metrics[[1]]),
    c("neighbors", ".metric", ".by", ".estimator", ".estimate", ".config")
  )
  res_est5 <- collect_metrics(res5)
  expect_equal(
    colnames(res_est5),
    c("neighbors", ".metric", ".estimator", ".by", "mean", "n", "std_err", ".config")
  )
  expect_equal(nrow(res_est5), n_grid)
  expect_equal(sum(res_est5$.by == "cyl", na.rm = TRUE), n_grid)
  expect_equal(sum(is.na(res_est5$.by)), 0)
  expect_equal(sum(res_est5$.metric == "demographic_parity"), n_grid)
  expect_equal(res_est5$n, rep(3, n_grid))
})

# ------------------------------------------------------------------------------

test_that("tune model and recipe", {
  skip_if_not_installed("kernlab")

  helper_objects <- helper_objects_tune()

  set.seed(4400)
  wflow <- workflow() %>%
    add_recipe(helper_objects$rec_tune_1) %>%
    add_model(helper_objects$svm_mod)
  pset <- extract_parameter_set_dials(wflow) %>%
    update(num_comp = dials::num_comp(c(1, 3)))
  grid <- dials::grid_regular(pset, levels = 3)
  folds <- rsample::vfold_cv(mtcars)
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

test_that("tune model and recipe (multi-predict)", {
  skip_if_not_installed("kernlab")

  helper_objects <- helper_objects_tune()

  set.seed(4400)
  wflow <- workflow() %>%
    add_recipe(helper_objects$rec_tune_1) %>%
    add_model(helper_objects$svm_mod)
  pset <- extract_parameter_set_dials(wflow) %>%
    update(num_comp = dials::num_comp(c(2, 3)))
  grid <- dials::grid_regular(pset, levels = c(3, 2))
  folds <- rsample::vfold_cv(mtcars)
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
  skip_if_not_installed("kernlab")

  helper_objects <- helper_objects_tune()

  set.seed(4400)
  wflow <- workflow() %>%
    add_recipe(helper_objects$rec_tune_1) %>%
    add_model(helper_objects$svm_mod)
  pset <- extract_parameter_set_dials(wflow) %>%
    update(num_comp = dials::num_comp(c(1, 3)))
  grid <- dials::grid_regular(pset, levels = 3)
  folds <- rsample::vfold_cv(mtcars)
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
  skip_if_not_installed("splines2")
  skip_if_not_installed("kernlab")

  helper_objects <- helper_objects_tune()

  set.seed(7898)
  data_folds <- rsample::vfold_cv(mtcars, v = 2)

  rec <- recipe(mpg ~ ., data = mtcars) %>%
    step_spline_b(disp, deg_free = tune())

  model <- linear_reg(mode = "regression") %>%
    set_engine("lm")

  # NA values not allowed in recipe
  cars_grid <- tibble(deg_free = c(3, NA_real_, 4))

  # ask for predictions and extractions
  control <- control_grid(
    save_pred = TRUE,
    extract = function(x) 1L
  )

  suppressMessages({
    cars_res <- tune_grid(
      model,
      preprocessor = rec,
      resamples = data_folds,
      grid = cars_grid,
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

  helper_objects <- helper_objects_tune()

  set.seed(7898)
  data_folds <- rsample::vfold_cv(mtcars, v = 2)

  # NA values not allowed in recipe
  rec <- recipe(mpg ~ ., data = mtcars) %>%
    step_spline_b(disp, deg_free = NA_real_)

  cars_grid <- tibble(cost = c(0.01, 0.02))

  expect_snapshot(
    cars_res <- tune_grid(
      helper_objects$svm_mod,
      preprocessor = rec,
      resamples = data_folds,
      grid = cars_grid,
      control = control_grid(extract = function(x) {1}, save_pred = TRUE)
    )
  )

  notes <- cars_res$.notes
  note <- notes[[1]]$note

  extracts <- cars_res$.extracts
  predictions <- cars_res$.predictions

  expect_length(notes, 2L)

  # recipe failed - no models run
  expect_equal(extracts, list(NULL, NULL))
  expect_equal(predictions, list(NULL, NULL))
})

test_that("tune model only - failure in formula is caught elegantly", {
  skip_if_not_installed("kernlab")

  helper_objects <- helper_objects_tune()

  set.seed(7898)
  data_folds <- rsample::vfold_cv(mtcars, v = 2)

  cars_grid <- tibble(cost = 0.01)

  # these terms don't exist!
  expect_snapshot(
    cars_res <- tune_grid(
      helper_objects$svm_mod,
      y ~ z,
      resamples = data_folds,
      grid = cars_grid,
      control = control_grid(extract = function(x) {1}, save_pred = TRUE)
    )
  )

  notes <- cars_res$.notes
  note <- notes[[1]]$note

  extracts <- cars_res$.extracts
  predictions <- cars_res$.predictions

  expect_length(notes, 2L)

  # formula failed - no models run
  expect_equal(extracts, list(NULL, NULL))
  expect_equal(predictions, list(NULL, NULL))
})

test_that("tune model and recipe - failure in recipe is caught elegantly", {
  skip_if_not_installed("splines2")
  skip_if_not_installed("kernlab")

  helper_objects <- helper_objects_tune()

  set.seed(7898)
  data_folds <- rsample::vfold_cv(mtcars, v = 2)

  rec <- recipe(mpg ~ ., data = mtcars) %>%
    step_spline_b(disp, deg_free = tune())


  # NA values not allowed in recipe
  cars_grid <- tibble(deg_free = c(NA_real_, 10L), cost = 0.01)

  suppressMessages({
    cars_res <- tune_grid(
      helper_objects$svm_mod,
      preprocessor = rec,
      resamples = data_folds,
      grid = cars_grid,
      control = control_grid(extract = function(x) {1}, save_pred = TRUE)
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

test_that("argument order gives errors for recipes", {
  skip_if_not_installed("kernlab")

  helper_objects <- helper_objects_tune()

  expect_snapshot(error = TRUE, {
    tune_grid(
      helper_objects$rec_tune_1,
      helper_objects$lm_mod,
      rsample::vfold_cv(mtcars, v = 2)
    )
  })
})

test_that("argument order gives errors for formula", {
  skip_if_not_installed("kernlab")

  helper_objects <- helper_objects_tune()

  expect_snapshot(error = TRUE, {
    tune_grid(mpg ~ ., helper_objects$lm_mod, rsample::vfold_cv(mtcars, v = 2))
  })
})

test_that("ellipses with tune_grid", {
  skip_if_not_installed("kernlab")

  helper_objects <- helper_objects_tune()

  wflow <- workflow() %>%
    add_recipe(helper_objects$rec_tune_1) %>%
    add_model(helper_objects$lm_mod)
  folds <- rsample::vfold_cv(mtcars)
  expect_snapshot(
    tune_grid(wflow, resamples = folds, grid = 3, something = "wrong")
  )
})


test_that("determining the grid type", {
  grid_1 <- expand.grid(a = 1:100, b = letters[1:2])
  expect_true(tune:::is_regular_grid(grid_1))
  expect_true(tune:::is_regular_grid(grid_1[-(1:10), ]))
  expect_false(tune:::is_regular_grid(grid_1[-(1:100), ]))
  set.seed(1932)
  grid_2 <- data.frame(a = runif(length(letters)), b = letters)
  expect_false(tune:::is_regular_grid(grid_2))
})



test_that("retain extra attributes", {
  skip_if_not_installed("kernlab")

  helper_objects <- helper_objects_tune()

  set.seed(4400)
  wflow <- workflow() %>%
    add_recipe(helper_objects$rec_no_tune_1) %>%
    add_model(helper_objects$svm_mod)
  pset <- extract_parameter_set_dials(wflow)
  grid <- dials::grid_regular(pset, levels = 3)
  folds <- rsample::vfold_cv(mtcars)
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
  wflow <- workflow() %>%
    add_formula(mpg ~ .) %>%
    add_model(helper_objects$svm_mod)
  pset <- extract_parameter_set_dials(wflow)
  grid <- dials::grid_regular(pset, levels = 3)
  folds <- rsample::vfold_cv(mtcars)
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

  res2 <- tune_grid(
    wflow,
    resamples = folds,
    grid = grid,
    control = control_grid(save_workflow = TRUE)
  )
  expect_null(attr(res, "workflow"))
  expect_true(inherits(attr(res2, "workflow"), "workflow"))

  wflow2 <- workflow() %>%
    add_recipe(recipes::recipe(mpg ~ ., mtcars[rep(1:32, 3000), ])) %>%
    add_model(helper_objects$svm_mod)
  pset2 <- extract_parameter_set_dials(wflow2)
  grid2 <- dials::grid_regular(pset2, levels = 3)

  expect_message(
    tune_grid(
      wflow2,
      resamples = folds,
      grid = grid2,
      control = control_grid(save_workflow = TRUE)
    ),
    "being saved contains a recipe, which is"
  )
})


