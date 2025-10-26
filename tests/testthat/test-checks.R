test_that("rsample objects", {
  obj_cv <- rsample::vfold_cv(mtcars)
  obj_loo <- rsample::loo_cv(mtcars)
  obj_nst <- rsample::nested_cv(mtcars, obj_cv, inside = rsample::bootstraps())
  obj_permut <- rsample::permutations(mtcars, hp)
  expect_no_error(tune:::check_rset(obj_cv))
  expect_snapshot(error = TRUE, tune:::check_rset(obj_loo))
  expect_snapshot(error = TRUE, tune:::check_rset(obj_nst))
  expect_snapshot(error = TRUE, tune:::check_rset(obj_permut))
})

# ------------------------------------------------------------------------------

test_that("grid objects", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("splines2")
  skip_if_not_installed("kernlab")
  data("Chicago", package = "modeldata")

  spline_rec <-
    recipes::recipe(ridership ~ ., data = head(Chicago)) |>
    recipes::step_date(date) |>
    recipes::step_holiday(date) |>
    recipes::step_rm(date, dplyr::ends_with("away")) |>
    recipes::step_impute_knn(
      recipes::all_predictors(),
      neighbors = tune("imputation")
    ) |>
    recipes::step_other(recipes::all_nominal(), threshold = tune()) |>
    recipes::step_dummy(recipes::all_nominal()) |>
    recipes::step_normalize(recipes::all_numeric_predictors()) |>
    recipes::step_spline_b(
      recipes::all_predictors(),
      deg_free = tune(),
      degree = tune()
    )

  glmn <- parsnip::linear_reg(penalty = tune(), mixture = tune()) |>
    parsnip::set_engine("glmnet")

  chi_wflow <-
    workflows::workflow() |>
    workflows::add_recipe(spline_rec) |>
    workflows::add_model(glmn)

  grid_1 <- tibble::tibble(
    penalty = 1:10,
    mixture = 1:10,
    imputation = 1:10,
    threshold = 1:10,
    deg_free = 1:10,
    degree = 1:10
  )

  set_1 <- extract_parameter_set_dials(chi_wflow)
  set_2 <- set_1 |> update(deg_free = dials::deg_free(c(1, 3)))

  expect_equal(tune:::check_grid(grid_1, chi_wflow), grid_1)

  expect_silent(tune:::check_grid(grid_1, chi_wflow))

  expect_snapshot(
    tune:::check_grid(rbind(grid_1, grid_1), chi_wflow)
  )

  expect_snapshot(error = TRUE, {
    tune:::check_grid(chi_wflow, chi_wflow)
  })

  bare_rec <-
    recipes::recipe(ridership ~ ., data = head(Chicago))

  svm_mod <-
    parsnip::svm_rbf(cost = tune()) |>
    parsnip::set_engine("kernlab") |>
    parsnip::set_mode("regression")

  wflow_1 <-
    workflow() |>
    add_model(svm_mod) |>
    add_recipe(bare_rec)

  expect_no_error(grid_2 <- tune:::check_grid(6, wflow_1))
  expect_equal(nrow(grid_2), 6)
  expect_true(inherits(grid_2, "data.frame"))

  # For issue #56
  grid_3 <- as.data.frame(grid_1)
  expect_equal(tune:::check_grid(grid_3, chi_wflow), grid_1)

  # For weird attributes
  grid_4 <- expand.grid(
    penalty = 1:10,
    mixture = 12,
    imputation = 1:2,
    threshold = 1:2,
    deg_free = 2:3,
    degree = 9:10
  )
  expect_equal(
    tune:::check_grid(grid_4, chi_wflow),
    tibble::as_tibble(vctrs::data_frame(grid_4))
  )

  expect_silent(tune:::check_grid(grid_4, chi_wflow))
})

test_that("Unknown `grid` columns are caught", {
  skip_if_not_installed("splines2")

  data <- data.frame(x = 1:2, y = 1:2)

  rec <- recipes::recipe(y ~ x, data = data)
  rec <- recipes::step_spline_b(rec, x, deg_free = tune())
  rec <- recipes::step_pca(rec, x, num_comp = tune())

  model <- parsnip::linear_reg()
  model <- parsnip::set_engine(model, "lm")

  workflow <- workflow()
  workflow <- add_recipe(workflow, rec)
  workflow <- add_model(workflow, model)

  grid <- tibble::tibble(deg_free = 2, num_comp = 0.01, other1 = 1, other2 = 1)

  expect_snapshot(error = TRUE, {
    tune:::check_grid(grid, workflow)
  })
})

test_that("Missing required `grid` columns are caught", {
  skip_if_not_installed("splines2")

  data <- data.frame(x = 1:2, y = 1:2)

  rec <- recipes::recipe(y ~ x, data = data)
  rec <- recipes::step_spline_b(rec, x, deg_free = tune())
  rec <- recipes::step_pca(rec, x, num_comp = tune())

  model <- parsnip::linear_reg()
  model <- parsnip::set_engine(model, "lm")

  workflow <- workflow()
  workflow <- add_recipe(workflow, rec)
  workflow <- add_model(workflow, model)

  grid <- tibble::tibble(num_comp = 0.01)

  expect_snapshot(error = TRUE, {
    tune:::check_grid(grid, workflow)
  })
})

# ------------------------------------------------------------------------------

test_that("workflow objects", {
  skip_if_not_installed("xgboost")

  bare_rec <-
    recipes::recipe(ridership ~ ., data = head(Chicago))

  svm_mod <-
    parsnip::svm_rbf(cost = tune()) |>
    parsnip::set_engine("kernlab") |>
    parsnip::set_mode("regression")

  wflow_1 <-
    workflow() |>
    add_model(svm_mod) |>
    add_recipe(bare_rec)

  expect_null(tune:::check_workflow(x = wflow_1))

  wflow_2 <-
    workflow() |>
    add_model(
      parsnip::boost_tree(mtry = tune()) |>
        parsnip::set_engine("xgboost") |>
        parsnip::set_mode("regression")
    ) |>
    add_recipe(bare_rec)

  expect_null(tune:::check_workflow(x = wflow_2))
  expect_snapshot(error = TRUE, {
    tune:::check_workflow(x = wflow_2, check_dials = TRUE)
  })

  glmn <- parsnip::linear_reg(penalty = tune(), mixture = tune()) |>
    parsnip::set_engine("glmnet")

  wflow_3 <-
    workflow() |>
    add_model(glmn)
  expect_snapshot(error = TRUE, {
    tune:::check_workflow(wflow_3)
  })

  wflow_4 <-
    workflow() |>
    add_recipe(bare_rec)
  expect_snapshot(error = TRUE, {
    tune:::check_workflow(wflow_4)
  })
})

test_that("errors informatively when needed package isn't installed", {
  # rstanarm is not installed during CI runs
  # in contexts where it _is_ installed, skip the test.
  skip_if(rlang::is_installed("rstanarm"))
  stan_wflow <- workflow(mpg ~ ., parsnip::linear_reg(engine = "stan"))

  expect_snapshot(
    check_workflow(stan_wflow),
    error = TRUE
  )

  expect_snapshot(
    fit_resamples(stan_wflow, rsample::bootstraps(mtcars)),
    error = TRUE
  )
})

test_that("workflow objects (will not tune, tidymodels/tune#548)", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("splines2")

  # one recipe without tuning, one with:
  rec_bare <- recipes::recipe(ridership ~ ., data = head(Chicago, 30))
  rec_tune <- rec_bare |>
    recipes::step_spline_natural(temp_max, deg_free = tune())

  # well-defined:
  lr_lm_0 <- parsnip::linear_reg()

  # not well-defined:
  lr_lm_1 <- parsnip::linear_reg(penalty = tune())
  lr_lm_2 <- parsnip::linear_reg(penalty = tune(), mixture = tune())

  # well-defined:
  lr_glmnet_0 <- lr_lm_0 |> parsnip::set_engine("glmnet")
  lr_glmnet_1 <- lr_lm_1 |> parsnip::set_engine("glmnet")
  lr_glmnet_2 <- lr_lm_2 |> parsnip::set_engine("glmnet")

  # don't error when supplied tune args make sense given engine / steps
  expect_no_error(check_workflow(workflow(rec_bare, lr_lm_0)))
  expect_no_error(check_workflow(workflow(rec_bare, lr_glmnet_0)))
  expect_no_error(check_workflow(workflow(rec_bare, lr_glmnet_1)))
  expect_no_error(check_workflow(workflow(rec_bare, lr_glmnet_2)))

  expect_no_error(check_workflow(workflow(rec_tune, lr_lm_0)))
  expect_no_error(check_workflow(workflow(rec_tune, lr_glmnet_0)))
  expect_no_error(check_workflow(workflow(rec_tune, lr_glmnet_1)))
  expect_no_error(check_workflow(workflow(rec_tune, lr_glmnet_2)))

  # error when supplied tune args don't make sense given engine / steps
  expect_error_nt <- function(x) {
    testthat::expect_error(x, class = "not_tunable_error")
  }

  expect_error_nt(check_workflow(workflow(rec_bare, lr_lm_1)))
  expect_error_nt(check_workflow(workflow(rec_bare, lr_lm_2)))

  expect_error_nt(check_workflow(workflow(rec_tune, lr_lm_1)))
  expect_error_nt(check_workflow(workflow(rec_tune, lr_lm_2)))

  # ensure that error (incl `call`) is displayed as it ought to be:
  expect_snapshot(
    error = TRUE,
    tune_grid(
      lr_lm_1,
      rec_bare,
      rsample::bootstraps(Chicago, 2)
    )
  )
  expect_snapshot(
    error = TRUE,
    tune_bayes(
      lr_lm_2,
      rec_tune,
      rsample::bootstraps(Chicago, 2)
    )
  )
})

# ------------------------------------------------------------------------------

test_that("yardstick objects", {
  skip_if_not_installed("splines2")

  spline_rec <-
    recipes::recipe(ridership ~ ., data = head(Chicago)) |>
    recipes::step_date(date) |>
    recipes::step_holiday(date) |>
    recipes::step_rm(date, dplyr::ends_with("away")) |>
    recipes::step_impute_knn(
      recipes::all_predictors(),
      neighbors = tune("imputation")
    ) |>
    recipes::step_other(recipes::all_nominal(), threshold = tune()) |>
    recipes::step_dummy(recipes::all_nominal()) |>
    recipes::step_normalize(recipes::all_numeric_predictors()) |>
    recipes::step_spline_b(
      recipes::all_predictors(),
      deg_free = tune(),
      degree = tune()
    )

  glmn <- parsnip::linear_reg(penalty = tune(), mixture = tune()) |>
    parsnip::set_engine("glmnet")

  chi_wflow <-
    workflows::workflow() |>
    workflows::add_recipe(spline_rec) |>
    workflows::add_model(glmn)

  metrics_1 <- tune:::check_metrics(NULL, chi_wflow)
  metrics_2 <- yardstick::metric_set(yardstick:::rmse)
  expect_true(inherits(metrics_1, "numeric_metric_set"))
  expect_snapshot(error = TRUE, {
    tune:::check_metrics(yardstick::rmse, chi_wflow)
  })
  expect_true(inherits(
    tune:::check_metrics(metrics_2, chi_wflow),
    "numeric_metric_set"
  ))
})

test_that("metrics must match the parsnip engine", {
  metric_set1 <- yardstick::metric_set(yardstick::accuracy)
  metric_set2 <- yardstick::metric_set(yardstick::rmse)

  mod1 <- parsnip::rand_forest(mode = "regression")
  mod2 <- parsnip::rand_forest(mode = "classification")

  workflow1 <- add_model(workflow(), mod1)
  workflow2 <- add_model(workflow(), mod2)

  expect_snapshot(error = TRUE, {
    tune:::check_metrics(metric_set1, workflow1)
  })

  expect_snapshot(error = TRUE, {
    tune:::check_metrics(metric_set2, workflow2)
  })
})

# ------------------------------------------------------------------------------

test_that("grid control objects", {
  expect_no_error(control_grid())
  expect_snapshot(error = TRUE, control_grid(tomato = 1))
  expect_snapshot(error = TRUE, control_grid(verbose = 1))
  expect_snapshot(error = TRUE, control_grid(verbose = rep(TRUE, 2)))
  expect_snapshot(error = TRUE, control_grid(allow_par = 1))
  expect_snapshot(error = TRUE, control_grid(save_pred = "no"))
  expect_snapshot(error = TRUE, control_grid(extract = Inf))
  expect_snapshot(error = TRUE, control_grid(pkgs = Inf))

  expect_no_error(control_grid(verbose = TRUE))
  expect_no_error(control_grid(allow_par = FALSE))
  expect_no_error(control_grid(save_pred = TRUE))
  expect_no_error(control_grid(extract = NULL))
  expect_no_error(control_grid(extract = I))
  expect_no_error(control_grid(pkgs = NULL))
  expect_no_error(control_grid(pkgs = letters))
  expect_s3_class(control_grid(), c("control_grid", "control_resamples"))
})

test_that("Bayes control objects", {
  expect_no_error(control_bayes())
  expect_snapshot(error = TRUE, control_bayes(tomato = 1))
  expect_snapshot(error = TRUE, control_bayes(verbose = 1))
  expect_snapshot(error = TRUE, control_bayes(verbose = rep(TRUE, 2)))
  expect_snapshot(error = TRUE, control_bayes(no_improve = FALSE))
  expect_snapshot(error = TRUE, control_bayes(uncertain = FALSE))
  expect_snapshot(error = TRUE, control_bayes(seed = FALSE))
  expect_snapshot(error = TRUE, control_bayes(save_pred = "no"))
  expect_snapshot(error = TRUE, control_bayes(extract = Inf))
  expect_snapshot(error = TRUE, control_bayes(pkgs = Inf))
  expect_snapshot(error = TRUE, control_bayes(time_limit = "a"))

  expect_snapshot(
    tmp <- control_bayes(no_improve = 2, uncertain = 5)
  )

  expect_no_error(control_bayes(verbose = TRUE))
  expect_no_error(control_bayes(no_improve = 2))
  expect_no_error(control_bayes(uncertain = 2))
  expect_no_error(control_bayes(save_pred = TRUE))
  expect_no_error(control_bayes(extract = NULL))
  expect_no_error(control_bayes(extract = I))
  expect_no_error(control_bayes(pkgs = NULL))
  expect_no_error(control_bayes(pkgs = letters))
  expect_no_error(control_bayes(time_limit = 2))
  expect_s3_class(control_bayes(), "control_bayes")
})

test_that("control object print methods", {
  # Test control_grid print
  expect_snapshot(control_grid())
  expect_snapshot(control_grid(verbose = TRUE, save_pred = TRUE))
  expect_snapshot(control_grid(pkgs = c("pkg1", "pkg2"), extract = I))

  # Test control_bayes print
  set.seed(123)
  expect_snapshot(control_bayes())
  expect_snapshot(control_bayes(verbose_iter = TRUE, no_improve = 5, save_gp_scoring = TRUE))

  # Test control_last_fit print
  expect_snapshot(control_last_fit())
  expect_snapshot(control_last_fit(verbose = TRUE))
})

# ------------------------------------------------------------------------------

test_that("initial values", {
  skip_if_not_installed("kernlab")

  svm_mod <-
    parsnip::svm_rbf(cost = tune()) |>
    parsnip::set_engine("kernlab") |>
    parsnip::set_mode("regression")
  wflow_1 <-
    workflow() |>
    add_model(svm_mod) |>
    add_recipe(recipes::recipe(mpg ~ ., data = mtcars))
  mtfolds <- rsample::vfold_cv(mtcars)

  grid_1 <- tune:::check_initial(
    2,
    pset = extract_parameter_set_dials(wflow_1),
    wflow = wflow_1,
    resamples = mtfolds,
    metrics = yardstick::metric_set(yardstick::rsq),
    eval_time = NULL,
    ctrl = control_bayes()
  )
  expect_true(is.data.frame(grid_1))
  expect_equal(nrow(grid_1), nrow(mtfolds))
  expect_true(all(purrr::map_lgl(grid_1$.metrics, \(.x) nrow(.x) == 2)))

  expect_snapshot(error = TRUE, {
    tune:::check_initial(
      data.frame(),
      pset = extract_parameter_set_dials(wflow_1),
      wflow = wflow_1,
      resamples = mtfolds,
      metrics = yardstick::metric_set(yardstick::rsq),
      ctrl = control_bayes()
    )
  })
})

# ------------------------------------------------------------------------------

test_that("validation helpers", {
  expect_true(tune:::check_class_or_null("a", "character"))
  expect_true(tune:::check_class_or_null(letters, "character"))
  expect_true(tune:::check_class_or_null(NULL, "character"))
  expect_false(tune:::check_class_or_null(NA, "character"))

  expect_true(tune:::check_class_and_single("a", "character"))
  expect_false(tune:::check_class_and_single(letters, "character"))
  expect_false(tune:::check_class_and_single(NULL, "character"))
  expect_false(tune:::check_class_and_single(NA, "character"))
})

# ------------------------------------------------------------------------------

test_that("check parameter finalization", {
  skip_if_not_installed("splines2")

  rec <-
    recipes::recipe(mpg ~ ., data = mtcars) |>
    recipes::step_spline_natural(disp, deg_free = 3)
  rec_tune <- rec |>
    recipes::step_pca(recipes::all_predictors(), num_comp = tune())
  f <- mpg ~ .
  rf1 <-
    parsnip::rand_forest(mtry = tune(), min_n = tune()) |>
    parsnip::set_engine("ranger") |>
    parsnip::set_mode("regression")
  lm1 <-
    parsnip::linear_reg(penalty = tune()) |>
    parsnip::set_engine("glmnet")

  w1 <-
    workflow() |>
    add_formula(f) |>
    add_model(rf1)

  expect_snapshot(
    expect_no_error(
      p1 <- tune:::check_parameters(
        w1,
        data = mtcars,
        grid_names = character(0)
      )
    )
  )
  expect_false(any(dials::has_unknowns(p1$object)))

  expect_no_error(
    p1 <- tune:::check_parameters(w1, data = mtcars, grid_names = "mtry")
  )

  w2 <-
    workflow() |>
    add_recipe(rec) |>
    add_model(rf1)

  expect_snapshot(
    expect_no_error(
      p2 <- tune:::check_parameters(w2, data = mtcars)
    )
  )
  expect_false(any(dials::has_unknowns(p2$object)))

  w3 <-
    workflow() |>
    add_recipe(rec) |>
    add_model(rf1)
  p3 <- extract_parameter_set_dials(w3)

  expect_snapshot(
    expect_no_error(
      p3_a <- tune:::check_parameters(w3, data = mtcars)
    )
  )
  expect_false(any(dials::has_unknowns(p3_a$object)))

  w4 <-
    workflow() |>
    add_recipe(rec_tune) |>
    add_model(rf1)

  expect_snapshot(error = TRUE, {
    tune:::check_parameters(w4, data = mtcars)
  })

  p4_a <-
    extract_parameter_set_dials(w4) |>
    update(mtry = dials::mtry(c(1, 10)))

  expect_no_error(
    p4_b <- tune:::check_parameters(w4, p4_a, data = mtcars)
  )
  expect_true(inherits(p4_b, "parameters"))

  w5 <-
    workflow() |>
    add_recipe(rec_tune) |>
    add_model(lm1)

  expect_no_error(
    p5 <- tune:::check_parameters(w5, data = mtcars)
  )
  expect_true(inherits(p5, "parameters"))
})

test_that("check fold weights", {
  folds <- rsample::vfold_cv(mtcars, v = 3)

  # No weights should pass silently
  expect_no_error(tune:::check_resample_weights(folds))

  # Valid weights should pass
  weights <- c(0.1, 0.5, 0.4)
  weighted_folds <- add_resample_weights(folds, weights)
  expect_no_error(tune:::check_resample_weights(weighted_folds))

  # Invalid weights should error
  expect_snapshot(
    add_resample_weights(folds, c("a", "b", "c")),
    error = TRUE
  )

  expect_snapshot(
    add_resample_weights(folds, c(0.5, 0.3)),
    error = TRUE
  )

  expect_snapshot(
    add_resample_weights(folds, c(-0.1, 0.5, 0.6)),
    error = TRUE
  )
})

test_that("fold weights integration test", {
  # Create simple data and resamples
  set.seed(1234)
  data_small <- mtcars[1:20, ]
  folds <- rsample::vfold_cv(data_small, v = 3)

  # Create simple model and recipe
  simple_rec <- recipes::recipe(mpg ~ wt + hp, data = data_small)
  simple_mod <- parsnip::linear_reg() |> parsnip::set_engine("lm")
  simple_wflow <- workflows::workflow() |>
    workflows::add_recipe(simple_rec) |>
    workflows::add_model(simple_mod)

  # Test with equal weights (should match unweighted results)
  equal_weights <- c(1, 1, 1)
  weighted_folds_equal <- add_resample_weights(folds, equal_weights)

  # Fit both weighted and unweighted
  unweighted_results <- fit_resamples(
    simple_wflow,
    folds,
    control = control_resamples(save_pred = FALSE)
  )
  weighted_results_equal <- fit_resamples(
    simple_wflow,
    weighted_folds_equal,
    control = control_resamples(save_pred = FALSE)
  )

  # Extract metrics
  unweighted_metrics <- collect_metrics(unweighted_results)
  weighted_metrics_equal <- collect_metrics(weighted_results_equal)

  # Should be nearly identical (allowing for small numerical differences)
  expect_equal(
    unweighted_metrics$mean,
    weighted_metrics_equal$mean,
    tolerance = 1e-10
  )

  # Test with unequal weights
  unequal_weights <- c(0.1, 0.3, 0.6) # Higher weight on last fold
  weighted_folds_unequal <- add_resample_weights(folds, unequal_weights)

  weighted_results_unequal <- fit_resamples(
    simple_wflow,
    weighted_folds_unequal,
    control = control_resamples(save_pred = FALSE)
  )
  weighted_metrics_unequal <- collect_metrics(weighted_results_unequal)

  # Should be different from unweighted results
  expect_false(all(
    abs(unweighted_metrics$mean - weighted_metrics_unequal$mean) < 1e-10
  ))

  # Verify that weights are properly stored and retrieved
  expect_equal(
    attr(weighted_folds_unequal, ".resample_weights"),
    unequal_weights
  )

  # Test fold size calculation
  calculated_weights <- calculate_resample_weights(folds)
  expect_length(calculated_weights, nrow(folds))
  expect_true(all(calculated_weights > 0))
  expect_equal(sum(calculated_weights), 1) # Should sum to 1 now
})

test_that("fold weights with tune_grid", {
  skip_if_not_installed("kernlab")

  # Create simple tuning scenario
  set.seed(5678)
  data_small <- mtcars[1:15, ]
  folds <- rsample::vfold_cv(data_small, v = 3)

  # Create tunable workflow
  tune_rec <- recipes::recipe(mpg ~ wt + hp, data = data_small) |>
    recipes::step_normalize(recipes::all_predictors())
  tune_mod <- parsnip::svm_rbf(
    cost = tune(),
    rbf_sigma = 0.001,
    mode = "regression"
  )

  tune_wflow <- workflows::workflow() |>
    workflows::add_recipe(tune_rec) |>
    workflows::add_model(tune_mod)

  # Create simple grid
  simple_grid <- tibble::tibble(cost = c(1, 10, 100))

  # Test with unequal weights
  weights <- c(0.2, 0.3, 0.5)
  weighted_folds <- add_resample_weights(folds, weights)

  # Tune with weights
  weighted_tune_results <- tune_grid(
    tune_wflow,
    weighted_folds,
    grid = simple_grid,
    control = control_grid(save_pred = FALSE)
  )

  # Verify results structure
  expect_s3_class(weighted_tune_results, "tune_results")

  # Extract metrics and verify they're computed
  weighted_metrics <- collect_metrics(weighted_tune_results)
  expect_true(nrow(weighted_metrics) > 0)
  expect_true(all(c("mean", "std_err") %in% names(weighted_metrics)))

  # Compare with unweighted results
  unweighted_tune_results <- tune_grid(
    tune_wflow,
    folds,
    grid = simple_grid,
    control = control_grid(save_pred = FALSE)
  )
  unweighted_metrics <- collect_metrics(unweighted_tune_results)

  # Results should differ due to weighting
  expect_false(all(
    abs(weighted_metrics$mean - unweighted_metrics$mean) < 1e-10
  ))
})

# ------------------------------------------------------------------------------
