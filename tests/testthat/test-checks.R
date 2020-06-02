context("object checking")

# ------------------------------------------------------------------------------

source(test_path("../helper-objects.R"))
load(test_path("svm_results.RData"))

# ------------------------------------------------------------------------------

test_that('rsample objects', {

  obj_cv <- rsample::vfold_cv(mtcars)
  obj_loo <- rsample::loo_cv(mtcars)
  obj_nst <- rsample::nested_cv(mtcars, obj_cv, inside = bootstraps())
  expect_error(tune:::check_rset(obj_cv), regexp = NA)
  expect_error(tune:::check_rset(obj_loo), regexp = "Leave-one-out")
  expect_error(tune:::check_rset(obj_nst), regexp = "Nested resampling")
})

# ------------------------------------------------------------------------------

test_that('grid objects', {

  grid_1 <- tibble(
    penalty = 1:10, mixture = 1:10, imputation = 1:10,
    threshold = 1:10, deg_free = 1:10, degree = 1:10
  )

  set_1 <- dials::parameters(chi_wflow)
  set_2 <- set_1 %>% update(deg_free = dials::deg_free(c(1, 3)))

  expect_equal(tune:::check_grid(grid_1, chi_wflow), grid_1)

  expect_error(tune:::check_grid(chi_wflow, chi_wflow),
               "`grid` should be a positive integer or a data frame")

  wflow_1 <-
    workflow() %>%
    add_model(svm_mod) %>%
    add_recipe(bare_rec)

  expect_error(grid_2 <- tune:::check_grid(6, wflow_1), NA)
  expect_equal(nrow(grid_2), 6)
  expect_true(inherits(grid_2, "data.frame"))

  # For issue #56
  grid_3 <- as.data.frame(grid_1)
  expect_equal(tune:::check_grid(grid_3, chi_wflow), grid_1)
})

test_that("Unknown `grid` columns are caught", {

  data <- data.frame(x = 1:2, y = 1:2)

  rec <- recipes::recipe(y ~ x, data = data)
  rec <- recipes::step_bs(rec, x, deg_free = tune())
  rec <- recipes::step_pca(rec, x, num_comp = tune())

  model <- parsnip::linear_reg()
  model <- parsnip::set_engine(model, "lm")

  workflow <- workflow()
  workflow <- add_recipe(workflow, rec)
  workflow <- add_model(workflow, model)

  grid <- tibble(deg_free = 2, num_comp = 0.01, other1 = 1, other2 = 1)

  expect_error(
    tune:::check_grid(grid, workflow),
    "have not been marked for tuning by `tune[(][)]`: 'other1', 'other2'"
  )
})

test_that("Missing required `grid` columns are caught", {

  data <- data.frame(x = 1:2, y = 1:2)

  rec <- recipes::recipe(y ~ x, data = data)
  rec <- recipes::step_bs(rec, x, deg_free = tune())
  rec <- recipes::step_pca(rec, x, num_comp = tune())

  model <- parsnip::linear_reg()
  model <- parsnip::set_engine(model, "lm")

  workflow <- workflow()
  workflow <- add_recipe(workflow, rec)
  workflow <- add_model(workflow, model)

  grid <- tibble(num_comp = 0.01)

  expect_error(
    tune:::check_grid(grid, workflow),
    "have been marked for tuning by `tune[(][)]`: 'deg_free'"
  )
})

# ------------------------------------------------------------------------------

test_that('workflow objects', {

  skip_if_not_installed("xgboost")
  wflow_1 <-
    workflow() %>%
    add_model(svm_mod) %>%
    add_recipe(bare_rec)

  expect_null(tune:::check_workflow(x = wflow_1))

  wflow_2 <-
    workflow() %>%
    add_model(boost_tree(mtry = tune()) %>% set_engine("xgboost")) %>%
    add_recipe(bare_rec)

  expect_null(tune:::check_workflow(x = wflow_2))
  expect_error(tune:::check_workflow(x = wflow_2, check_dials = TRUE),
               "arguments whose ranges are not finalized")

  wflow_3 <-
    workflow() %>%
    add_model(glmn)
  expect_error(tune:::check_workflow(wflow_3),
               "A model formula or recipe are required.")

  wflow_4 <-
    workflow() %>%
    add_recipe(bare_rec)
  expect_error(tune:::check_workflow(wflow_4),
               "A parsnip model is required.")
})

# ------------------------------------------------------------------------------

test_that('yardstick objects', {

  metrics_1 <- tune:::check_metrics(NULL, chi_wflow)
  metrics_2 <- yardstick::metric_set(yardstick:::rmse)
  expect_true(inherits(metrics_1, "numeric_metric_set"))
  expect_error(tune:::check_metrics(yardstick::rmse, chi_wflow),
               "The `metrics` argument should be the results")
  expect_true(inherits(tune:::check_metrics(metrics_2, chi_wflow), "numeric_metric_set"))
})

test_that('metrics must match the parsnip engine', {

  metric_set1 <- yardstick::metric_set(yardstick::accuracy)
  metric_set2 <- yardstick::metric_set(yardstick::rmse)

  mod1 <- parsnip::rand_forest(mode = "regression")
  mod2 <- parsnip::rand_forest(mode = "classification")

  workflow1 <- add_model(workflow(), mod1)
  workflow2 <- add_model(workflow(), mod2)

  expect_error(
    tune:::check_metrics(metric_set1, workflow1),
    "The parsnip model has `mode = 'regression'`"
  )

  expect_error(
    tune:::check_metrics(metric_set2, workflow2),
    "The parsnip model has `mode = 'classification'`"
  )
})

# ------------------------------------------------------------------------------

test_that('grid control objects', {

  expect_error(control_grid(), NA)
  expect_error(control_grid(tomato = 1))
  expect_error(control_grid(verbose = 1), "Argument 'verbose' should be a single logical")
  expect_error(control_grid(verbose = rep(TRUE, 2)), "Argument 'verbose' should be a single logical")
  expect_error(control_grid(allow_par = 1), "Argument 'allow_par' should be a single logical")
  expect_error(control_grid(save_pred = "no"), "Argument 'save_pred' should be a single logical")
  expect_error(control_grid(extract = Inf), "Argument 'extract' should be a function or NULL")
  expect_error(control_grid(pkgs = Inf), "Argument 'pkgs' should be a character or NULL")

  expect_error(control_grid(verbose = TRUE), NA)
  expect_error(control_grid(allow_par = FALSE), NA)
  expect_error(control_grid(save_pred = TRUE), NA)
  expect_error(control_grid(extract = NULL), NA)
  expect_error(control_grid(extract = I), NA)
  expect_error(control_grid(pkgs = NULL), NA)
  expect_error(control_grid(pkgs = letters), NA)
  expect_is(control_grid(), c("control_grid", "control_resamples"))
})

test_that('Bayes control objects', {

  expect_error(control_bayes(), NA)
  expect_error(control_bayes(tomato = 1))
  expect_error(control_bayes(verbose = 1), "Argument 'verbose' should be a single logical")
  expect_error(control_bayes(verbose = rep(TRUE, 2)), "Argument 'verbose' should be a single logical")
  expect_error(control_bayes(no_improve = FALSE), "Argument 'no_improve' should be a single numeric")
  expect_error(control_bayes(uncertain = FALSE), "Argument 'uncertain' should be a single numeric")
  expect_error(control_bayes(seed = FALSE), "Argument 'seed' should be a single numeric")
  expect_error(control_bayes(save_pred = "no"), "Argument 'save_pred' should be a single logical")
  expect_error(control_bayes(extract = Inf), "Argument 'extract' should be a function or NULL")
  expect_error(control_bayes(pkgs = Inf), "Argument 'pkgs' should be a character or NULL")
  expect_error(control_bayes(time_limit = "a"), "Argument 'time_limit' should be a single logical or numeric")

  expect_message(control_bayes(no_improve = 2, uncertain = 5), "Uncertainty sample scheduled after 5")

  expect_error(control_bayes(verbose = TRUE), NA)
  expect_error(control_bayes(no_improve = 2), NA)
  expect_error(control_bayes(uncertain = 2), NA)
  expect_error(control_bayes(save_pred = TRUE), NA)
  expect_error(control_bayes(extract = NULL), NA)
  expect_error(control_bayes(extract = I), NA)
  expect_error(control_bayes(pkgs = NULL), NA)
  expect_error(control_bayes(pkgs = letters), NA)
  expect_error(control_bayes(time_limit = 2), NA)
  expect_is(control_bayes(), "control_bayes")
})

# ------------------------------------------------------------------------------

test_that('initial values', {

  svm_mod <-
    svm_rbf(cost = tune()) %>%
    set_engine("kernlab") %>%
    set_mode("regression")

  wflow_1 <-
    workflow() %>%
    add_model(svm_mod) %>%
    add_recipe(recipe(mpg ~ ., data = mtcars))

  grid_1 <- tune:::check_initial(2, dials::parameters(wflow_1), wflow_1,
                                 mtfolds, yardstick::metric_set(yardstick::rsq),
                                 control_bayes())
  expect_true(is.data.frame(grid_1))
  expect_equal(nrow(grid_1), nrow(mtfolds))
  expect_true(all(purrr::map_lgl(grid_1$.metrics, ~ nrow(.x) == 2)))

  expect_error(
    tune:::check_initial(data.frame(),
                         dials::parameters(wflow_1), wflow_1,
                         mtfolds, yardstick::metric_set(yardstick::rsq),
                         control_bayes()),
    "`initial` should be a positive integer or"
  )
})

# ------------------------------------------------------------------------------


test_that('Acquisition function objects', {

  expect_null(tune:::check_direction(FALSE))
  expect_error(tune:::check_direction(1), "should be a single logical.")
  expect_error(tune:::check_direction(rep(TRUE, 2)), "should be a single logical.")

  expect_null(tune:::check_best(1))
  expect_error(tune:::check_best(FALSE), "should be a single, non-missing numeric")
  expect_error(tune:::check_best(rep(2, 2)), "should be a single, non-missing numeric")
  expect_error(tune:::check_best(NA), "should be a single, non-missing numeric")

})

# ------------------------------------------------------------------------------

test_that('validation helpers', {

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

test_that('check parameter finalization', {

  rec <-
    recipe(mpg ~ ., data = mtcars) %>%
    step_ns(disp, deg_free = 3)
  rec_tune <- rec %>% step_pca(all_predictors(), num_comp = tune())
  f <- mpg ~ .
  rf1 <-
    rand_forest(mtry = tune(), min_n  = tune()) %>%
    set_engine("ranger") %>%
    set_mode("regression")
  lm1 <-
    linear_reg(penalty = tune()) %>%
    set_engine("glmnet")

  w1 <-
    workflow() %>%
    add_formula(f) %>%
    add_model(rf1)

  expect_message(
    expect_error(
      p1 <- tune:::check_parameters(w1, data = mtcars),
      regex = NA
    ),
    "finalize unknown parameter: mtry"
  )
  expect_false(any(dials::has_unknowns(p1$object)))

  w2 <-
    workflow() %>%
    add_recipe(rec) %>%
    add_model(rf1)

  expect_message(
    expect_error(
      p2 <- tune:::check_parameters(w2, data = mtcars),
      regex = NA
    ),
    "finalize unknown parameter: mtry"
  )
  expect_false(any(dials::has_unknowns(p2$object)))

  w3 <-
    workflow() %>%
    add_recipe(rec) %>%
    add_model(rf1)
  p3 <- parameters(w3)

  expect_message(
    expect_error(
      p3_a <- tune:::check_parameters(w3, data = mtcars),
      regex = NA
    ),
    "finalize unknown parameter: mtry"
  )
  expect_false(any(dials::has_unknowns(p3_a$object)))

  w4 <-
    workflow() %>%
    add_recipe(rec_tune) %>%
    add_model(rf1)

  expect_error(
    tune:::check_parameters(w4, data = mtcars),
    regex = "to finalize the parameter ranges"
  )

  p4_a <-
    parameters(w4) %>%
    update(mtry = dials::mtry(c(1, 10)))

  expect_error(
    p4_b <- tune:::check_parameters(w4, p4_a, data = mtcars),
    regex = NA
  )
  expect_true(inherits(p4_b, "parameters"))

  w5 <-
    workflow() %>%
    add_recipe(rec_tune) %>%
    add_model(lm1)

  expect_error(
    p5 <- tune:::check_parameters(w5, data = mtcars),
    regex = NA
  )
  expect_true(inherits(p5, "parameters"))
})

