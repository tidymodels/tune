data("Chicago", package = "modeldata")

spline_rec <-
  recipes::recipe(ridership ~ ., data = head(Chicago)) %>%
  recipes::step_date(date) %>%
  recipes::step_holiday(date) %>%
  recipes::step_rm(date, dplyr::ends_with("away")) %>%
  recipes::step_impute_knn(recipes::all_predictors(), neighbors = tune("imputation")) %>%
  recipes::step_other(recipes::all_nominal(), threshold = tune()) %>%
  recipes::step_dummy(recipes::all_nominal()) %>%
  recipes::step_normalize(recipes::all_numeric_predictors()) %>%
  recipes::step_bs(recipes::all_predictors(), deg_free = tune(), degree = tune())

glmn <- parsnip::linear_reg(penalty = tune(), mixture = tune()) %>%
  parsnip::set_engine("glmnet")

chi_wflow <-
  workflows::workflow() %>%
  workflows::add_recipe(spline_rec) %>%
  workflows::add_model(glmn)

bare_rec <-
  recipes::recipe(ridership ~ ., data = head(Chicago))

svm_mod <-
  parsnip::svm_rbf(cost = tune()) %>%
  parsnip::set_engine("kernlab") %>%
  parsnip::set_mode("regression")

mtfolds <- rsample::vfold_cv(mtcars)

svm_results <- readRDS(test_path("svm_results.rds"))

# ------------------------------------------------------------------------------

test_that('rsample objects', {

  obj_cv <- rsample::vfold_cv(mtcars)
  obj_loo <- rsample::loo_cv(mtcars)
  obj_nst <- rsample::nested_cv(mtcars, obj_cv, inside = bootstraps())
  expect_error(tune:::check_rset(obj_cv), regexp = NA)
  expect_snapshot(error = TRUE, tune:::check_rset(obj_loo))
  expect_snapshot(error = TRUE, tune:::check_rset(obj_nst))
})

# ------------------------------------------------------------------------------

test_that('grid objects', {

  grid_1 <- tibble::tibble(
    penalty = 1:10, mixture = 1:10, imputation = 1:10,
    threshold = 1:10, deg_free = 1:10, degree = 1:10
  )

  set_1 <- extract_parameter_set_dials(chi_wflow)
  set_2 <- set_1 %>% update(deg_free = dials::deg_free(c(1, 3)))

  expect_equal(tune:::check_grid(grid_1, chi_wflow), grid_1)

  expect_silent(tune:::check_grid(grid_1, chi_wflow))

  expect_snapshot(
    tune:::check_grid(rbind(grid_1, grid_1), chi_wflow)
  )

  expect_snapshot(error = TRUE,
    tune:::check_grid(chi_wflow, chi_wflow)
  )

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

  # For weird attributes
  grid_4 <- expand.grid(
    penalty = 1:10, mixture = 12, imputation = 1:2,
    threshold = 1:2, deg_free = 2:3, degree = 9:10
  )
  expect_equal(tune:::check_grid(grid_4, chi_wflow),
               tibble::as_tibble(vctrs::data_frame(grid_4)))

  expect_silent(tune:::check_grid(grid_4, chi_wflow))
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

  grid <- tibble::tibble(deg_free = 2, num_comp = 0.01, other1 = 1, other2 = 1)

  expect_snapshot(error = TRUE,
    tune:::check_grid(grid, workflow)
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

  grid <- tibble::tibble(num_comp = 0.01)

  expect_snapshot(error = TRUE,
    tune:::check_grid(grid, workflow)
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
    add_model(parsnip::boost_tree(mtry = tune()) %>% parsnip::set_engine("xgboost")) %>%
    add_recipe(bare_rec)

  expect_null(tune:::check_workflow(x = wflow_2))
  expect_snapshot(error = TRUE,
    tune:::check_workflow(x = wflow_2, check_dials = TRUE)
  )

  wflow_3 <-
    workflow() %>%
    add_model(glmn)
  expect_snapshot(error = TRUE,
    tune:::check_workflow(wflow_3)
  )

  wflow_4 <-
    workflow() %>%
    add_recipe(bare_rec)
  expect_snapshot(error = TRUE,
    tune:::check_workflow(wflow_4)
  )
})

# ------------------------------------------------------------------------------

test_that('yardstick objects', {

  metrics_1 <- tune:::check_metrics(NULL, chi_wflow)
  metrics_2 <- yardstick::metric_set(yardstick:::rmse)
  expect_true(inherits(metrics_1, "numeric_metric_set"))
  expect_snapshot(error = TRUE,
    tune:::check_metrics(yardstick::rmse, chi_wflow)
  )
  expect_true(inherits(tune:::check_metrics(metrics_2, chi_wflow), "numeric_metric_set"))
})

test_that('metrics must match the parsnip engine', {

  metric_set1 <- yardstick::metric_set(yardstick::accuracy)
  metric_set2 <- yardstick::metric_set(yardstick::rmse)

  mod1 <- parsnip::rand_forest(mode = "regression")
  mod2 <- parsnip::rand_forest(mode = "classification")

  workflow1 <- add_model(workflow(), mod1)
  workflow2 <- add_model(workflow(), mod2)

  expect_snapshot(error = TRUE,
    tune:::check_metrics(metric_set1, workflow1)
  )

  expect_snapshot(error = TRUE,
    tune:::check_metrics(metric_set2, workflow2)
  )
})

# ------------------------------------------------------------------------------

test_that('grid control objects', {

  expect_error(control_grid(), NA)
  expect_snapshot(error = TRUE, control_grid(tomato = 1))
  expect_snapshot(error = TRUE, control_grid(verbose = 1))
  expect_snapshot(error = TRUE, control_grid(verbose = rep(TRUE, 2)))
  expect_snapshot(error = TRUE, control_grid(allow_par = 1))
  expect_snapshot(error = TRUE, control_grid(save_pred = "no"))
  expect_snapshot(error = TRUE, control_grid(extract = Inf))
  expect_snapshot(error = TRUE, control_grid(pkgs = Inf))

  expect_error(control_grid(verbose = TRUE), NA)
  expect_error(control_grid(allow_par = FALSE), NA)
  expect_error(control_grid(save_pred = TRUE), NA)
  expect_error(control_grid(extract = NULL), NA)
  expect_error(control_grid(extract = I), NA)
  expect_error(control_grid(pkgs = NULL), NA)
  expect_error(control_grid(pkgs = letters), NA)
  expect_s3_class(control_grid(), c("control_grid", "control_resamples"))
})

test_that('Bayes control objects', {

  expect_error(control_bayes(), NA)
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

  expect_error(control_bayes(verbose = TRUE), NA)
  expect_error(control_bayes(no_improve = 2), NA)
  expect_error(control_bayes(uncertain = 2), NA)
  expect_error(control_bayes(save_pred = TRUE), NA)
  expect_error(control_bayes(extract = NULL), NA)
  expect_error(control_bayes(extract = I), NA)
  expect_error(control_bayes(pkgs = NULL), NA)
  expect_error(control_bayes(pkgs = letters), NA)
  expect_error(control_bayes(time_limit = 2), NA)
  expect_s3_class(control_bayes(), "control_bayes")
})

# ------------------------------------------------------------------------------

test_that('initial values', {

  svm_mod <-
    parsnip::svm_rbf(cost = tune()) %>%
    parsnip::set_engine("kernlab") %>%
    parsnip::set_mode("regression")

  wflow_1 <-
    workflow() %>%
    add_model(svm_mod) %>%
    add_recipe(recipes::recipe(mpg ~ ., data = mtcars))

  grid_1 <- tune:::check_initial(2, extract_parameter_set_dials(wflow_1), wflow_1,
                                 mtfolds, yardstick::metric_set(yardstick::rsq),
                                 control_bayes())
  expect_true(is.data.frame(grid_1))
  expect_equal(nrow(grid_1), nrow(mtfolds))
  expect_true(all(purrr::map_lgl(grid_1$.metrics, ~ nrow(.x) == 2)))

  expect_snapshot(error = TRUE,
    tune:::check_initial(data.frame(),
                         extract_parameter_set_dials(wflow_1), wflow_1,
                         mtfolds, yardstick::metric_set(yardstick::rsq),
                         control_bayes())
  )
})

# ------------------------------------------------------------------------------


test_that('Acquisition function objects', {

  expect_null(tune:::check_direction(FALSE))
  expect_snapshot(error = TRUE, tune:::check_direction(1))
  expect_snapshot(error = TRUE, tune:::check_direction(rep(TRUE, 2)))

  expect_null(tune:::check_best(1))
  expect_snapshot(error = TRUE, tune:::check_best(FALSE))
  expect_snapshot(error = TRUE, tune:::check_best(rep(2, 2)))
  expect_snapshot(error = TRUE, tune:::check_best(NA))

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
    recipes::recipe(mpg ~ ., data = mtcars) %>%
    recipes::step_ns(disp, deg_free = 3)
  rec_tune <- rec %>% recipes::step_pca(recipes::all_predictors(), num_comp = tune())
  f <- mpg ~ .
  rf1 <-
    parsnip::rand_forest(mtry = tune(), min_n  = tune()) %>%
    parsnip::set_engine("ranger") %>%
    parsnip::set_mode("regression")
  lm1 <-
    parsnip::linear_reg(penalty = tune()) %>%
    parsnip::set_engine("glmnet")

  w1 <-
    workflow() %>%
    add_formula(f) %>%
    add_model(rf1)

  expect_snapshot(
    expect_error(
      p1 <- tune:::check_parameters(w1, data = mtcars, grid_names = character(0)),
      regex = NA
    )
  )
  expect_false(any(dials::has_unknowns(p1$object)))

  expect_error(
    p1 <- tune:::check_parameters(w1, data = mtcars, grid_names = "mtry"),
    regex = NA
  )

  w2 <-
    workflow() %>%
    add_recipe(rec) %>%
    add_model(rf1)

  expect_snapshot(
    expect_error(
      p2 <- tune:::check_parameters(w2, data = mtcars),
      regex = NA
    )
  )
  expect_false(any(dials::has_unknowns(p2$object)))

  w3 <-
    workflow() %>%
    add_recipe(rec) %>%
    add_model(rf1)
  p3 <- extract_parameter_set_dials(w3)

  expect_snapshot(
    expect_error(
      p3_a <- tune:::check_parameters(w3, data = mtcars),
      regex = NA
    )
  )
  expect_false(any(dials::has_unknowns(p3_a$object)))

  w4 <-
    workflow() %>%
    add_recipe(rec_tune) %>%
    add_model(rf1)

  expect_snapshot(error = TRUE,
    tune:::check_parameters(w4, data = mtcars)
  )

  p4_a <-
    extract_parameter_set_dials(w4) %>%
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
