context("object checking")

# ------------------------------------------------------------------------------

source("../helper-objects.R")
load("svm_results.RData")

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

  expect_error(tune:::check_grid(grid_1[, -1], chi_wflow),
               "the grid object should have columns")

  expect_error(tune:::check_grid(chi_wflow, chi_wflow),
               "The `grid` argument should")

  wflow_1 <-
    workflow() %>%
    add_model(glmn) %>%
    add_recipe(bare_rec)

  expect_error(grid_2 <- tune:::check_grid(NULL, wflow_1), NA)
  expect_equal(nrow(grid_2), 10)
  expect_true(inherits(grid_2, "data.frame"))

  # For issue #56
  grid_3 <- as.data.frame(grid_1)
  expect_equal(tune:::check_grid(grid_3, chi_wflow), grid_1)
})


# ------------------------------------------------------------------------------

test_that('workflow objects', {
  skip_if_not_installed("xgboost")
  wflow_1 <-
    workflow() %>%
    add_model(glmn) %>%
    add_recipe(bare_rec)

  expect_null(tune:::check_object(x = chi_wflow))
  expect_null(tune:::check_object(x = wflow_1))

  wflow_2 <-
    workflow() %>%
    add_model(boost_tree(mtry = tune()) %>% set_engine("xgboost")) %>%
    add_recipe(bare_rec)

  expect_null(tune:::check_object(x = wflow_2))
  expect_error(tune:::check_object(x = wflow_2, TRUE),
               "arguments whose ranges are not finalized")

  wflow_3 <-
    workflow() %>%
    add_model(glmn)
  expect_error(tune:::check_object(wflow_3),
               "A model formula or recipe are required.")

  wflow_4 <-
    workflow() %>%
    add_recipe(bare_rec)
  expect_error(tune:::check_object(wflow_4),
               "A parsnip model is required.")
})

# ------------------------------------------------------------------------------

test_that('yardstick objects', {
  perf_1 <- tune:::check_perf(NULL, chi_wflow)
  perf_2 <- yardstick::metric_set(yardstick:::rmse)
  expect_true(inherits(perf_1, "numeric_metric_set"))
  expect_error(tune:::check_perf(yardstick::rmse, chi_wflow),
               "The `perf` argument should be the results")
  expect_true(inherits(tune:::check_perf(perf_2, chi_wflow), "numeric_metric_set"))
})

# ------------------------------------------------------------------------------

test_that('grid control objects', {
  expect_error(grid_control(), NA)
  expect_error(grid_control(tomato = 1))
  expect_error(grid_control(verbose = 1), "Argument 'verbose' should be a single logical")
  expect_error(grid_control(verbose = rep(TRUE, 2)), "Argument 'verbose' should be a single logical")
  expect_error(grid_control(allow_par = 1), "Argument 'allow_par' should be a single logical")
  expect_error(grid_control(save_pred = "no"), "Argument 'save_pred' should be a single logical")
  expect_error(grid_control(extract = Inf), "Argument 'extract' should be a function or NULL")
  expect_error(grid_control(pkgs = Inf), "Argument 'pkgs' should be a character or NULL")

  expect_error(grid_control(verbose = TRUE), NA)
  expect_error(grid_control(allow_par = FALSE), NA)
  expect_error(grid_control(save_pred = TRUE), NA)
  expect_error(grid_control(extract = NULL), NA)
  expect_error(grid_control(extract = I), NA)
  expect_error(grid_control(pkgs = NULL), NA)
  expect_error(grid_control(pkgs = letters), NA)

})

test_that('Bayes control objects', {
  expect_error(Bayes_control(), NA)
  expect_error(Bayes_control(tomato = 1))
  expect_error(Bayes_control(verbose = 1), "Argument 'verbose' should be a single logical")
  expect_error(Bayes_control(verbose = rep(TRUE, 2)), "Argument 'verbose' should be a single logical")
  expect_error(Bayes_control(no_improve = FALSE), "Argument 'no_improve' should be a single numeric")
  expect_error(Bayes_control(uncertain = FALSE), "Argument 'uncertain' should be a single numeric")
  expect_error(Bayes_control(seed = FALSE), "Argument 'seed' should be a single numeric")
  expect_error(Bayes_control(save_pred = "no"), "Argument 'save_pred' should be a single logical")
  expect_error(Bayes_control(extract = Inf), "Argument 'extract' should be a function or NULL")
  expect_error(Bayes_control(pkgs = Inf), "Argument 'pkgs' should be a character or NULL")
  expect_error(Bayes_control(time_limit = "a"), "Argument 'time_limit' should be a single logical or numeric")

  expect_message(Bayes_control(no_improve = 2, uncertain = 5), "Uncertainty sample scheduled after 5")

  expect_error(Bayes_control(verbose = TRUE), NA)
  expect_error(Bayes_control(no_improve = 2), NA)
  expect_error(Bayes_control(uncertain = 2), NA)
  expect_error(Bayes_control(save_pred = TRUE), NA)
  expect_error(Bayes_control(extract = NULL), NA)
  expect_error(Bayes_control(extract = I), NA)
  expect_error(Bayes_control(pkgs = NULL), NA)
  expect_error(Bayes_control(pkgs = letters), NA)
  expect_error(Bayes_control(time_limit = 2), NA)

})

# ------------------------------------------------------------------------------

test_that('initial values', {
  wflow_1 <-
    workflow() %>%
    add_model(glmn) %>%
    add_recipe(recipe(mpg ~ ., data = mtcars))

  grid_1 <- tune:::check_initial(NULL, dials::parameters(wflow_1), wflow_1,
                                 mtfolds, yardstick::metric_set(yardstick::rsq),
                                 Bayes_control())
  expect_true(is.data.frame(grid_1))
  expect_equal(nrow(grid_1), 10)
  expect_true(all(purrr::map_lgl(grid_1$.metrics, ~ nrow(.x) == 3)))

  grid_2 <- tune:::check_initial(2, dials::parameters(wflow_1), wflow_1,
                                 mtfolds, yardstick::metric_set(yardstick::rsq),
                                 Bayes_control())
  expect_true(is.data.frame(grid_2))
  expect_equal(nrow(grid_2), 10)
  expect_true(all(purrr::map_lgl(grid_2$.metrics, ~ nrow(.x) == 2)))

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


