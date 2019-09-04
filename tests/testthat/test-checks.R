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
    threshold = 1:10, num_terms = 1:10, neighbors = 1:10
  )

  set_1 <- param_set(chi_wflow)
  set_2 <- set_1 %>% update("num_terms", dials::num_terms(c(1, 3)))

  expect_equal(tune:::check_grid(grid_1, chi_wflow), grid_1)

  expect_error(tune:::check_grid(grid_1[, -1], chi_wflow),
               "the grid object should have columns")

  expect_error(tune:::check_grid(chi_wflow, chi_wflow),
               "The `grid` argument should")

  wflow_1 <-
    workflows::workflow() %>%
    add_model(glmn) %>%
    add_recipe(bare_rec)

  expect_error(grid_2 <- tune:::check_grid(NULL, wflow_1), NA)
  expect_equal(nrow(grid_2), 10)
  expect_true(inherits(grid_2, "data.frame"))
})


# ------------------------------------------------------------------------------

test_that('workflow objects', {
  wflow_1 <-
    workflows::workflow() %>%
    add_model(glmn) %>%
    add_recipe(bare_rec)

  expect_null(tune:::check_object(x = chi_wflow))
  expect_null(tune:::check_object(x = wflow_1))

  wflow_2 <-
    workflows::workflow() %>%
    add_model(boost_tree(mtry = tune()) %>% set_engine("xgboost")) %>%
    add_recipe(bare_rec)

  expect_null(tune:::check_object(x = wflow_2))
  expect_error(tune:::check_object(x = wflow_2, TRUE),
               "arguments whose ranges are not finalized")

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
  expect_error(
    tune:::check_grid_control(list(verbose = TRUE, allow_par = TRUE, extract = NULL)),
    NA
  )
  expect_warning(
    expect_error(tune:::check_grid_control(list(verbose = TRUE, par = TRUE)),
                 "allow_par")
  )
  expect_warning(
    expect_error(
      tune:::check_grid_control(list(verbose = TRUE, par = TRUE, allow_par = TRUE)),
      "extract"
    )
  )
})

test_that('Bayes control objects', {
  expect_error(Bayes_control(), NA)
  expect_error(Bayes_control(tomato = 1))
  expect_error(
    tune:::check_Bayes_control(list(verbose = TRUE, uncertain = 3, seed = 1,
                                    time_limit = 12)),
    NA
  )
  expect_warning(
    expect_error(
      tune:::check_Bayes_control(list(verbose = TRUE, random_val = 3, seed = 1)),
      "uncertain"
    )
  )

})

# ------------------------------------------------------------------------------

test_that('initial values', {
  wflow_1 <-
    workflows::workflow() %>%
    add_model(glmn) %>%
    add_recipe(recipe(mpg ~ ., data = mtcars))

  grid_1 <- tune:::check_initial(NULL, param_set(wflow_1), wflow_1,
                                 mtfolds, yardstick::metric_set(yardstick::rsq),
                                 Bayes_control())
  expect_true(is.data.frame(grid_1))
  expect_equal(nrow(grid_1), 3)

  grid_2 <- tune:::check_initial(2, param_set(wflow_1), wflow_1,
                                 mtfolds, yardstick::metric_set(yardstick::rsq),
                                 Bayes_control())
  expect_true(is.data.frame(grid_2))
  expect_equal(nrow(grid_2), 2)

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


