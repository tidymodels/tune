check_param_set_tibble <- function(x) {
  expect_equal(names(x), c("name", "id", "source", "component", "component_id", "object"))
  expect_equal(class(x$name), "character")
  expect_equal(class(x$id), "character")
  expect_equal(class(x$source), "character")
  expect_equal(class(x$component), "character")
  expect_equal(class(x$component_id), "character")
  expect_true(!any(duplicated(x$id)))

  expect_equal(class(x$object), "list")
  obj_check <- purrr::map_lgl(x$object, ~ inherits(.x, "param") | all(is.na(.x)))
  expect_true(all(obj_check))

  invisible(TRUE)
}

# ------------------------------------------------------------------------------

test_that("parameters.recipe() still works after deprecation", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("splines2")

  withr::local_options(lifecycle_verbosity = "quiet")

  data("Chicago", package = "modeldata")
  spline_rec <-
    recipes::recipe(ridership ~ ., data = head(Chicago)) %>%
    recipes::step_impute_knn(recipes::all_predictors(), neighbors = tune("imputation")) %>%
    recipes::step_other(recipes::all_nominal(), threshold = tune()) %>%
    recipes::step_spline_b(recipes::all_predictors(), deg_free = tune(), degree = tune())

  spline_info <- dials::parameters(spline_rec)
  check_param_set_tibble(spline_info)
})

# ------------------------------------------------------------------------------

test_that("parameters.model_spec() still works after deprecation", {
  withr::local_options(lifecycle_verbosity = "quiet")

  skip_if_not_installed("parsnip")

  bst_model <-
    parsnip::boost_tree(mode = "classification", trees = tune("funky name \n")) %>%
    parsnip::set_engine("C5.0", rules = tune(), noGlobalPruning = TRUE)

  c5_info <- dials::parameters(bst_model)
  check_param_set_tibble(c5_info)
})
