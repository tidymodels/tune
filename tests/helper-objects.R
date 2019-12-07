
library(tune)
library(recipes)
library(parsnip)
library(rsample)
library(workflows)
data("Chicago", package = "dials")

# ------------------------------------------------------------------------------

spline_rec <-
  recipe(ridership ~ ., data = head(Chicago)) %>%
  step_date(date) %>%
  step_holiday(date) %>%
  step_rm(date, ends_with("away")) %>%
  step_knnimpute(all_predictors(), neighbors = tune("imputation")) %>%
  step_other(all_nominal(), threshold = tune()) %>%
  step_dummy(all_nominal()) %>%
  step_normalize(all_predictors()) %>%
  step_bs(all_predictors(), deg_free = tune(), degree = tune())

bare_rec <-
  recipe(ridership ~ ., data = head(Chicago))

rm_rec <-
  recipe(ridership ~ ., data = head(Chicago)) %>%
  step_rm(date, ends_with("away"))

ununtable_rec <-
  recipe(ridership ~ ., data = head(Chicago)) %>%
  step_dummy(all_nominal(), naming = tune())

# ------------------------------------------------------------------------------

bst_model <-
  boost_tree(mode = "classification", trees = tune("funky name \n")) %>%
  set_engine("C5.0", rules = tune(), noGlobalPruning = TRUE)

lm_model <-
  linear_reg() %>%
  set_engine("lm")

no_engine <- linear_reg()

weird_annotation <-
  boost_tree(mode = "regression", trees = tune("$^%&#!")) %>%
  set_engine("C5.0", rules = TRUE, noGlobalPruning = TRUE)

glmn <- linear_reg(penalty = tune(), mixture = tune()) %>% set_engine("glmnet")

svm_mod <-
  svm_rbf(cost = tune()) %>%
  set_engine("kernlab") %>%
  set_mode("regression")

# ------------------------------------------------------------------------------

chi_wflow <-
  workflow() %>%
  add_recipe(spline_rec) %>%
  add_model(glmn)

# ------------------------------------------------------------------------------

mtfolds <- vfold_cv(mtcars)

# ------------------------------------------------------------------------------

check_tunable_tibble <- function(x) {
  expect_equal(names(x), c("name", "call_info", "source", "component", "component_id"))
  expect_equal(class(x$name), "character")
  expect_equal(class(x$call_info), "list")
  expect_equal(class(x$source), "character")
  expect_equal(class(x$component), "character")
  expect_equal(class(x$component_id), "character")
  invisible(TRUE)
}


check_tune_args_tibble <- function(x) {
  expect_equal(names(x), c("name", "tunable", "id", "source", "component", "component_id"))
  expect_equal(class(x$name), "character")
  expect_equal(class(x$tunable), "logical")
  expect_equal(class(x$id), "character")
  expect_equal(class(x$source), "character")
  expect_equal(class(x$component), "character")
  expect_equal(class(x$component_id), "character")
  expect_true(!any(duplicated(x$id)))
  invisible(TRUE)
}

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

check_merged_tibble <- function(x, type = "recipe", complete = TRUE) {
  expect_true(tibble::is_tibble(x))
  expect_equal(names(x), "x")
  expect_true(all(purrr::map_lgl(x$x, inherits, type)))
  if (complete) {
    any_args <- purrr::map_int(x$x, ~ tune_args(.x) %>% nrow())
    expect_true(!any(any_args > 0))
  }
  invisible(TRUE)
}

