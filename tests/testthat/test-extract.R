context("object extraction")

# ------------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(recipes)
library(workflows)
library(dials)
library(rsample)
library(glmnet)

source("../helper-objects.R")

# ------------------------------------------------------------------------------

rec_tune_1 <-
  recipe(mpg ~ ., data = mtcars) %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors(), num_comp = tune())

rec_no_tune_1 <-
  recipe(mpg ~ ., data = mtcars) %>%
  step_normalize(all_predictors())

lm_mod <- linear_reg() %>% set_engine("lm")
glmn_mod <-
  linear_reg(mixture = tune(), penalty = tune()) %>%
  set_engine("glmnet", nlambda = 5)

set.seed(363)
mt_folds <- vfold_cv(mtcars, v = 5)

# ------------------------------------------------------------------------------

test_that('tune recipe only', {
  extr_1_1 <- function(x) {
    tidy(x$recipe, number = 2)
  }
  expect_error(
    res_1_1 <-
      workflow() %>%
      add_recipe(rec_tune_1) %>%
      add_model(lm_mod) %>%
      tune_grid(rs = mt_folds, control = grid_control(extract = extr_1_1)),
    NA
  )
  expect_error(extract_1_1 <- dplyr::bind_rows(res_1_1$.extracts), NA)

  expect_true(all(names(extract_1_1) == c("num_comp", ".extracts")))
  expect_true(
    all(purrr:::map_lgl(extract_1_1$.extracts, ~ tibble::is_tibble(.x))),
  )


  extr_1_2 <- function(x) {
    tidy(x, number = 2) # should fail
  }

  # should not fail:
  expect_error(
    res_1_2 <-
      workflow() %>%
      add_recipe(rec_tune_1) %>%
      add_model(lm_mod) %>%
      tune_grid(rs = mt_folds, control = grid_control(extract = extr_1_2)),
    NA
  )

  expect_error(extract_1_2 <- dplyr::bind_rows(res_1_2$.extracts), NA)
  expect_true(all(names(extract_1_2) == c("num_comp", ".extracts")))
  expect_true(
    all(purrr:::map_lgl(extract_1_2$.extracts, ~ inherits(.x, "try-error"))),
  )

})

# ------------------------------------------------------------------------------

test_that('tune model only', {
  extr_2_1 <- function(x) {
    tibble(penalty = x$model$lambda, df = x$model$df)
  }


  expect_error(
    res_2_1 <-
      workflow() %>%
      add_recipe(rec_no_tune_1) %>%
      add_model(glmn_mod) %>%
      tune_grid(rs = mt_folds, control = grid_control(extract = extr_2_1)),
    NA
  )
  expect_error(extract_2_1 <- dplyr::bind_rows(res_2_1$.extracts), NA)

  expect_true(all(names(extract_2_1) == c("penalty", "mixture", ".extracts")))
  expect_true(
    all(purrr:::map_lgl(extract_2_1$.extracts, ~ tibble::is_tibble(.x))),
  )
  expect_true(
    all(purrr:::map_lgl(extract_2_1$.extracts, ~ all(names(.x) == c("penalty", "df")))),
  )

  extr_2_2 <- function(x) {
    tibble(is_null_rec = is.null(x$recipe))
  }

  # should not fail:
  expect_error(
    res_2_2 <-
      workflow() %>%
      add_recipe(rec_tune_1) %>%
      add_model(lm_mod) %>%
      tune_grid(rs = mt_folds, control = grid_control(extract = extr_2_2)),
    NA
  )

  expect_error(extract_2_2 <- dplyr::bind_rows(res_2_2$.extracts) %>% tidyr::unnest(), NA)
  expect_true(all(!extract_2_2$is_null_rec))

})

# ------------------------------------------------------------------------------

test_that('tune model and recipe', {
  extr_3_1 <- function(x) {
    x
  }

  wflow_3 <-
    workflow() %>%
    add_recipe(rec_tune_1) %>%
    add_model(glmn_mod)
  set.seed(35)
  grid_3 <-
    param_set(wflow_3) %>%
    update("num_comp", num_comp(c(2, 5))) %>%
    grid_latin_hypercube()

  expect_error(
    res_3_1 <- tune_grid(wflow_3, rs = mt_folds, grid = grid_3,
                         control = grid_control(extract = extr_3_1)),
    NA
  )
  expect_error(extract_3_1 <- dplyr::bind_rows(res_3_1$.extracts), NA)

  expect_true(all(names(extract_3_1) == c("num_comp", "penalty", "mixture", ".extracts")))
  expect_true(
    all(purrr:::map_lgl(extract_3_1$.extracts, ~ is.list(.x))),
  )
  expect_true(
    all(purrr:::map_lgl(extract_3_1$.extracts, ~ all(names(.x) == c("recipe", "model")))),
  )
  expect_true(
    all(purrr:::map_lgl(extract_3_1$.extracts, ~ inherits(.x$recipe, "recipe"))),
  )
  expect_true(
    all(purrr:::map_lgl(extract_3_1$.extracts, ~ inherits(.x$model, "glmnet"))),
  )

})

