context("grid search")

# ------------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(recipes)
library(workflows)
library(dials)
library(rsample)
library(kernlab)
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

svm_mod <- svm_rbf(mode = "regression", cost = tune()) %>% set_engine("kernlab")

# ------------------------------------------------------------------------------

test_that('tune recipe only', {
  set.seed(4400)
  wflow <- workflow() %>% add_recipe(rec_tune_1) %>% add_model(lm_mod)
  pset <- param_set(wflow) %>% update("num_comp", num_comp(c(1, 3)))
  grid <- grid_regular(pset, levels = 3)
  folds <- vfold_cv(mtcars)
  res <- tune_grid(wflow, rs = folds, grid = grid)
  expect_equal(res$id, folds$id)
  res_est <- summarize(res)
  expect_equal(nrow(res_est), nrow(grid) * 2)
  expect_equal(sum(res_est$.metric == "rmse"), nrow(grid))
  expect_equal(sum(res_est$.metric == "rsq"), nrow(grid))
  expect_equal(res_est$n, rep(10, nrow(grid) * 2))
})

# ------------------------------------------------------------------------------

test_that('tune model only (with recipe)', {
  set.seed(4400)
  wflow <- workflow() %>% add_recipe(rec_no_tune_1) %>% add_model(svm_mod)
  pset <- param_set(wflow)
  grid <- grid_regular(pset, levels = 3)
  folds <- vfold_cv(mtcars)
  res <- tune_grid(wflow, rs = folds, grid = grid)
  expect_equal(res$id, folds$id)
  res_est <- summarize(res)
  expect_equal(nrow(res_est), nrow(grid) * 2)
  expect_equal(sum(res_est$.metric == "rmse"), nrow(grid))
  expect_equal(sum(res_est$.metric == "rsq"), nrow(grid))
  expect_equal(res_est$n, rep(10, nrow(grid) * 2))
})

# ------------------------------------------------------------------------------

test_that('tune model only (with recipe, multi-predict)', {
  set.seed(4400)
  wflow <- workflow() %>% add_recipe(rec_no_tune_1) %>% add_model(glmn)
  pset <- param_set(wflow)
  grid <- grid_regular(pset, levels = 3)
  folds <- vfold_cv(mtcars)
  res <- tune_grid(wflow, rs = folds, grid = grid)
  expect_equal(res$id, folds$id)
  res_est <- summarize(res)
  expect_equal(nrow(res_est), nrow(grid) * 2)
  expect_equal(sum(res_est$.metric == "rmse"), nrow(grid))
  expect_equal(sum(res_est$.metric == "rsq"), nrow(grid))
  expect_equal(res_est$n, rep(10, nrow(grid) * 2))
})

# ------------------------------------------------------------------------------

test_that('tune model and recipe', {
  set.seed(4400)
  wflow <- workflow() %>% add_recipe(rec_tune_1) %>% add_model(svm_mod)
  pset <- param_set(wflow) %>% update("num_comp", num_comp(c(1, 3)))
  grid <- grid_regular(pset, levels = 3)
  folds <- vfold_cv(mtcars)
  res <- tune_grid(wflow, rs = folds, grid = grid)
  expect_equal(res$id, folds$id)
  res_est <- summarize(res)
  expect_equal(nrow(res_est), nrow(grid) * 2)
  expect_equal(sum(res_est$.metric == "rmse"), nrow(grid))
  expect_equal(sum(res_est$.metric == "rsq"), nrow(grid))
  expect_equal(res_est$n, rep(10, nrow(grid) * 2))
})

# ------------------------------------------------------------------------------

test_that('tune model and recipe (multi-predict)', {
  set.seed(4400)
  wflow <- workflow() %>% add_recipe(rec_tune_1) %>% add_model(glmn)
  pset <- param_set(wflow) %>% update("num_comp", num_comp(c(2, 3)))
  grid <- grid_regular(pset, levels = c(3, 3, 2))
  folds <- vfold_cv(mtcars)
  res <- tune_grid(wflow, rs = folds, grid = grid)
  expect_equal(res$id, folds$id)
  res_est <- summarize(res)
  expect_equal(nrow(res_est), nrow(grid) * 2)
  expect_equal(sum(res_est$.metric == "rmse"), nrow(grid))
  expect_equal(sum(res_est$.metric == "rsq"), nrow(grid))
  expect_equal(res_est$n, rep(10, nrow(grid) * 2))
})



