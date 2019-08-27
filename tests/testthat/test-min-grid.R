context("grid search")

# ------------------------------------------------------------------------------

library(dplyr)

source("../helper-objects.R")

# ------------------------------------------------------------------------------

test_that('boosted tree grid reduction - xgboost', {
  reg_grid <- expand.grid(trees = 1:3, learn_rate = (1:5)/5)
  reg_grid_smol <- min_grid(boost_tree() %>% set_engine("xgboost"), reg_grid)

  expect_equal(reg_grid_smol$trees, rep(3, 5))
  expect_equal(reg_grid_smol$learn_rate, (1:5)/5)
  for (i in 1:nrow(reg_grid_smol)) {
    expect_equal(reg_grid_smol$.submodels[[i]], list(trees = 1:2))
  }

  reg_ish_grid <- expand.grid(trees = 1:3, learn_rate = (1:5)/5)[-3,]
  reg_ish_grid_smol <- min_grid(boost_tree() %>% set_engine("xgboost"), reg_ish_grid)

  expect_equal(reg_ish_grid_smol$trees, c(2, rep(3, 4)))
  expect_equal(reg_ish_grid_smol$learn_rate, (1:5)/5)
  expect_equal(reg_ish_grid_smol$.submodels[[1]], list(trees = 1))
  for (i in 2:nrow(reg_ish_grid_smol)) {
    expect_equal(reg_ish_grid_smol$.submodels[[i]], list(trees = 1:2))
  }

  reg_grid_extra <- expand.grid(trees = 1:3, learn_rate = (1:5)/5, blah = 10:12)
  reg_grid_extra_smol <- min_grid(boost_tree() %>% set_engine("xgboost"), reg_grid_extra)

  expect_equal(reg_grid_extra_smol$trees, rep(3, 15))
  expect_equal(reg_grid_extra_smol$learn_rate, rep((1:5)/5, each = 3))
  expect_equal(reg_grid_extra_smol$blah, rep(10:12, 5))
  for (i in 1:nrow(reg_grid_extra_smol)) {
    expect_equal(reg_grid_extra_smol$.submodels[[i]], list(trees = 1:2))
  }

})

# ------------------------------------------------------------------------------


test_that('linear regression grid reduction - glmnet', {
  reg_grid <- expand.grid(penalty = 1:3, mixture = (1:5)/5)
  reg_grid_smol <- min_grid(linear_reg() %>% set_engine("glmnet"), reg_grid)

  expect_equal(reg_grid_smol$penalty, rep(3, 5))
  expect_equal(reg_grid_smol$mixture, (1:5)/5)
  for (i in 1:nrow(reg_grid_smol)) {
    expect_equal(reg_grid_smol$.submodels[[i]], list(penalty = 1:2))
  }

  reg_ish_grid <- expand.grid(penalty = 1:3, mixture = (1:5)/5)[-3,]
  reg_ish_grid_smol <- min_grid(linear_reg() %>% set_engine("glmnet"), reg_ish_grid)

  expect_equal(reg_ish_grid_smol$penalty, c(2, rep(3, 4)))
  expect_equal(reg_ish_grid_smol$mixture, (1:5)/5)
  expect_equal(reg_ish_grid_smol$.submodels[[1]], list(penalty = 1))
  for (i in 2:nrow(reg_ish_grid_smol)) {
    expect_equal(reg_ish_grid_smol$.submodels[[i]], list(penalty = 1:2))
  }

  reg_grid_extra <- expand.grid(penalty = 1:3, mixture = (1:5)/5, blah = 10:12)
  reg_grid_extra_smol <- min_grid(linear_reg() %>% set_engine("glmnet"), reg_grid_extra)

  expect_equal(reg_grid_extra_smol$penalty, rep(3, 15))
  expect_equal(reg_grid_extra_smol$mixture, rep((1:5)/5, each = 3))
  expect_equal(reg_grid_extra_smol$blah, rep(10:12, 5))
  for (i in 1:nrow(reg_grid_extra_smol)) {
    expect_equal(reg_grid_extra_smol$.submodels[[i]], list(penalty = 1:2))
  }

})

# ------------------------------------------------------------------------------


test_that('logistic regression grid reduction - glmnet', {
  reg_grid <- expand.grid(penalty = 1:3, mixture = (1:5)/5)
  reg_grid_smol <- min_grid(logistic_reg() %>% set_engine("glmnet"), reg_grid)

  expect_equal(reg_grid_smol$penalty, rep(3, 5))
  expect_equal(reg_grid_smol$mixture, (1:5)/5)
  for (i in 1:nrow(reg_grid_smol)) {
    expect_equal(reg_grid_smol$.submodels[[i]], list(penalty = 1:2))
  }

  reg_ish_grid <- expand.grid(penalty = 1:3, mixture = (1:5)/5)[-3,]
  reg_ish_grid_smol <- min_grid(logistic_reg() %>% set_engine("glmnet"), reg_ish_grid)

  expect_equal(reg_ish_grid_smol$penalty, c(2, rep(3, 4)))
  expect_equal(reg_ish_grid_smol$mixture, (1:5)/5)
  expect_equal(reg_ish_grid_smol$.submodels[[1]], list(penalty = 1))
  for (i in 2:nrow(reg_ish_grid_smol)) {
    expect_equal(reg_ish_grid_smol$.submodels[[i]], list(penalty = 1:2))
  }

  reg_grid_extra <- expand.grid(penalty = 1:3, mixture = (1:5)/5, blah = 10:12)
  reg_grid_extra_smol <- min_grid(logistic_reg() %>% set_engine("glmnet"), reg_grid_extra)

  expect_equal(reg_grid_extra_smol$penalty, rep(3, 15))
  expect_equal(reg_grid_extra_smol$mixture, rep((1:5)/5, each = 3))
  expect_equal(reg_grid_extra_smol$blah, rep(10:12, 5))
  for (i in 1:nrow(reg_grid_extra_smol)) {
    expect_equal(reg_grid_extra_smol$.submodels[[i]], list(penalty = 1:2))
  }

})

# more of a negative control test
test_that('logistic regression grid reduction - spark', {
  reg_grid <- expand.grid(penalty = 1:3, mixture = (1:5)/5)
  reg_grid_smol <- min_grid(logistic_reg() %>% set_engine("spark"), reg_grid)

  expect_equal(reg_grid_smol$penalty, reg_grid$penalty)
  expect_equal(reg_grid_smol$mixture, reg_grid$mixture)
  for (i in 1:nrow(reg_grid_smol)) {
    expect_equal(reg_grid_smol$.submodels[[i]], list())
  }
})

# ------------------------------------------------------------------------------

test_that('MARS grid reduction - earth', {
  reg_grid <- expand.grid(num_terms = 1:3, prod_degree = 1:2)
  reg_grid_smol <- min_grid(mars() %>% set_engine("earth"), reg_grid)

  expect_equal(reg_grid_smol$num_terms, rep(3, 2))
  expect_equal(reg_grid_smol$prod_degree, 1:2)
  for (i in 1:nrow(reg_grid_smol)) {
    expect_equal(reg_grid_smol$.submodels[[i]], list(num_terms = 1:2))
  }

  reg_ish_grid <- expand.grid(num_terms = 1:3, prod_degree = 1:2)[-3,]
  reg_ish_grid_smol <- min_grid(mars() %>% set_engine("earth"), reg_ish_grid)

  expect_equal(reg_ish_grid_smol$num_terms, 2:3)
  expect_equal(reg_ish_grid_smol$prod_degree, 1:2)
  expect_equal(reg_ish_grid_smol$.submodels[[1]], list(num_terms = 1))
  for (i in 2:nrow(reg_ish_grid_smol)) {
    expect_equal(reg_ish_grid_smol$.submodels[[i]], list(num_terms = 1:2))
  }

  reg_grid_extra <- expand.grid(num_terms = 1:3, prod_degree = 1:2, blah = 10:12)
  reg_grid_extra_smol <- min_grid(mars() %>% set_engine("earth"), reg_grid_extra)

  expect_equal(reg_grid_extra_smol$num_terms, rep(3, 6))
  expect_equal(reg_grid_extra_smol$prod_degree, rep(1:2, each = 3))
  expect_equal(reg_grid_extra_smol$blah, rep(10:12, 2))
  for (i in 1:nrow(reg_grid_extra_smol)) {
    expect_equal(reg_grid_extra_smol$.submodels[[i]], list(num_terms = 1:2))
  }

})

# ------------------------------------------------------------------------------

test_that('multinomial grid reduction - glmnet', {
  reg_grid <- expand.grid(penalty = 1:3, mixture = (1:5)/5)
  reg_grid_smol <- min_grid(multinom_reg() %>% set_engine("glmnet"), reg_grid)

  expect_equal(reg_grid_smol$penalty, rep(3, 5))
  expect_equal(reg_grid_smol$mixture, (1:5)/5)
  for (i in 1:nrow(reg_grid_smol)) {
    expect_equal(reg_grid_smol$.submodels[[i]], list(penalty = 1:2))
  }

  reg_ish_grid <- expand.grid(penalty = 1:3, mixture = (1:5)/5)[-3,]
  reg_ish_grid_smol <- min_grid(multinom_reg() %>% set_engine("glmnet"), reg_ish_grid)

  expect_equal(reg_ish_grid_smol$penalty, c(2, rep(3, 4)))
  expect_equal(reg_ish_grid_smol$mixture, (1:5)/5)
  expect_equal(reg_ish_grid_smol$.submodels[[1]], list(penalty = 1))
  for (i in 2:nrow(reg_ish_grid_smol)) {
    expect_equal(reg_ish_grid_smol$.submodels[[i]], list(penalty = 1:2))
  }

  reg_grid_extra <- expand.grid(penalty = 1:3, mixture = (1:5)/5, blah = 10:12)
  reg_grid_extra_smol <- min_grid(multinom_reg() %>% set_engine("glmnet"), reg_grid_extra)

  expect_equal(reg_grid_extra_smol$penalty, rep(3, 15))
  expect_equal(reg_grid_extra_smol$mixture, rep((1:5)/5, each = 3))
  expect_equal(reg_grid_extra_smol$blah, rep(10:12, 5))
  for (i in 1:nrow(reg_grid_extra_smol)) {
    expect_equal(reg_grid_extra_smol$.submodels[[i]], list(penalty = 1:2))
  }

})

# ------------------------------------------------------------------------------


test_that('nearest neighbors grid reduction - kknn', {
  reg_grid <- expand.grid(neighbors = 1:3, prod_degree = 1:2)
  reg_grid_smol <- min_grid(nearest_neighbor() %>% set_engine("kknn"), reg_grid)

  expect_equal(reg_grid_smol$neighbors, rep(3, 2))
  expect_equal(reg_grid_smol$prod_degree, 1:2)
  for (i in 1:nrow(reg_grid_smol)) {
    expect_equal(reg_grid_smol$.submodels[[i]], list(neighbors = 1:2))
  }

  reg_ish_grid <- expand.grid(neighbors = 1:3, prod_degree = 1:2)[-3,]
  reg_ish_grid_smol <- min_grid(nearest_neighbor() %>% set_engine("kknn"), reg_ish_grid)

  expect_equal(reg_ish_grid_smol$neighbors, 2:3)
  expect_equal(reg_ish_grid_smol$prod_degree, 1:2)
  expect_equal(reg_ish_grid_smol$.submodels[[1]], list(neighbors = 1))
  for (i in 2:nrow(reg_ish_grid_smol)) {
    expect_equal(reg_ish_grid_smol$.submodels[[i]], list(neighbors = 1:2))
  }

  reg_grid_extra <- expand.grid(neighbors = 1:3, prod_degree = 1:2, blah = 10:12)
  reg_grid_extra_smol <- min_grid(nearest_neighbor() %>% set_engine("kknn"), reg_grid_extra)

  expect_equal(reg_grid_extra_smol$neighbors, rep(3, 6))
  expect_equal(reg_grid_extra_smol$prod_degree, rep(1:2, each = 3))
  expect_equal(reg_grid_extra_smol$blah, rep(10:12, 2))
  for (i in 1:nrow(reg_grid_extra_smol)) {
    expect_equal(reg_grid_extra_smol$.submodels[[i]], list(neighbors = 1:2))
  }

})
