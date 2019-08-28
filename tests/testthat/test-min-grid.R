context("grid search")

# ------------------------------------------------------------------------------

library(dplyr)

source("../helper-objects.R")

# ------------------------------------------------------------------------------

test_that('boosted tree grid reduction - xgboost', {
  mod <- boost_tree() %>% set_engine("xgboost")

  # A typical grid
  reg_grid <- expand.grid(trees = 1:3, min_n = 1:2)
  reg_grid_smol <- min_grid(mod, reg_grid)

  expect_equal(reg_grid_smol$trees, rep(3, 2))
  expect_equal(reg_grid_smol$min_n, 1:2)
  for (i in 1:nrow(reg_grid_smol)) {
    expect_equal(reg_grid_smol$.submodels[[i]], list(trees = 1:2))
  }

  # Unbalanced grid
  reg_ish_grid <- expand.grid(trees = 1:3, min_n = 1:2)[-3,]
  reg_ish_grid_smol <- min_grid(mod, reg_ish_grid)

  expect_equal(reg_ish_grid_smol$trees, 2:3)
  expect_equal(reg_ish_grid_smol$min_n, 1:2)
  for (i in 2:nrow(reg_ish_grid_smol)) {
    expect_equal(reg_ish_grid_smol$.submodels[[i]], list(trees = 1:2))
  }

  # Grid with a third parameter
  reg_grid_extra <- expand.grid(trees = 1:3, min_n = 1:2, tree_depth = 10:12)
  reg_grid_extra_smol <- min_grid(mod, reg_grid_extra)

  expect_equal(reg_grid_extra_smol$trees, rep(3, 6))
  expect_equal(reg_grid_extra_smol$min_n, rep(1:2, 3))
  expect_equal(reg_grid_extra_smol$tree_depth, rep(10:12, each = 2))
  for (i in 1:nrow(reg_grid_extra_smol)) {
    expect_equal(reg_grid_extra_smol$.submodels[[i]], list(trees = 1:2))
  }

  # Only trees
  only_trees <- expand.grid(trees = 1:3)
  only_trees_smol <- min_grid(mod, only_trees)

  expect_equal(only_trees_smol$trees, 3)
  expect_equal(only_trees_smol$.submodels, list(list(trees = 1:2)))

  # No submodels
  no_sub <- tibble(trees = 1, min_n = 1:2)
  no_sub_smol <- min_grid(mod, no_sub)

  expect_equal(no_sub_smol$trees, rep(1, 2))
  expect_equal(no_sub_smol$min_n, 1:2)
  for (i in 1:nrow(no_sub_smol)) {
    expect_null(no_sub_smol$.submodels[[i]])
  }

})

# ------------------------------------------------------------------------------

test_that('boosted tree grid reduction - C5.0', {
  mod <- boost_tree() %>% set_engine("C5.0")

  # A typical grid
  reg_grid <- expand.grid(trees = 1:3, min_n = 1:2)
  reg_grid_smol <- min_grid(mod, reg_grid)

  expect_equal(reg_grid_smol$trees, rep(3, 2))
  expect_equal(reg_grid_smol$min_n, 1:2)
  for (i in 1:nrow(reg_grid_smol)) {
    expect_equal(reg_grid_smol$.submodels[[i]], list(trees = 1:2))
  }

  # Unbalanced grid
  reg_ish_grid <- expand.grid(trees = 1:3, min_n = 1:2)[-3,]
  reg_ish_grid_smol <- min_grid(mod, reg_ish_grid)

  expect_equal(reg_ish_grid_smol$trees, 2:3)
  expect_equal(reg_ish_grid_smol$min_n, 1:2)
  for (i in 2:nrow(reg_ish_grid_smol)) {
    expect_equal(reg_ish_grid_smol$.submodels[[i]], list(trees = 1:2))
  }

  # Grid with a third parameter
  reg_grid_extra <- expand.grid(trees = 1:3, min_n = 1:2, tree_depth = 10:12)
  reg_grid_extra_smol <- min_grid(mod, reg_grid_extra)

  expect_equal(reg_grid_extra_smol$trees, rep(3, 6))
  expect_equal(reg_grid_extra_smol$min_n, rep(1:2, each = 3))
  expect_equal(reg_grid_extra_smol$tree_depth, rep(10:12, 2))
  for (i in 1:nrow(reg_grid_extra_smol)) {
    expect_equal(reg_grid_extra_smol$.submodels[[i]], list(trees = 1:2))
  }

  # Only trees
  only_trees <- expand.grid(trees = 1:3)
  only_trees_smol <- min_grid(mod, only_trees)

  expect_equal(only_trees_smol$trees, 3)
  expect_equal(only_trees_smol$.submodels, list(list(trees = 1:2)))

  # No submodels
  no_sub <- tibble(trees = 1, min_n = 1:2)
  no_sub_smol <- min_grid(mod, no_sub)

  expect_equal(no_sub_smol$trees, rep(1, 2))
  expect_equal(no_sub_smol$min_n, 1:2)
  for (i in 1:nrow(no_sub_smol)) {
    expect_null(no_sub_smol$.submodels[[i]])
  }


})

# ------------------------------------------------------------------------------


test_that('linear regression grid reduction - glmnet', {
  mod <- linear_reg() %>% set_engine("glmnet")

  # A typical grid
  reg_grid <- expand.grid(penalty = 1:3, mixture = (1:5)/5)
  reg_grid_smol <- min_grid(mod, reg_grid)

  expect_equal(reg_grid_smol$penalty, rep(3, 5))
  expect_equal(reg_grid_smol$mixture, (1:5)/5)
  for (i in 1:nrow(reg_grid_smol)) {
    expect_equal(reg_grid_smol$.submodels[[i]], list(penalty = 1:2))
  }

  # Unbalanced grid
  reg_ish_grid <- expand.grid(penalty = 1:3, mixture = (1:5)/5)[-3,]
  reg_ish_grid_smol <- min_grid(mod, reg_ish_grid)

  expect_equal(reg_ish_grid_smol$penalty, c(2, rep(3, 4)))
  expect_equal(reg_ish_grid_smol$mixture, (1:5)/5)
  expect_equal(reg_ish_grid_smol$.submodels[[1]], list(penalty = 1))
  for (i in 2:nrow(reg_ish_grid_smol)) {
    expect_equal(reg_ish_grid_smol$.submodels[[i]], list(penalty = 1:2))
  }

  # Grid with a third parameter
  reg_grid_extra <- expand.grid(penalty = 1:3, mixture = (1:5)/5, blah = 10:12)
  reg_grid_extra_smol <- min_grid(mod, reg_grid_extra)

  expect_equal(reg_grid_extra_smol$penalty, rep(3, 15))
  expect_equal(reg_grid_extra_smol$mixture, rep((1:5)/5, each = 3))
  expect_equal(reg_grid_extra_smol$blah, rep(10:12, 5))
  for (i in 1:nrow(reg_grid_extra_smol)) {
    expect_equal(reg_grid_extra_smol$.submodels[[i]], list(penalty = 1:2))
  }

  # Penaly not specified
  expect_error(min_grid(mod, data.frame(mixture = 1:3)),
               "At least one penalty value is required for glmnet")

  # Only penalty
  only_penalty <- expand.grid(penalty = 1:3)
  only_penalty_smol <- min_grid(mod, only_penalty)

  expect_equal(only_penalty_smol$penalty, 3)
  expect_equal(only_penalty_smol$.submodels, list(list(penalty = 1:2)))

  # No submodels
  no_sub <- tibble(penalty = 1:5, mixture = (1:5)/5)
  no_sub_smol <- min_grid(mod, no_sub)

  expect_equal(no_sub_smol$penalty, 1:5)
  expect_equal(no_sub_smol$mixture, (1:5)/5)
  for (i in 1:nrow(no_sub_smol)) {
    expect_null(no_sub_smol$.submodels[[i]])
  }

})

# ------------------------------------------------------------------------------

test_that('logistic regression grid reduction - glmnet', {
  mod <- logistic_reg() %>% set_engine("glmnet")

  # A typical grid
  reg_grid <- expand.grid(penalty = 1:3, mixture = (1:5)/5)
  reg_grid_smol <- min_grid(mod, reg_grid)

  expect_equal(reg_grid_smol$penalty, rep(3, 5))
  expect_equal(reg_grid_smol$mixture, (1:5)/5)
  for (i in 1:nrow(reg_grid_smol)) {
    expect_equal(reg_grid_smol$.submodels[[i]], list(penalty = 1:2))
  }

  # Unbalanced grid
  reg_ish_grid <- expand.grid(penalty = 1:3, mixture = (1:5)/5)[-3,]
  reg_ish_grid_smol <- min_grid(mod, reg_ish_grid)

  expect_equal(reg_ish_grid_smol$penalty, c(2, rep(3, 4)))
  expect_equal(reg_ish_grid_smol$mixture, (1:5)/5)
  expect_equal(reg_ish_grid_smol$.submodels[[1]], list(penalty = 1))
  for (i in 2:nrow(reg_ish_grid_smol)) {
    expect_equal(reg_ish_grid_smol$.submodels[[i]], list(penalty = 1:2))
  }

  # Grid with a third parameter
  reg_grid_extra <- expand.grid(penalty = 1:3, mixture = (1:5)/5, blah = 10:12)
  reg_grid_extra_smol <- min_grid(mod, reg_grid_extra)

  expect_equal(reg_grid_extra_smol$penalty, rep(3, 15))
  expect_equal(reg_grid_extra_smol$mixture, rep((1:5)/5, each = 3))
  expect_equal(reg_grid_extra_smol$blah, rep(10:12, 5))
  for (i in 1:nrow(reg_grid_extra_smol)) {
    expect_equal(reg_grid_extra_smol$.submodels[[i]], list(penalty = 1:2))
  }

  # Penaly not specified
  expect_error(min_grid(mod, data.frame(mixture = 1:3)),
               "At least one penalty value is required for glmnet")

  # Only penalty
  only_penalty <- expand.grid(penalty = 1:3)
  only_penalty_smol <- min_grid(mod, only_penalty)

  expect_equal(only_penalty_smol$penalty, 3)
  expect_equal(only_penalty_smol$.submodels, list(list(penalty = 1:2)))

  # No submodels
  no_sub <- tibble(penalty = 1:5, mixture = (1:5)/5)
  no_sub_smol <- min_grid(mod, no_sub)

  expect_equal(no_sub_smol$penalty, 1:5)
  expect_equal(no_sub_smol$mixture, (1:5)/5)
  for (i in 1:nrow(no_sub_smol)) {
    expect_null(no_sub_smol$.submodels[[i]])
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
  mod <- mars() %>% set_engine("earth")

  # A typical grid
  reg_grid <- expand.grid(num_terms = 1:3, prod_degree = 1:2)
  reg_grid_smol <- min_grid(mod, reg_grid)

  expect_equal(reg_grid_smol$num_terms, rep(3, 2))
  expect_equal(reg_grid_smol$prod_degree, 1:2)
  for (i in 1:nrow(reg_grid_smol)) {
    expect_equal(reg_grid_smol$.submodels[[i]], list(num_terms = 1:2))
  }

  # Unbalanced grid
  reg_ish_grid <- expand.grid(num_terms = 1:3, prod_degree = 1:2)[-3,]
  reg_ish_grid_smol <- min_grid(mod, reg_ish_grid)

  expect_equal(reg_ish_grid_smol$num_terms, 2:3)
  expect_equal(reg_ish_grid_smol$prod_degree, 1:2)
  for (i in 2:nrow(reg_ish_grid_smol)) {
    expect_equal(reg_ish_grid_smol$.submodels[[i]], list(num_terms = 1:2))
  }

  # Grid with a third parameter
  reg_grid_extra <- expand.grid(num_terms = 1:3, prod_degree = 1:2, blah = 10:12)
  reg_grid_extra_smol <- min_grid(mod, reg_grid_extra)

  expect_equal(reg_grid_extra_smol$num_terms, rep(3, 6))
  expect_equal(reg_grid_extra_smol$prod_degree, rep(1:2, each = 3))
  expect_equal(reg_grid_extra_smol$blah, rep(10:12, 2))
  for (i in 1:nrow(reg_grid_extra_smol)) {
    expect_equal(reg_grid_extra_smol$.submodels[[i]], list(num_terms = 1:2))
  }

  # Only num_terms
  only_num_terms <- expand.grid(num_terms = 1:3)
  only_num_terms_smol <- min_grid(mod, only_num_terms)

  expect_equal(only_num_terms_smol$num_terms, 3)
  expect_equal(only_num_terms_smol$.submodels, list(list(num_terms = 1:2)))

  # No submodels
  no_sub <- tibble(num_terms = 1, prod_degree = 1:2)
  no_sub_smol <- min_grid(mod, no_sub)

  expect_equal(no_sub_smol$num_terms, rep(1, 2))
  expect_equal(no_sub_smol$prod_degree, 1:2)
  for (i in 1:nrow(no_sub_smol)) {
    expect_null(no_sub_smol$.submodels[[i]])
  }

})

# ------------------------------------------------------------------------------

test_that('multinomial regression grid reduction - glmnet', {
  mod <- multinom_reg() %>% set_engine("glmnet")

  # A typical grid
  reg_grid <- expand.grid(penalty = 1:3, mixture = (1:5)/5)
  reg_grid_smol <- min_grid(mod, reg_grid)

  expect_equal(reg_grid_smol$penalty, rep(3, 5))
  expect_equal(reg_grid_smol$mixture, (1:5)/5)
  for (i in 1:nrow(reg_grid_smol)) {
    expect_equal(reg_grid_smol$.submodels[[i]], list(penalty = 1:2))
  }

  # Unbalanced grid
  reg_ish_grid <- expand.grid(penalty = 1:3, mixture = (1:5)/5)[-3,]
  reg_ish_grid_smol <- min_grid(mod, reg_ish_grid)

  expect_equal(reg_ish_grid_smol$penalty, c(2, rep(3, 4)))
  expect_equal(reg_ish_grid_smol$mixture, (1:5)/5)
  expect_equal(reg_ish_grid_smol$.submodels[[1]], list(penalty = 1))
  for (i in 2:nrow(reg_ish_grid_smol)) {
    expect_equal(reg_ish_grid_smol$.submodels[[i]], list(penalty = 1:2))
  }

  # Grid with a third parameter
  reg_grid_extra <- expand.grid(penalty = 1:3, mixture = (1:5)/5, blah = 10:12)
  reg_grid_extra_smol <- min_grid(mod, reg_grid_extra)

  expect_equal(reg_grid_extra_smol$penalty, rep(3, 15))
  expect_equal(reg_grid_extra_smol$mixture, rep((1:5)/5, each = 3))
  expect_equal(reg_grid_extra_smol$blah, rep(10:12, 5))
  for (i in 1:nrow(reg_grid_extra_smol)) {
    expect_equal(reg_grid_extra_smol$.submodels[[i]], list(penalty = 1:2))
  }

  # Penaly not specified
  expect_error(min_grid(mod, data.frame(mixture = 1:3)),
               "At least one penalty value is required for glmnet")

  # Only penalty
  only_penalty <- expand.grid(penalty = 1:3)
  only_penalty_smol <- min_grid(mod, only_penalty)

  expect_equal(only_penalty_smol$penalty, 3)
  expect_equal(only_penalty_smol$.submodels, list(list(penalty = 1:2)))

  # No submodels
  no_sub <- tibble(penalty = 1:5, mixture = (1:5)/5)
  no_sub_smol <- min_grid(mod, no_sub)

  expect_equal(no_sub_smol$penalty, 1:5)
  expect_equal(no_sub_smol$mixture, (1:5)/5)
  for (i in 1:nrow(no_sub_smol)) {
    expect_null(no_sub_smol$.submodels[[i]])
  }

})

# ------------------------------------------------------------------------------


test_that('nearest neighbors grid reduction - kknn', {
  mod <- nearest_neighbor() %>% set_engine("kknn")

  # A typical grid
  reg_grid <- expand.grid(neighbors = 1:3, dist_power = 1:2)
  reg_grid_smol <- min_grid(mod, reg_grid)

  expect_equal(reg_grid_smol$neighbors, rep(3, 2))
  expect_equal(reg_grid_smol$dist_power, 1:2)
  for (i in 1:nrow(reg_grid_smol)) {
    expect_equal(reg_grid_smol$.submodels[[i]], list(neighbors = 1:2))
  }

  # Unbalanced grid
  reg_ish_grid <- expand.grid(neighbors = 1:3, dist_power = 1:2)[-3,]
  reg_ish_grid_smol <- min_grid(mod, reg_ish_grid)

  expect_equal(reg_ish_grid_smol$neighbors, 2:3)
  expect_equal(reg_ish_grid_smol$dist_power, 1:2)
  for (i in 2:nrow(reg_ish_grid_smol)) {
    expect_equal(reg_ish_grid_smol$.submodels[[i]], list(neighbors = 1:2))
  }

  # Grid with a third parameter
  wts <- c('rectangular', 'triangular', 'epanechnikov')
  reg_grid_extra <- expand.grid(neighbors = 1:3, dist_power = 1:2, weight_func = wts)
  reg_grid_extra_smol <- min_grid(mod, reg_grid_extra)

  expect_equal(reg_grid_extra_smol$neighbors, rep(3, 6))
  expect_equal(reg_grid_extra_smol$dist_power, rep(1:2, 3))
  expect_equal(reg_grid_extra_smol$weight_func, rep(wts, each = 2))
  for (i in 1:nrow(reg_grid_extra_smol)) {
    expect_equal(reg_grid_extra_smol$.submodels[[i]], list(neighbors = 1:2))
  }

  # Only neighbors
  only_neighbors <- expand.grid(neighbors = 1:3)
  only_neighbors_smol <- min_grid(mod, only_neighbors)

  expect_equal(only_neighbors_smol$neighbors, 3)
  expect_equal(only_neighbors_smol$.submodels, list(list(neighbors = 1:2)))

  # No submodels
  no_sub <- tibble(neighbors = 1, dist_power = 1:2)
  no_sub_smol <- min_grid(mod, no_sub)

  expect_equal(no_sub_smol$neighbors, rep(1, 2))
  expect_equal(no_sub_smol$dist_power, 1:2)
  for (i in 1:nrow(no_sub_smol)) {
    expect_null(no_sub_smol$.submodels[[i]])
  }
})
