# ------------------------------------------------------------------------------

test_that("boosted tree grid reduction - xgboost", {
  mod <- parsnip::boost_tree() %>%
    parsnip::set_engine("xgboost")

  # A typical grid
  reg_grid <- expand.grid(trees = 1:3, min_n = 1:2)
  reg_grid_smol <- min_grid(mod, reg_grid)

  expect_equal(reg_grid_smol$trees, rep(3, 2))
  expect_equal(reg_grid_smol$min_n, 1:2)
  for (i in 1:nrow(reg_grid_smol)) {
    expect_equal(reg_grid_smol$.submodels[[i]], list(trees = 1:2))
  }

  # Unbalanced grid
  reg_ish_grid <- expand.grid(trees = 1:3, min_n = 1:2)[-3, ]
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
  no_sub <- tibble::tibble(trees = 1, min_n = 1:2)
  no_sub_smol <- min_grid(mod, no_sub)

  expect_equal(no_sub_smol$trees, rep(1, 2))
  expect_equal(no_sub_smol$min_n, 1:2)
  for (i in 1:nrow(no_sub_smol)) {
    expect_length(no_sub_smol$.submodels[[i]], 0)
  }

  # different id names
  mod_1 <- parsnip::boost_tree(trees = tune("Amos")) %>%
    parsnip::set_engine("xgboost")
  reg_grid <- expand.grid(Amos = 1:3, min_n = 1:2)
  reg_grid_smol <- min_grid(mod_1, reg_grid)

  expect_equal(reg_grid_smol$Amos, rep(3, 2))
  expect_equal(reg_grid_smol$min_n, 1:2)
  for (i in 1:nrow(reg_grid_smol)) {
    expect_equal(reg_grid_smol$.submodels[[i]], list(Amos = 1:2))
  }

  all_sub <- expand.grid(Amos = 1:3)
  all_sub_smol <- min_grid(mod_1, all_sub)

  expect_equal(all_sub_smol$Amos, 3)
  expect_equal(all_sub_smol$.submodels[[1]], list(Amos = 1:2))

  mod_2 <- parsnip::boost_tree(trees = tune("Ade Tukunbo")) %>%
    parsnip::set_engine("xgboost")
  reg_grid <- expand.grid(`Ade Tukunbo` = 1:3, min_n = 1:2, ` \t123` = 10:11)
  reg_grid_smol <- min_grid(mod_2, reg_grid)

  expect_equal(reg_grid_smol$`Ade Tukunbo`, rep(3, 4))
  expect_equal(reg_grid_smol$min_n, rep(1:2, each = 2))
  expect_equal(reg_grid_smol$` \t123`, rep(10:11, 2))
  for (i in 1:nrow(reg_grid_smol)) {
    expect_equal(reg_grid_smol$.submodels[[i]], list(`Ade Tukunbo` = 1:2))
  }
})

# ------------------------------------------------------------------------------

test_that("boosted tree grid reduction - C5.0", {
  mod <- parsnip::boost_tree() %>% parsnip::set_engine("C5.0")

  # A typical grid
  reg_grid <- expand.grid(trees = 1:3, min_n = 1:2)
  reg_grid_smol <- min_grid(mod, reg_grid)

  expect_equal(reg_grid_smol$trees, rep(3, 2))
  expect_equal(reg_grid_smol$min_n, 1:2)
  for (i in 1:nrow(reg_grid_smol)) {
    expect_equal(reg_grid_smol$.submodels[[i]], list(trees = 1:2))
  }

  # Unbalanced grid
  reg_ish_grid <- expand.grid(trees = 1:3, min_n = 1:2)[-3, ]
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
  no_sub <- tibble::tibble(trees = 1, min_n = 1:2)
  no_sub_smol <- min_grid(mod, no_sub)

  expect_equal(no_sub_smol$trees, rep(1, 2))
  expect_equal(no_sub_smol$min_n, 1:2)
  for (i in 1:nrow(no_sub_smol)) {
    expect_length(no_sub_smol$.submodels[[i]], 0)
  }

  # different id names
  mod_1 <- parsnip::boost_tree(trees = tune("Marco")) %>%
    parsnip::set_engine("C5.0")
  reg_grid <- expand.grid(Marco = 1:3, min_n = 1:2)
  reg_grid_smol <- min_grid(mod_1, reg_grid)

  expect_equal(reg_grid_smol$Marco, rep(3, 2))
  expect_equal(reg_grid_smol$min_n, 1:2)
  for (i in 1:nrow(reg_grid_smol)) {
    expect_equal(reg_grid_smol$.submodels[[i]], list(Marco = 1:2))
  }

  all_sub <- expand.grid(Marco = 1:3)
  all_sub_smol <- min_grid(mod_1, all_sub)

  expect_equal(all_sub_smol$Marco, 3)
  expect_equal(all_sub_smol$.submodels[[1]], list(Marco = 1:2))

  mod_2 <- parsnip::boost_tree(trees = tune("Anderson Dawes")) %>%
    parsnip::set_engine("C5.0")
  reg_grid <- expand.grid(`Anderson Dawes` = 1:3, min_n = 1:2, ` \t123` = 10:11)
  reg_grid_smol <- min_grid(mod_2, reg_grid)

  expect_equal(reg_grid_smol$`Anderson Dawes`, rep(3, 4))
  expect_equal(reg_grid_smol$min_n, rep(1:2, each = 2))
  expect_equal(reg_grid_smol$` \t123`, rep(10:11, 2))
  for (i in 1:nrow(reg_grid_smol)) {
    expect_equal(reg_grid_smol$.submodels[[i]], list(`Anderson Dawes` = 1:2))
  }
})

# ------------------------------------------------------------------------------


test_that("linear regression grid reduction - glmnet", {

  # glmnet depends on >= 3.6.0 so we only test locally
  skip_if_not_installed("glmnet")

  mod <- parsnip::linear_reg() %>% parsnip::set_engine("glmnet")

  # A typical grid
  reg_grid <- expand.grid(penalty = 1:3, mixture = (1:5) / 5)
  reg_grid_smol <- min_grid(mod, reg_grid)

  expect_equal(reg_grid_smol$penalty, rep(3, 5))
  expect_equal(reg_grid_smol$mixture, (1:5) / 5)
  for (i in 1:nrow(reg_grid_smol)) {
    expect_equal(reg_grid_smol$.submodels[[i]], list(penalty = 1:2))
  }

  # Unbalanced grid
  reg_ish_grid <- expand.grid(penalty = 1:3, mixture = (1:5) / 5)[-3, ]
  reg_ish_grid_smol <- min_grid(mod, reg_ish_grid)

  expect_equal(reg_ish_grid_smol$penalty, c(2, rep(3, 4)))
  expect_equal(reg_ish_grid_smol$mixture, (1:5) / 5)
  expect_equal(reg_ish_grid_smol$.submodels[[1]], list(penalty = 1))
  for (i in 2:nrow(reg_ish_grid_smol)) {
    expect_equal(reg_ish_grid_smol$.submodels[[i]], list(penalty = 1:2))
  }

  # Grid with a third parameter
  reg_grid_extra <- expand.grid(penalty = 1:3, mixture = (1:5) / 5, blah = 10:12)
  reg_grid_extra_smol <- min_grid(mod, reg_grid_extra)

  expect_equal(reg_grid_extra_smol$penalty, rep(3, 15))
  expect_equal(reg_grid_extra_smol$mixture, rep((1:5) / 5, each = 3))
  expect_equal(reg_grid_extra_smol$blah, rep(10:12, 5))
  for (i in 1:nrow(reg_grid_extra_smol)) {
    expect_equal(reg_grid_extra_smol$.submodels[[i]], list(penalty = 1:2))
  }

  # Penalty not specified
  expect_snapshot(error = TRUE, {
    min_grid(mod, data.frame(mixture = 1:3))
  })

  # Only penalty
  only_penalty <- expand.grid(penalty = 1:3)
  only_penalty_smol <- min_grid(mod, only_penalty)

  expect_equal(only_penalty_smol$penalty, 3)
  expect_equal(only_penalty_smol$.submodels, list(list(penalty = 1:2)))

  # No submodels
  no_sub <- tibble::tibble(penalty = 1:5, mixture = (1:5) / 5)
  no_sub_smol <- min_grid(mod, no_sub)

  expect_equal(no_sub_smol$penalty, 1:5)
  expect_equal(no_sub_smol$mixture, (1:5) / 5)
  for (i in 1:nrow(no_sub_smol)) {
    expect_length(no_sub_smol$.submodels[[i]], 0)
  }

  # different id names
  mod_1 <- parsnip::linear_reg(penalty = tune("Shaddid")) %>%
    parsnip::set_engine("glmnet")
  reg_grid <- expand.grid(Shaddid = 1:3, mixture = 1:2)
  reg_grid_smol <- min_grid(mod_1, reg_grid)

  expect_equal(reg_grid_smol$Shaddid, rep(3, 2))
  expect_equal(reg_grid_smol$mixture, 1:2)
  for (i in 1:nrow(reg_grid_smol)) {
    expect_equal(reg_grid_smol$.submodels[[i]], list(Shaddid = 1:2))
  }

  all_sub <- expand.grid(Shaddid = 1:3)
  all_sub_smol <- min_grid(mod_1, all_sub)

  expect_equal(all_sub_smol$Shaddid, 3)
  expect_equal(all_sub_smol$.submodels[[1]], list(Shaddid = 1:2))

  mod_2 <- parsnip::linear_reg(penalty = tune("Josephus Miller")) %>%
    parsnip::set_engine("glmnet")
  reg_grid <- expand.grid(`Josephus Miller` = 1:3, mixture = 1:2, ` \t123` = 10:11)
  reg_grid_smol <- min_grid(mod_2, reg_grid)

  expect_equal(reg_grid_smol$`Josephus Miller`, rep(3, 4))
  expect_equal(reg_grid_smol$mixture, rep(1:2, each = 2))
  expect_equal(reg_grid_smol$` \t123`, rep(10:11, 2))
  for (i in 1:nrow(reg_grid_smol)) {
    expect_equal(reg_grid_smol$.submodels[[i]], list(`Josephus Miller` = 1:2))
  }
})

# ------------------------------------------------------------------------------

test_that("logistic regression grid reduction - glmnet", {

  # glmnet depends on >= 3.6.0 so we only test locally
  skip_if_not_installed("glmnet")

  mod <- parsnip::logistic_reg() %>% parsnip::set_engine("glmnet")

  # A typical grid
  reg_grid <- expand.grid(penalty = 1:3, mixture = (1:5) / 5)
  reg_grid_smol <- min_grid(mod, reg_grid)

  expect_equal(reg_grid_smol$penalty, rep(3, 5))
  expect_equal(reg_grid_smol$mixture, (1:5) / 5)
  for (i in 1:nrow(reg_grid_smol)) {
    expect_equal(reg_grid_smol$.submodels[[i]], list(penalty = 1:2))
  }

  # Unbalanced grid
  reg_ish_grid <- expand.grid(penalty = 1:3, mixture = (1:5) / 5)[-3, ]
  reg_ish_grid_smol <- min_grid(mod, reg_ish_grid)

  expect_equal(reg_ish_grid_smol$penalty, c(2, rep(3, 4)))
  expect_equal(reg_ish_grid_smol$mixture, (1:5) / 5)
  expect_equal(reg_ish_grid_smol$.submodels[[1]], list(penalty = 1))
  for (i in 2:nrow(reg_ish_grid_smol)) {
    expect_equal(reg_ish_grid_smol$.submodels[[i]], list(penalty = 1:2))
  }

  # Grid with a third parameter
  reg_grid_extra <- expand.grid(penalty = 1:3, mixture = (1:5) / 5, blah = 10:12)
  reg_grid_extra_smol <- min_grid(mod, reg_grid_extra)

  expect_equal(reg_grid_extra_smol$penalty, rep(3, 15))
  expect_equal(reg_grid_extra_smol$mixture, rep((1:5) / 5, each = 3))
  expect_equal(reg_grid_extra_smol$blah, rep(10:12, 5))
  for (i in 1:nrow(reg_grid_extra_smol)) {
    expect_equal(reg_grid_extra_smol$.submodels[[i]], list(penalty = 1:2))
  }

  # Penalty not specified
  expect_snapshot(error = TRUE, {
    min_grid(mod, data.frame(mixture = 1:3))
  })

  # Only penalty
  only_penalty <- expand.grid(penalty = 1:3)
  only_penalty_smol <- min_grid(mod, only_penalty)

  expect_equal(only_penalty_smol$penalty, 3)
  expect_equal(only_penalty_smol$.submodels, list(list(penalty = 1:2)))

  # No submodels
  no_sub <- tibble::tibble(penalty = 1:5, mixture = (1:5) / 5)
  no_sub_smol <- min_grid(mod, no_sub)

  expect_equal(no_sub_smol$penalty, 1:5)
  expect_equal(no_sub_smol$mixture, (1:5) / 5)
  for (i in 1:nrow(no_sub_smol)) {
    expect_length(no_sub_smol$.submodels[[i]], 0)
  }


  # different id names
  mod_1 <- parsnip::logistic_reg(penalty = tune("Prax")) %>%
    parsnip::set_engine("glmnet")
  reg_grid <- expand.grid(Prax = 1:3, mixture = 1:2)
  reg_grid_smol <- min_grid(mod_1, reg_grid)

  expect_equal(reg_grid_smol$Prax, rep(3, 2))
  expect_equal(reg_grid_smol$mixture, 1:2)
  for (i in 1:nrow(reg_grid_smol)) {
    expect_equal(reg_grid_smol$.submodels[[i]], list(Prax = 1:2))
  }

  all_sub <- expand.grid(Prax = 1:3)
  all_sub_smol <- min_grid(mod_1, all_sub)

  expect_equal(all_sub_smol$Prax, 3)
  expect_equal(all_sub_smol$.submodels[[1]], list(Prax = 1:2))

  mod_2 <- parsnip::logistic_reg(penalty = tune("Samara Rosenberg")) %>%
    parsnip::set_engine("glmnet")
  reg_grid <- expand.grid(`Samara Rosenberg` = 1:3, mixture = 1:2, ` \t123` = 10:11)
  reg_grid_smol <- min_grid(mod_2, reg_grid)

  expect_equal(reg_grid_smol$`Samara Rosenberg`, rep(3, 4))
  expect_equal(reg_grid_smol$mixture, rep(1:2, each = 2))
  expect_equal(reg_grid_smol$` \t123`, rep(10:11, 2))
  for (i in 1:nrow(reg_grid_smol)) {
    expect_equal(reg_grid_smol$.submodels[[i]], list(`Samara Rosenberg` = 1:2))
  }
})

# more of a negative control test
test_that("logistic regression grid reduction - spark", {
  reg_grid <- expand.grid(penalty = 1:3, mixture = (1:5) / 5)
  reg_grid_smol <- min_grid(parsnip::logistic_reg() %>%
    parsnip::set_engine("spark"), reg_grid)

  expect_equal(reg_grid_smol$penalty, reg_grid$penalty)
  expect_equal(reg_grid_smol$mixture, reg_grid$mixture)
  for (i in 1:nrow(reg_grid_smol)) {
    expect_equal(reg_grid_smol$.submodels[[i]], list())
  }
})

# ------------------------------------------------------------------------------

test_that("MARS grid reduction - earth", {
  mod <- parsnip::mars() %>% parsnip::set_engine("earth")

  # A typical grid
  reg_grid <- expand.grid(num_terms = 1:3, prod_degree = 1:2)
  reg_grid_smol <- min_grid(mod, reg_grid)

  expect_equal(reg_grid_smol$num_terms, rep(3, 2))
  expect_equal(reg_grid_smol$prod_degree, 1:2)
  for (i in 1:nrow(reg_grid_smol)) {
    expect_equal(reg_grid_smol$.submodels[[i]], list(num_terms = 1:2))
  }

  # Unbalanced grid
  reg_ish_grid <- expand.grid(num_terms = 1:3, prod_degree = 1:2)[-3, ]
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
  no_sub <- tibble::tibble(num_terms = 1, prod_degree = 1:2)
  no_sub_smol <- min_grid(mod, no_sub)

  expect_equal(no_sub_smol$num_terms, rep(1, 2))
  expect_equal(no_sub_smol$prod_degree, 1:2)
  for (i in 1:nrow(no_sub_smol)) {
    expect_length(no_sub_smol$.submodels[[i]], 0)
  }


  # different id names
  mod_1 <- parsnip::mars(num_terms = tune("Filip")) %>%
    parsnip::set_engine("earth")
  reg_grid <- expand.grid(Filip = 1:3, prod_degree = 1:2)
  reg_grid_smol <- min_grid(mod_1, reg_grid)

  expect_equal(reg_grid_smol$Filip, rep(3, 2))
  expect_equal(reg_grid_smol$prod_degree, 1:2)
  for (i in 1:nrow(reg_grid_smol)) {
    expect_equal(reg_grid_smol$.submodels[[i]], list(Filip = 1:2))
  }

  all_sub <- expand.grid(Filip = 1:3)
  all_sub_smol <- min_grid(mod_1, all_sub)

  expect_equal(all_sub_smol$Filip, 3)
  expect_equal(all_sub_smol$.submodels[[1]], list(Filip = 1:2))

  mod_2 <- parsnip::mars(num_terms = tune("Elvi Okoye")) %>%
    parsnip::set_engine("earth")
  reg_grid <- expand.grid(`Elvi Okoye` = 1:3, prod_degree = 1:2, ` \t123` = 10:11)
  reg_grid_smol <- min_grid(mod_2, reg_grid)

  expect_equal(reg_grid_smol$`Elvi Okoye`, rep(3, 4))
  expect_equal(reg_grid_smol$prod_degree, rep(1:2, each = 2))
  expect_equal(reg_grid_smol$` \t123`, rep(10:11, 2))
  for (i in 1:nrow(reg_grid_smol)) {
    expect_equal(reg_grid_smol$.submodels[[i]], list(`Elvi Okoye` = 1:2))
  }
})

# ------------------------------------------------------------------------------

test_that("multinomial regression grid reduction - glmnet", {

  # glmnet depends on >= 3.6.0 so we only test locally
  skip_if_not_installed("glmnet")

  mod <- parsnip::multinom_reg() %>% parsnip::set_engine("glmnet")

  # A typical grid
  reg_grid <- expand.grid(penalty = 1:3, mixture = (1:5) / 5)
  reg_grid_smol <- min_grid(mod, reg_grid)

  expect_equal(reg_grid_smol$penalty, rep(3, 5))
  expect_equal(reg_grid_smol$mixture, (1:5) / 5)
  for (i in 1:nrow(reg_grid_smol)) {
    expect_equal(reg_grid_smol$.submodels[[i]], list(penalty = 1:2))
  }

  # Unbalanced grid
  reg_ish_grid <- expand.grid(penalty = 1:3, mixture = (1:5) / 5)[-3, ]
  reg_ish_grid_smol <- min_grid(mod, reg_ish_grid)

  expect_equal(reg_ish_grid_smol$penalty, c(2, rep(3, 4)))
  expect_equal(reg_ish_grid_smol$mixture, (1:5) / 5)
  expect_equal(reg_ish_grid_smol$.submodels[[1]], list(penalty = 1))
  for (i in 2:nrow(reg_ish_grid_smol)) {
    expect_equal(reg_ish_grid_smol$.submodels[[i]], list(penalty = 1:2))
  }

  # Grid with a third parameter
  reg_grid_extra <- expand.grid(penalty = 1:3, mixture = (1:5) / 5, blah = 10:12)
  reg_grid_extra_smol <- min_grid(mod, reg_grid_extra)

  expect_equal(reg_grid_extra_smol$penalty, rep(3, 15))
  expect_equal(reg_grid_extra_smol$mixture, rep((1:5) / 5, each = 3))
  expect_equal(reg_grid_extra_smol$blah, rep(10:12, 5))
  for (i in 1:nrow(reg_grid_extra_smol)) {
    expect_equal(reg_grid_extra_smol$.submodels[[i]], list(penalty = 1:2))
  }

  # Penalty not specified
  expect_snapshot(error = TRUE, {
    min_grid(mod, data.frame(mixture = 1:3))
  })

  # Only penalty
  only_penalty <- expand.grid(penalty = 1:3)
  only_penalty_smol <- min_grid(mod, only_penalty)

  expect_equal(only_penalty_smol$penalty, 3)
  expect_equal(only_penalty_smol$.submodels, list(list(penalty = 1:2)))

  # No submodels
  no_sub <- tibble::tibble(penalty = 1:5, mixture = (1:5) / 5)
  no_sub_smol <- min_grid(mod, no_sub)

  expect_equal(no_sub_smol$penalty, 1:5)
  expect_equal(no_sub_smol$mixture, (1:5) / 5)
  for (i in 1:nrow(no_sub_smol)) {
    expect_length(no_sub_smol$.submodels[[i]], 0)
  }

  # different id names
  mod_1 <- parsnip::multinom_reg(penalty = tune("Cortazar")) %>%
    parsnip::set_engine("glmnet")
  reg_grid <- expand.grid(Cortazar = 1:3, mixture = 1:2)
  reg_grid_smol <- min_grid(mod_1, reg_grid)

  expect_equal(reg_grid_smol$Cortazar, rep(3, 2))
  expect_equal(reg_grid_smol$mixture, 1:2)
  for (i in 1:nrow(reg_grid_smol)) {
    expect_equal(reg_grid_smol$.submodels[[i]], list(Cortazar = 1:2))
  }

  all_sub <- expand.grid(Cortazar = 1:3)
  all_sub_smol <- min_grid(mod_1, all_sub)

  expect_equal(all_sub_smol$Cortazar, 3)
  expect_equal(all_sub_smol$.submodels[[1]], list(Cortazar = 1:2))

  mod_2 <- parsnip::multinom_reg(penalty = tune("Shed Garvey")) %>%
    parsnip::set_engine("glmnet")
  reg_grid <- expand.grid(`Shed Garvey` = 1:3, mixture = 1:2, ` \t123` = 10:11)
  reg_grid_smol <- min_grid(mod_2, reg_grid)

  expect_equal(reg_grid_smol$`Shed Garvey`, rep(3, 4))
  expect_equal(reg_grid_smol$mixture, rep(1:2, each = 2))
  expect_equal(reg_grid_smol$` \t123`, rep(10:11, 2))
  for (i in 1:nrow(reg_grid_smol)) {
    expect_equal(reg_grid_smol$.submodels[[i]], list(`Shed Garvey` = 1:2))
  }
})

# ------------------------------------------------------------------------------


test_that("nearest neighbors grid reduction - kknn", {

  mod <- parsnip::nearest_neighbor() %>% parsnip::set_engine("kknn")

  # A typical grid
  reg_grid <- expand.grid(neighbors = 1:3, dist_power = 1:2)
  reg_grid_smol <- min_grid(mod, reg_grid)

  expect_equal(reg_grid_smol$neighbors, rep(3, 2))
  expect_equal(reg_grid_smol$dist_power, 1:2)
  for (i in 1:nrow(reg_grid_smol)) {
    expect_equal(reg_grid_smol$.submodels[[i]], list(neighbors = 1:2))
  }

  # Unbalanced grid
  reg_ish_grid <- expand.grid(neighbors = 1:3, dist_power = 1:2)[-3, ]
  reg_ish_grid_smol <- min_grid(mod, reg_ish_grid)

  expect_equal(reg_ish_grid_smol$neighbors, 2:3)
  expect_equal(reg_ish_grid_smol$dist_power, 1:2)
  for (i in 2:nrow(reg_ish_grid_smol)) {
    expect_equal(reg_ish_grid_smol$.submodels[[i]], list(neighbors = 1:2))
  }

  # Grid with a third parameter
  wts <- c("rectangular", "triangular", "epanechnikov")
  reg_grid_extra <- expand.grid(neighbors = 1:3, dist_power = 1:2, weight_func = wts)
  reg_grid_extra_smol <- min_grid(mod, reg_grid_extra)

  expect_equal(reg_grid_extra_smol$neighbors, rep(3, 6))
  expect_equal(reg_grid_extra_smol$dist_power, rep(1:2, each = 3))
  expect_equal(reg_grid_extra_smol$weight_func, rep(wts, 2))
  for (i in 1:nrow(reg_grid_extra_smol)) {
    expect_equal(reg_grid_extra_smol$.submodels[[i]], list(neighbors = 1:2))
  }

  # Only neighbors
  only_neighbors <- expand.grid(neighbors = 1:3)
  only_neighbors_smol <- min_grid(mod, only_neighbors)

  expect_equal(only_neighbors_smol$neighbors, 3)
  expect_equal(only_neighbors_smol$.submodels, list(list(neighbors = 1:2)))

  # No submodels
  no_sub <- tibble::tibble(neighbors = 1, dist_power = 1:2)
  no_sub_smol <- min_grid(mod, no_sub)

  expect_equal(no_sub_smol$neighbors, rep(1, 2))
  expect_equal(no_sub_smol$dist_power, 1:2)
  for (i in 1:nrow(no_sub_smol)) {
    expect_length(no_sub_smol$.submodels[[i]], 0)
  }


  # different id names
  mod_1 <- parsnip::nearest_neighbor(neighbors = tune("Nami")) %>%
    parsnip::set_engine("kknn")
  reg_grid <- expand.grid(Nami = 1:3, dist_power = 1:2)
  reg_grid_smol <- min_grid(mod_1, reg_grid)

  expect_equal(reg_grid_smol$Nami, rep(3, 2))
  expect_equal(reg_grid_smol$dist_power, 1:2)
  for (i in 1:nrow(reg_grid_smol)) {
    expect_equal(reg_grid_smol$.submodels[[i]], list(Nami = 1:2))
  }

  all_sub <- expand.grid(Nami = 1:3)
  all_sub_smol <- min_grid(mod_1, all_sub)

  expect_equal(all_sub_smol$Nami, 3)
  expect_equal(all_sub_smol$.submodels[[1]], list(Nami = 1:2))

  mod_2 <- parsnip::nearest_neighbor(neighbors = tune("Michio Pa")) %>%
    parsnip::set_engine("kknn")
  reg_grid <- expand.grid(`Michio Pa` = 1:3, dist_power = 1:2, ` \t123` = 10:11)
  reg_grid_smol <- min_grid(mod_2, reg_grid)

  expect_equal(reg_grid_smol$`Michio Pa`, rep(3, 4))
  expect_equal(reg_grid_smol$dist_power, rep(1:2, each = 2))
  expect_equal(reg_grid_smol$` \t123`, rep(10:11, 2))
  for (i in 1:nrow(reg_grid_smol)) {
    expect_equal(reg_grid_smol$.submodels[[i]], list(`Michio Pa` = 1:2))
  }
})
