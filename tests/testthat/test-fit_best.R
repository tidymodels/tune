test_that("fit_best", {
  skip_if_not_installed("kknn")

  library(recipes)
  library(rsample)
  library(dplyr)
  library(parsnip)

  data(meats, package = "modeldata")
  meats <- meats %>% select(-water, -fat)

  set.seed(1)
  meat_split <- initial_split(meats)
  meat_train <- training(meat_split)
  meat_test <- testing(meat_split)

  set.seed(2)
  meat_rs <- vfold_cv(meat_train, v = 3)

  pca_rec <-
    recipe(protein ~ ., data = meat_train) %>%
    step_pca(all_predictors(), num_comp = tune())

  knn_mod <- nearest_neighbor(neighbors = tune()) %>% set_mode("regression")

  ctrl <- control_grid(save_workflow = TRUE)

  set.seed(3)
  knn_pca_res <-
    tune_grid(knn_mod, pca_rec, resamples = meat_rs, grid = 3, control = ctrl)

  expect_silent(knn_fit <- fit_best(knn_pca_res))
  expect_true(knn_fit$trained)
  expect_silent(fit_best(knn_pca_res, metric = "rsq"))
  expect_snapshot(fit_best(knn_pca_res, verbose = TRUE))
  expect_snapshot(
    tmp <-
      fit_best(knn_pca_res,
               verbose = TRUE,
               parameters = tibble(neighbors = 1, num_comp = 1))
  )

  expect_snapshot_error(
    fit_best(1L)
  )
  expect_snapshot_error(
    fit_best(tibble())
  )
  expect_snapshot_error(
    fit_best(knn_pca_res, metric = "WAT")
  )
  expect_snapshot_error(
    fit_best(knn_pca_res, parameters = tibble())
  )
  expect_snapshot_error(
    fit_best(knn_pca_res, parameters = tibble(neighbors = 2))
  )
  expect_snapshot_error(
    fit_best(knn_pca_res, chickens = 2)
  )
  data(example_ames_knn)
  expect_snapshot_error(
    fit_best(ames_iter_search)
  )
})
