test_that("fit_best", {
  skip_if_not_installed("kknn")
  skip_if_not_installed("modeldata")

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

test_that("fit_best() works with validation split: 3-way split", {
  skip_if_not_installed("kknn")
  skip_if_not_installed("modeldata")
  data(ames, package = "modeldata", envir = rlang::current_env())

  set.seed(23598723)
  initial_val_split <- rsample::initial_validation_split(ames)
  val_set <- validation_set(initial_val_split)

  f <- Sale_Price ~ Gr_Liv_Area + Year_Built
  knn_mod <- nearest_neighbor(neighbors = tune()) %>% set_mode("regression")
  wflow <- workflow(f, knn_mod)

  tune_res <- tune_grid(
    wflow,
    grid = tibble(neighbors = c(1, 5)),
    resamples = val_set,
    control = control_grid(save_workflow = TRUE)
  ) %>% suppressWarnings()
  set.seed(3)
  fit_on_train <- fit_best(tune_res)
  pred <- predict(fit_on_train, testing(initial_val_split))

  set.seed(3)
  exp_fit_on_train <- nearest_neighbor(neighbors = 5) %>%
    set_mode("regression") %>%
    fit(f, training(initial_val_split))
  exp_pred <- predict(exp_fit_on_train, testing(initial_val_split))

  expect_equal(pred, exp_pred)
})

test_that("fit_best() works with validation split: 2x 2-way splits", {
  skip_if_not_installed("kknn")
  skip_if_not_installed("modeldata")
  data(ames, package = "modeldata", envir = rlang::current_env())

  set.seed(23598723)
  split <- initial_validation_split(ames)
  train <- training(split)
  val_set <- validation_set(split)

  f <- Sale_Price ~ Gr_Liv_Area + Year_Built
  knn_mod <- nearest_neighbor(neighbors = tune()) %>% set_mode("regression")
  wflow <- workflow(f, knn_mod)

  tune_res <- tune_grid(
    wflow,
    grid = tibble(neighbors = c(1, 5)),
    resamples = val_set,
    control = control_grid(save_workflow = TRUE)
  )
  set.seed(3)
  fit_on_train_and_val <- fit_best(tune_res)
  pred <- predict(fit_on_train_and_val, testing(split))

  set.seed(3)
  exp_fit_on_train_and_val <- nearest_neighbor(neighbors = 5) %>%
    set_mode("regression") %>%
    fit(f, train)
  exp_pred <- predict(exp_fit_on_train_and_val, testing(split))

  expect_equal(pred, exp_pred)
})

test_that(
  "fit_best() warns when metric or eval_time are specified in addition to parameters", {
  skip_if_not_installed("kknn")

  knn_mod <- nearest_neighbor(neighbors = tune()) %>% set_mode("regression")
  res <-
      tune_grid(
      workflow(mpg ~ ., knn_mod),
      bootstraps(mtcars),
      control = control_grid(save_workflow = TRUE)
   )

   tune_params <- select_best(res, metric = "rmse")
   expect_snapshot(
    manual_wf <- fit_best(res, metric = "rmse", parameters = tune_params)
   )
   expect_snapshot(
    manual_wf <- fit_best(res, metric = "rmse", eval_time = 10, parameters = tune_params)
   )
})
