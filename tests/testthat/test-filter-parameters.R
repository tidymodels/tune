context("parameter filtering")

# ------------------------------------------------------------------------------

source(test_path("../helper-objects.R"))
svm_reg_results <- readRDS(test_path("svm_reg_results.rds"))
load(test_path("test_objects.RData"))

# ------------------------------------------------------------------------------

rec_tune_1 <-
  recipe(mpg ~ ., data = mtcars) %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors(), num_comp = tune())

lm_mod <- linear_reg() %>% set_engine("lm")

set.seed(363)
mt_folds <- vfold_cv(mtcars, v = 3)

extr_rec <- function(x) {
  extract_recipe(x) %>% tidy(number = 2)
}
lm_res <-
  workflow() %>%
  add_recipe(rec_tune_1) %>%
  add_model(lm_mod) %>%
  tune_grid(resamples = mt_folds, control = control_grid(extract = extr_rec),
            grid = tibble(num_comp = 1:2))

# ------------------------------------------------------------------------------

test_that("basic functionality",{
  filtered_grid <- filter_parameters(svm_reg_results, parameters = tibble(`%^*#` = 1))
  expect_true(all(purrr::map_lgl(filtered_grid$.metrics, ~ all(.x$`%^*#` == 1))))
  expect_true(all(purrr::map_lgl(filtered_grid$.predictions, ~ all(.x$`%^*#` == 1))))

  best_param <- select_best(svm_reg_results, metric = "accuracy")
  best_grid <- filter_parameters(svm_reg_results, parameters = best_param)
  expect_true(all(purrr::map_lgl(best_grid$.metrics, ~ all(.x$cost == best_param$cost))))
  expect_true(all(purrr::map_lgl(best_grid$.metrics, ~ all(.x$`%^*#` == best_param$`%^*#`))))
  expect_true(all(purrr::map_lgl(best_grid$.metrics, ~ all(.x$scale_factor == best_param$scale_factor))))
  expect_true(all(purrr::map_lgl(best_grid$.predictions, ~ all(.x$cost == best_param$cost))))
  expect_true(all(purrr::map_lgl(best_grid$.predictions, ~ all(.x$`%^*#` == best_param$`%^*#`))))
  expect_true(all(purrr::map_lgl(best_grid$.predictions, ~ all(.x$scale_factor == best_param$scale_factor))))

  filtered_grid <- filter_parameters(mt_knn_bo, parameters = tibble(neighbors = 1))
  expect_true(all(purrr::map_lgl(filtered_grid$.metrics, ~ all(.x$neighbors == 1))))
  expect_true(all(purrr::map_lgl(filtered_grid$.predictions, ~ all(.x$neighbors == 1))))

  best_param <- select_best(mt_knn_bo, metric = "rmse")
  best_grid <- filter_parameters(mt_knn_bo, parameters = best_param)
  expect_true(all(purrr::map_lgl(best_grid$.metrics, ~ all(.x$neighbors == best_param$neighbors))))
  expect_true(all(purrr::map_lgl(best_grid$.predictions, ~ all(.x$neighbors == best_param$neighbors))))

  filtered_grid <- filter_parameters(lm_res, parameters = tibble(num_comp = 1))
  expect_true(all(purrr::map_lgl(filtered_grid$.metrics, ~ all(.x$num_comp == 1))))
  expect_true(all(purrr::map_lgl(filtered_grid$.extracts, ~ all(.x$num_comp == 1))))

  best_param <- select_best(lm_res, metric = "rmse")
  best_grid <- filter_parameters(lm_res, parameters = best_param)
  expect_true(all(purrr::map_lgl(best_grid$.metrics, ~ all(.x$num_comp == best_param$num_comp))))
  expect_true(all(purrr::map_lgl(best_grid$.extracts, ~ all(.x$num_comp == best_param$num_comp))))


  filtered_grid <- filter_parameters(svm_reg_results, `%^*#` == 1)
  expect_true(all(purrr::map_lgl(filtered_grid$.metrics, ~ all(.x$`%^*#` == 1))))
  expect_true(all(purrr::map_lgl(filtered_grid$.predictions, ~ all(.x$`%^*#` == 1))))
})

# ------------------------------------------------------------------------------

test_that("bad inputs",{
  skip_if(tune:::dplyr_pre_1.0.0())

  expect_error(
    filter_parameters(collect_metrics(svm_reg_results), parameters = tibble(`%^*#` = 1)),
    "should have class 'tune_results'"
    )
  expect_error(
    filter_parameters(svm_reg_results, parameters = tibble(soup = 1)),
    "There are no columns in"
  )
  expect_error(
    filter_parameters(svm_reg_results, tibble(soup = 1)),
    "must be a logical"
  )
  expect_error(
    filter_parameters(svm_reg_results, parameters = tibble(`%^*#` = 1/3)),
    "No parameter combinations were selected"
  )
  expect_warning(
    filter_parameters(svm_reg_results, parameters = tibble(`%^*#` = 1, soup = 2)),
    "There are unneeded columns in `parameters`"
  )
 })
