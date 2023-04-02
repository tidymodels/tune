test_that("basic functionality", {
  svm_reg_results <- readRDS(test_path("data", "svm_reg_results.rds"))
  load(test_path("data", "test_objects.RData"))
  options(width = 200, pillar.advice = FALSE, pillar.min_title_chars = Inf)

  filtered_grid <- filter_parameters(svm_reg_results, parameters = tibble::tibble(`%^*#` = 1))
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

  filtered_grid <- filter_parameters(mt_knn_bo, parameters = tibble::tibble(neighbors = 1))
  expect_true(all(purrr::map_lgl(filtered_grid$.metrics, ~ all(.x$neighbors == 1))))
  expect_true(all(purrr::map_lgl(filtered_grid$.predictions, ~ all(.x$neighbors == 1))))

  best_param <- select_best(mt_knn_bo, metric = "rmse")
  best_grid <- filter_parameters(mt_knn_bo, parameters = best_param)
  expect_true(all(purrr::map_lgl(best_grid$.metrics, ~ all(.x$neighbors == best_param$neighbors))))
  expect_true(all(purrr::map_lgl(best_grid$.predictions, ~ all(.x$neighbors == best_param$neighbors))))

  rec_tune_1 <-
    recipes::recipe(mpg ~ ., data = mtcars) %>%
    recipes::step_normalize(recipes::all_predictors()) %>%
    recipes::step_pca(recipes::all_predictors(), num_comp = tune())

  lm_mod <- parsnip::linear_reg() %>% parsnip::set_engine("lm")

  set.seed(363)
  mt_folds <- rsample::vfold_cv(mtcars, v = 3)

  extr_rec <- function(x) {
    extract_recipe(x) %>% tidy(number = 2)
  }
  lm_res <-
    workflow() %>%
    add_recipe(rec_tune_1) %>%
    add_model(lm_mod) %>%
    tune_grid(
      resamples = mt_folds,
      control = control_grid(extract = extr_rec),
      grid = tibble::tibble(num_comp = 1:2)
    )

  filtered_grid <- filter_parameters(lm_res, parameters = tibble::tibble(num_comp = 1))
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

test_that("bad inputs", {
  svm_reg_results <- readRDS(test_path("data", "svm_reg_results.rds"))

  expect_snapshot(error = TRUE, {
    filter_parameters(collect_metrics(svm_reg_results), parameters = tibble::tibble(`%^*#` = 1))
  })
  expect_snapshot(error = TRUE, {
    filter_parameters(svm_reg_results, parameters = tibble::tibble(soup = 1))
  })
  expect_snapshot(error = TRUE, {
    filter_parameters(svm_reg_results, tibble::tibble(soup = 1))
  })
  expect_snapshot(error = TRUE, {
    filter_parameters(svm_reg_results, cost < 1, tibble::tibble(soup = 1))
  })
  expect_snapshot(error = TRUE, {
    filter_parameters(svm_reg_results, parameters = tibble::tibble(`%^*#` = 1 / 3))
  })
  expect_snapshot(
    filter_parameters(svm_reg_results, parameters = tibble::tibble(`%^*#` = 1, soup = 2))
  )
  expect_snapshot(
    res <- filter_parameters(
      svm_reg_results,
      parameters = tibble::tibble(`%^*#` = 1, soup = 2, boop = 3)
    )
  )
  expect_snapshot(
    res <- filter_parameters(
      svm_reg_results,
      parameters = tibble::tibble(`%^*#` = 1, soup = 2, boop = 3, loop = 4)
    )
  )
})
