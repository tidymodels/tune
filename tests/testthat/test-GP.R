context("GP model and helpers")
# ------------------------------------------------------------------------------
# library(tidymodels)
# set.seed(7898)
# data_folds <- vfold_cv(two_class_dat, repeats = 5)
#
# two_class_rec <-
#   recipe(Class ~ ., data = two_class_dat) %>%
#   step_normalize(A, B)
#
# knn_model <-
#   nearest_neighbor(
#     mode = "classification",
#     neighbors = tune("K"),
#     weight_func = tune(),
#     dist_power = tune("exponent")
#   ) %>%
#   set_engine("kknn")
#
# two_class_wflow <-
#   workflow() %>%
#   add_recipe(two_class_rec) %>%
#   add_model(knn_model)
#
# two_class_set <-
#   parameters(two_class_wflow) %>%
#   update(K = neighbors(c(1, 50))) %>%
#   update(exponent = dist_power(c(1/10, 2)))
#
# set.seed(2494)
# two_class_grid <-
#   two_class_set %>%
#   grid_max_entropy(size = 10)
#
# class_metrics <- metric_set(roc_auc, accuracy, kap, mcc)
#
# res <- tune_grid(two_class_wflow, resamples = data_folds, grid = two_class_grid,
#                  metrics = class_metrics, control = control_grid(verbose = TRUE))
# saveRDS(res,
#         file = testthat::test_path("knn_results.rds"),
#         version = 2, compress = "xz")
#
# saveRDS(two_class_set,
#         file = testthat::test_path("knn_set.rds"),
#         version = 2, compress = "xz")
#
# saveRDS(two_class_grid,
#         file = testthat::test_path("knn_grid.rds"),
#         version = 2, compress = "xz")

# ------------------------------------------------------------------------------

source(test_path("../helper-objects.R"))
load(test_path("svm_results.RData"))
knn_results <- readRDS(test_path("knn_results.rds"))
knn_set <- readRDS(test_path("knn_set.rds"))
knn_grid <- readRDS(test_path("knn_grid.rds"))

knn_gp <-
  tune:::fit_gp(collect_metrics(knn_results),
                knn_set,
                "accuracy",
                control_bayes()
  )

# ------------------------------------------------------------------------------

test_that('encoding before model', {
  knn_encoded <- tune:::encode_set(knn_grid, knn_set)

  expect_true(all(knn_encoded$K >= 0 & knn_encoded$K <= 1))
  expect_true(all(knn_encoded$exponent >= 0 & knn_encoded$exponent <= 1))
  expect_true(is.factor(knn_encoded$weight_func))
  expect_equal(levels(knn_encoded$weight_func), dials::weight_func()$values)

})


# ------------------------------------------------------------------------------

test_that('GP fit - svm', {


  svm_gp <-
    tune:::fit_gp(collect_metrics(svm_results),
                  svm_set,
                  "accuracy",
                  control_bayes(verbose = TRUE)
    )
  expect_equal(class(svm_gp), "GP")
  expect_equal(
    colnames(svm_gp$X),
    c('C', '`%^*#`', 'scale_factor')
  )

})

# ------------------------------------------------------------------------------

test_that('GP fit - knn', {

  knn_cols <- c(
    'K', 'weight_funcrectangular', 'weight_functriangular',
    'weight_funcepanechnikov', 'weight_funcbiweight',
    'weight_functriweight', 'weight_funccos', 'weight_funcinv',
    'weight_funcgaussian', 'weight_funcrank', 'weight_funcoptimal',
    'exponent'
  )
  expect_equal(class(knn_gp), "GP")
  expect_equal(colnames(knn_gp$X), knn_cols)

})

# ------------------------------------------------------------------------------

test_that('GP scoring', {

  ctrl <- control_bayes()
  curr <-
    collect_metrics(svm_results) %>%
    filter(.metric == "accuracy") %>%
    mutate(.iter = 0)

  svm_gp <-
    tune:::fit_gp(collect_metrics(svm_results),
                  svm_set,
                  "accuracy",
                  ctrl
    )

  svm_scores <-
    tune:::pred_gp(
      svm_gp,
      pset = svm_set,
      size = 20,
      current = curr,
      control = ctrl
    )
  expect_true(tibble::is_tibble(svm_scores))
  expect_equal(
    colnames(svm_scores),
    c('C', '%^*#', 'scale_factor', '.mean', '.sd')
  )
  expect_equal(nrow(svm_scores), 20)

})

