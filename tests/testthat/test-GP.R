library(dplyr)

# ------------------------------------------------------------------------------

svm_results <- readRDS(test_path("svm_results.rds"))
knn_results <- readRDS(test_path("knn_results.rds"))
knn_set <- readRDS(test_path("knn_set.rds"))
knn_grid <- readRDS(test_path("knn_grid.rds"))
knn_gp <- readRDS(test_path("knn_gp.rds"))

svm_set <- attributes(svm_results)$parameters

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
    c('cost', '`%^*#`', 'scale_factor')
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
    dplyr::filter(.metric == "accuracy") %>%
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
    c('cost', '%^*#', 'scale_factor', '.mean', '.sd')
  )
  expect_equal(nrow(svm_scores), 20)

})

