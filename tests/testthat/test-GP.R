test_that("encoding before model", {
  skip_if_not_installed("dials", minimum_version = "1.4.0")
  knn_set <- readRDS(test_path("data", "knn_set.rds"))
  knn_grid <- readRDS(test_path("data", "knn_grid.rds"))

  knn_encoded <- tune:::encode_set(knn_grid, knn_set)

  expect_true(all(knn_encoded$K >= 0 & knn_encoded$K <= 1))
  expect_true(all(knn_encoded$exponent >= 0 & knn_encoded$exponent <= 1))
  expect_true(is.factor(knn_encoded$weight_func))
  expect_equal(levels(knn_encoded$weight_func), dials::weight_func()$values)
})

# ------------------------------------------------------------------------------

test_that("GP fit - svm", {
  svm_results <- readRDS(test_path("data", "svm_results.rds"))
  svm_set <- attributes(svm_results)$parameters

  svm_gp <-
    tune:::fit_gp(
      collect_metrics(svm_results),
      pset = svm_set,
      metric = "accuracy",
      control = control_bayes(verbose = TRUE)
    )
  expect_equal(class(svm_gp), "GP")
  expect_equal(
    colnames(svm_gp$X),
    c("cost", "`%^*#`", "scale_factor")
  )
})

# ------------------------------------------------------------------------------

test_that("GP fit - knn", {
  knn_gp <- readRDS(test_path("data", "knn_gp.rds"))

  knn_cols <- c(
    "K", "weight_funcrectangular", "weight_functriangular",
    "weight_funcepanechnikov", "weight_funcbiweight",
    "weight_functriweight", "weight_funccos", "weight_funcinv",
    "weight_funcgaussian", "weight_funcrank", "exponent"
  )
  expect_equal(class(knn_gp), "GP")
  expect_equal(colnames(knn_gp$X), knn_cols)
})

# ------------------------------------------------------------------------------

test_that("GP scoring", {
  svm_results <- readRDS(test_path("data", "svm_results.rds"))
  svm_set <- attributes(svm_results)$parameters

  ctrl <- control_bayes()
  curr <-
    collect_metrics(svm_results) %>%
    dplyr::filter(.metric == "accuracy") %>%
    mutate(.iter = 0)

  svm_gp <-
    tune:::fit_gp(
      collect_metrics(svm_results),
      pset = svm_set,
      metric = "accuracy",
      control = ctrl
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
    c("cost", "%^*#", "scale_factor", ".mean", ".sd")
  )
  expect_equal(nrow(svm_scores), 20)
})
