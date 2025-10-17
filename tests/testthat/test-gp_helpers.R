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

test_that("GP fit - svm - failure", {
  svm_results <- readRDS(test_path("data", "svm_results.rds"))
  svm_set <- attributes(svm_results)$parameters

  expect_snapshot({
    svm_gp <-
      tune:::fit_gp(
        collect_metrics(svm_results),
        pset = svm_set,
        metric = "accuracy",
        control = control_bayes(verbose = TRUE)
      )
  })

  expect_equal(class(svm_gp), "list")
  expect_named(
    svm_gp,
    c("fit", "use", "rsq", "tr")
  )
  expect_false(svm_gp$use)
  expect_named(
    svm_gp$tr,
    c("cost", "X....", "scale_factor", ".outcome")
  )

  curr <-
    collect_metrics(svm_results) |>
    dplyr::filter(.metric == "accuracy") |>
    mutate(.iter = 0)

  expect_snapshot({
    svm_scores <-
      tune:::pred_gp(
        svm_gp,
        pset = svm_set,
        size = 20,
        current = curr,
        control = ctrl
      )
  })
})

# ------------------------------------------------------------------------------

test_that("GP scoring with failed model", {
  svm_results <- readRDS(test_path("data", "svm_results.rds"))
  svm_set <- attributes(svm_results)$parameters

  ctrl <- control_bayes()
  curr <-
    collect_metrics(svm_results) |>
    dplyr::filter(.metric == "accuracy") |>
    mutate(.iter = 0)

  expect_snapshot({
    svm_gp <-
      tune:::fit_gp(
        collect_metrics(svm_results),
        pset = svm_set,
        metric = "accuracy",
        control = ctrl
      )
  })

  expect_snapshot({
    svm_scores <-
      tune:::pred_gp(
        svm_gp,
        pset = svm_set,
        size = 20,
        current = curr,
        control = ctrl
      )
  })
  expect_true(tibble::is_tibble(svm_scores))
  expect_named(
    svm_scores,
    c("cost", "%^*#", "scale_factor", ".mean", ".sd")
  )
  expect_equal(nrow(svm_scores), 1)
})


# ------------------------------------------------------------------------------

test_that("GP fit - knn", {
  knn_results <- readRDS(test_path("data", "knn_results.rds"))
  knn_set <- attributes(knn_results)$parameters

  knn_mtr <-
    collect_metrics(knn_results) |>
    dplyr::filter(.metric == "roc_auc")

  set.seed(1)
  knn_gp <-
    tune:::fit_gp(
      knn_mtr,
      pset = knn_set,
      metric = "roc_auc",
      control = control_bayes()
    )

  expect_equal(class(knn_gp), "list")
  expect_named(
    knn_gp,
    c("fit", "use", "rsq", "tr")
  )
  expect_true(knn_gp$use)
  expect_named(
    knn_gp$tr,
    c("K", "weight_func", "exponent", ".outcome")
  )

  expect_snapshot({
    set.seed(1)
    knn_scores <-
      tune:::pred_gp(
        knn_gp,
        pset = knn_set,
        size = 20,
        current = knn_mtr |> mutate(.iter = 0),
        control = ctrl
      )
  })

  expect_named(
    knn_scores,
    c("K", "weight_func", "exponent", ".mean", ".sd")
  )
  expect_equal(nrow(knn_scores), 20L)
})
