test_that("two quantitative predictor marginal plot", {
  svm_results <- readRDS(test_path("data", "svm_results.rds"))

  p <- autoplot(svm_results)
  expect_s3_class(p, "ggplot")
  expect_equal(names(p$data), c("mean", "# resamples", ".metric", "name", "value"))
  expect_equal(rlang::get_expr(p$mapping$x), expr(value))
  expect_equal(rlang::get_expr(p$mapping$y), expr(mean))
  expect_equal(p$labels$y, "")
  expect_equal(p$labels$x, "")

  p <- autoplot(svm_results, metric = "accuracy")
  expect_true(isTRUE(all.equal(unique(p$data$.metric), "accuracy")))
})


test_that("two quantitative predictor and one qualitative marginal plot", {
  knn_results <- readRDS(test_path("data", "knn_results.rds"))

  p <- autoplot(knn_results)
  expect_s3_class(p, "ggplot")
  expect_equal(
    names(p$data),
    c("Distance Weighting Function", "mean", "# resamples", ".metric", "name", "value")
  )
  expect_equal(rlang::get_expr(p$mapping$x), expr(value))
  expect_equal(rlang::get_expr(p$mapping$y), expr(mean))
  expect_equal(p$labels$y, "")
  expect_equal(p$labels$x, "")
  expect_equal(p$labels$colour, "Distance Weighting Function")
})

test_that("not marginal plot with grid search", {
  knn_results <- readRDS(test_path("data", "knn_results.rds"))

  expect_snapshot(error = TRUE, autoplot(knn_results, type = "performance"))
  expect_snapshot(error = TRUE, autoplot(knn_results, type = "parameters"))
})


test_that("marginal plot labels and transformations - irregular grid", {
  svm_results <- readRDS(test_path("data", "svm_results.rds"))

  p <- autoplot(svm_results)
  expect_s3_class(p, "ggplot")
  expect_equal(names(p$data), c("mean", "# resamples", ".metric", "name", "value"))
  expect_equal(rlang::get_expr(p$mapping$x), expr(value))
  expect_equal(rlang::get_expr(p$mapping$y), expr(mean))
  expect_equal(p$labels$y, "")
  expect_equal(p$labels$x, "")
  expect_equal(
    sort(unique(p$data$name)),
    c("%^*#", "Cost (log-2)", "Scale Factor (log-10)")
  )
  expect_equal(
    sort(unique(collect_metrics(svm_results)$cost)),
    sort(unique(2^(p$data$value[p$data$name == "Cost (log-2)"]))),
    tolerance = 0.001
  )
  expect_equal(
    sort(unique(collect_metrics(svm_results)$scale_factor)),
    sort(unique(10^(p$data$value[p$data$name == "Scale Factor (log-10)"]))),
    tolerance = 0.001
  )

  p <- autoplot(svm_results, metric = "accuracy")
  expect_true(isTRUE(all.equal(unique(p$data$.metric), "accuracy")))
})

# ------------------------------------------------------------------------------

test_that("marginal plot for iterative search", {
  load(test_path("data", "test_objects.RData"))

  p <- autoplot(mt_spln_knn_bo_sep)
  expect_s3_class(p, "ggplot")
  expect_equal(
    names(p$data),
    c("Distance Weighting Function", "mean", "# resamples", ".metric", "name", "value")
  )
  expect_equal(rlang::get_expr(p$mapping$x), expr(value))
  expect_equal(rlang::get_expr(p$mapping$y), expr(mean))
  expect_equal(p$labels$y, "")
  expect_equal(p$labels$x, "")
  expect_equal(p$labels$colour, "Distance Weighting Function")

  p <- autoplot(mt_spln_knn_bo_sep, metric = "rmse")
  expect_true(isTRUE(all.equal(unique(p$data$.metric), "rmse")))
})


test_that("performance plot for iterative search", {
  load(test_path("data", "test_objects.RData"))

  p <- autoplot(mt_spln_knn_bo_sep, type = "performance")
  expect_s3_class(p, "ggplot")
  expect_equal(
    names(p$data),
    c(
      "K", "weight_func", "deg_free", ".metric",
      ".estimator", "mean", "n", "std_err", ".config", ".iter"
    )
  )
  expect_equal(rlang::get_expr(p$mapping$x), expr(.iter))
  expect_equal(rlang::get_expr(p$mapping$y), expr(mean))
  expect_equal(p$labels$x, "Iteration")

  p <- autoplot(mt_spln_knn_bo_sep, type = "performance", metric = "rmse")
  expect_true(isTRUE(all.equal(unique(p$data$.metric), "rmse")))

  p <- autoplot(mt_spln_knn_bo_sep, type = "performance", width = 0)
  expect_true(isTRUE(all.equal(names(p$mapping), c("x", "y"))))
})


test_that("parameter plot for iterative search", {
  load(test_path("data", "test_objects.RData"))

  p <- autoplot(mt_spln_knn_bo_sep, type = "parameters")
  expect_s3_class(p, "ggplot")
  expect_equal(names(p$data), c(".iter", "name", "value"))

  name_vals <- sort(unique(p$data$name))
  expect_equal(name_vals, c("K", "Spline Degrees of Freedom"))
  expect_equal(rlang::get_expr(p$mapping$x), expr(.iter))
  expect_equal(rlang::get_expr(p$mapping$y), expr(value))
  expect_equal(p$labels$y, "")
  expect_equal(p$labels$x, "Iteration")
})


test_that("regular grid plot", {
  skip_if_not_installed("scales", "1.3.0")
  rcv_results <- readRDS(test_path("data", "rcv_results.rds"))
  svm_reg_results <- readRDS(test_path("data", "svm_reg_results.rds"))

  p <- autoplot(rcv_results)
  expect_s3_class(p, "ggplot")
  expect_equal(
    names(p$data),
    c("degree", "wt df", "wt degree", "mean", "# resamples", ".metric", "name", "value")
  )
  expect_equal(rlang::get_expr(p$mapping$x), expr(value))
  expect_equal(rlang::get_expr(p$mapping$y), expr(mean))
  expect_equal(rlang::get_expr(p$mapping$col), sym("wt df"))
  expect_equal(rlang::get_expr(p$mapping$group), sym("wt df"))
  expect_equal(p$facet$vars(), c(".metric", "degree", "wt degree"))

  expect_equal(p$labels$y, "")
  expect_equal(p$labels$x, "deg_free")

  p <- autoplot(rcv_results, metric = "rmse")
  expect_s3_class(p, "ggplot")
  expect_equal(
    names(p$data),
    c("degree", "wt df", "wt degree", "mean", "# resamples", ".metric", "name", "value")
  )
  expect_equal(rlang::get_expr(p$mapping$x), expr(value))
  expect_equal(rlang::get_expr(p$mapping$y), expr(mean))
  expect_equal(rlang::get_expr(p$mapping$col), sym("wt df"))
  expect_equal(rlang::get_expr(p$mapping$group), sym("wt df"))
  expect_equal(p$facet$vars(), c("degree", "wt degree"))

  expect_equal(p$labels$y, "rmse")
  expect_equal(p$labels$x, "deg_free")


  p <- autoplot(svm_reg_results)
  expect_s3_class(p, "ggplot")
  expect_equal(
    names(p$data),
    c("%^*#", "Scale Factor", "mean", "# resamples", ".metric", "name", "value")
  )
  expect_equal(rlang::get_expr(p$mapping$x), expr(value))
  expect_equal(rlang::get_expr(p$mapping$y), expr(mean))
  expect_equal(rlang::get_expr(p$mapping$col), sym("%^*#"))
  expect_equal(rlang::get_expr(p$mapping$group), sym("%^*#"))
  expect_equal(p$facet$vars(), c(".metric", "Scale Factor"))

  expect_equal(p$labels$y, "")
  expect_equal(p$labels$colour, as.name("%^*#"))
  expect_equal(p$labels$x, "Cost")

  expect_true(grepl("^trans", class(p$scales$scales[[1]]$trans)))
  expect_equal(p$scales$scales[[1]]$trans$name, "log-2")
  expect_equal(unique(p$data$name), "Cost")
})



test_that("coord_obs_pred", {
  skip_if_not_installed("modeldata")

  data(solubility_test, package = "modeldata")

  p <-
    ggplot2::ggplot(solubility_test, ggplot2::aes(x = solubility, y = prediction)) +
    ggplot2::geom_abline(lty = 2) +
    ggplot2::geom_point(alpha = 0.5)

  rng <- range(solubility_test[[1]], solubility_test[[2]])

  p2 <- p + coord_obs_pred()

  expect_no_error(print(p2))

  expect_true(inherits(p2$coordinates, "CoordObsPred"))
  expect_equal(p2$coordinates$limits$x, rng)
  expect_equal(p2$coordinates$limits$y, rng)
  expect_equal(p2$coordinates$ratio, 1)

  solubility_test$solubility[1] <- NA
  p3 <-
    ggplot2::ggplot(solubility_test, ggplot2::aes(x = solubility, y = prediction)) +
    ggplot2::geom_abline(lty = 2) +
    ggplot2::geom_point(alpha = 0.5)
  expect_snapshot_warning(print(p3 + coord_obs_pred()))
})

test_that("1D regular grid x labels", {
  skip_if_not_installed("kernlab")

  set.seed(1)
  res <-
    parsnip::svm_rbf(cost = tune()) %>%
    parsnip::set_engine("kernlab") %>%
    parsnip::set_mode("regression") %>%
    tune_grid(mpg ~ ., resamples = rsample::vfold_cv(mtcars, v = 5), grid = 3)
  expect_equal(autoplot(res)$labels$x, c(cost = "Cost"))
})

test_that("plot_regular_grid with fairness metrics (#773)", {
  skip_on_cran()
  skip_if_not_installed("yardstick", minimum_version = "1.2.0.9001")
  skip_if_not_installed("kknn")

  knn <- parsnip::nearest_neighbor("classification", "kknn", neighbors = tune())
  mtcars_fair <- mtcars
  mtcars_fair$vs <- as.factor(mtcars_fair$vs)
  mtcars_fair$cyl <- as.factor(mtcars_fair$cyl)
  mtcars_fair$am <- as.factor(mtcars_fair$am)
  set.seed(4400)
  boots <- rsample::bootstraps(mtcars_fair, 3)
  n_grid <- 3

  set.seed(1)
  res <- tune_grid(
    knn, vs ~ mpg + hp + cyl, resamples = boots, grid = n_grid,
    metrics =
      yardstick::metric_set(
        yardstick::roc_auc,
        yardstick::demographic_parity(cyl),
        yardstick::demographic_parity(am)
      )
  )

  res_plot <- autoplot(res)

  expect_contains(
    res_plot$data$.metric,
    c("demographic_parity(am)", "demographic_parity(cyl)", "roc_auc")
  )
})

test_that("plot_marginals with fairness metrics (#773)", {
  skip_on_cran()
  skip_if_not_installed("yardstick", minimum_version = "1.2.0.9001")
  skip_if_not_installed("kknn")

  knn <- parsnip::nearest_neighbor("classification", "kknn", neighbors = tune(), weight_func = tune())
  mtcars_fair <- mtcars
  mtcars_fair$vs <- as.factor(mtcars_fair$vs)
  mtcars_fair$cyl <- as.factor(mtcars_fair$cyl)
  mtcars_fair$am <- as.factor(mtcars_fair$am)
  set.seed(4400)
  boots <- rsample::bootstraps(mtcars_fair, 3)
  n_grid <- 3

  set.seed(1)
  res <- tune_grid(
    knn, vs ~ mpg + hp + cyl, resamples = boots, grid = n_grid,
    metrics =
      yardstick::metric_set(
        yardstick::roc_auc,
        yardstick::demographic_parity(cyl),
        yardstick::demographic_parity(am)
      )
  )

  res_plot <- autoplot(res)

  expect_contains(
    res_plot$data$.metric,
    c("demographic_parity(am)", "demographic_parity(cyl)", "roc_auc")
  )
})

test_that("plot_perf_vs_iter with fairness metrics (#773)", {
  skip_on_cran()
  skip_if_not_installed("yardstick", minimum_version = "1.2.0.9001")
  skip_if_not_installed("kknn")

  knn <- parsnip::nearest_neighbor("classification", "kknn", neighbors = tune())
  mtcars_fair <- mtcars
  mtcars_fair$vs <- as.factor(mtcars_fair$vs)
  mtcars_fair$cyl <- as.factor(mtcars_fair$cyl)
  mtcars_fair$am <- as.factor(mtcars_fair$am)
  set.seed(4400)
  boots <- rsample::bootstraps(mtcars_fair, 3)
  n_grid <- 3

  set.seed(1)
  suppressMessages(
    res <- tune_bayes(
      knn, vs ~ mpg + hp + cyl, resamples = boots,
      metrics =
        yardstick::metric_set(
          yardstick::roc_auc,
          yardstick::demographic_parity(cyl),
          yardstick::demographic_parity(am)
        )
    )
  )

  res_plot <- autoplot(res, type = "performance")

  expect_contains(
    res_plot$data$.metric,
    c("demographic_parity(am)", "demographic_parity(cyl)", "roc_auc")
  )
})

test_that("regular grid plot", {
  skip_if_not_installed("ggplot2", minimum_version = "3.5.0")
  skip_if_not_installed("kernlab")

  svm_spec <-
    parsnip::svm_rbf(cost = tune()) %>%
    parsnip::set_engine("kernlab") %>%
    parsnip::set_mode("regression")

  svm_grid <-
    svm_spec %>%
    extract_parameter_set_dials() %>%
    dials::grid_regular(levels = 1)

  set.seed(1)
  res <-
    svm_spec %>%
    tune_grid(mpg ~ ., resamples = rsample::vfold_cv(mtcars, v = 5), grid = svm_grid)

  expect_snapshot(
    error = TRUE,
    autoplot(res)
  )
})

test_that("evaluation time warning for non-survival model", {
  skip_if_not_installed("kernlab")

  set.seed(1)
  res <-
    parsnip::svm_rbf(cost = tune()) %>%
    parsnip::set_engine("kernlab") %>%
    parsnip::set_mode("regression") %>%
    tune_grid(mpg ~ ., resamples = rsample::vfold_cv(mtcars, v = 5), grid = 2)

  expect_snapshot(foo <- autoplot(res, metric = "rmse", eval_time = 10))

})
