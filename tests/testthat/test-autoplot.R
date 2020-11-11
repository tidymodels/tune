context("autoplot")

# ------------------------------------------------------------------------------

source(test_path("../helper-objects.R"))
knn_results <- readRDS(test_path("knn_results.rds"))
svm_results <- readRDS(test_path("svm_results.rds"))
svm_reg_results <- readRDS(test_path("svm_reg_results.rds"))
rcv_results <- readRDS(test_path("rcv_results.rds"))
load(test_path("test_objects.RData"))

# ------------------------------------------------------------------------------

test_that("two quantitative predictor marginal plot",{
  p <- autoplot(svm_results)
  expect_is(p, "ggplot")
  expect_equal(names(p$data), c('mean', '# resamples', '.metric', 'name', 'value'))
  expect_equal(rlang::get_expr(p$mapping$x), expr(value))
  expect_equal(rlang::get_expr(p$mapping$y), expr(mean))
  expect_equal(p$labels$y, "")
  expect_equal(p$labels$x, "")

  p <- autoplot(svm_results, metric = "accuracy")
  expect_true(isTRUE(all.equal(unique(p$data$.metric), "accuracy")))
})


test_that("two quantitative predictor and one qualitative marginal plot",{
  p <- autoplot(knn_results)
  expect_is(p, "ggplot")
  expect_equal(
    names(p$data),
    c('Distance Weighting Function', 'mean', '# resamples', '.metric', 'name', 'value')
  )
  expect_equal(rlang::get_expr(p$mapping$x), expr(value))
  expect_equal(rlang::get_expr(p$mapping$y), expr(mean))
  expect_equal(p$labels$y, "")
  expect_equal(p$labels$x, "")
  expect_equal(p$labels$colour, "Distance Weighting Function")
})

test_that("not marginal plot with grid search",{
  expect_error(
    autoplot(knn_results, type = "performance"),
    "`type = performance` is only used iterative search results."
  )
  expect_error(
    autoplot(knn_results, type = "parameters"),
    "`type = parameters` is only used iterative search results."
  )
})


test_that("marginal plot labels and transformations - irregular grid",{
  p <- autoplot(svm_results)
  expect_is(p, "ggplot")
  expect_equal(names(p$data), c('mean', '# resamples', '.metric', 'name', 'value'))
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
    tol = 0.001
  )
  expect_equal(
    sort(unique(collect_metrics(svm_results)$scale_factor)),
    sort(unique(10^(p$data$value[p$data$name == "Scale Factor (log-10)"]))),
    tol = 0.001
  )

  p <- autoplot(svm_results, metric = "accuracy")
  expect_true(isTRUE(all.equal(unique(p$data$.metric), "accuracy")))
})

# ------------------------------------------------------------------------------

test_that("marginal plot for iterative search",{
  p <- autoplot(mt_spln_knn_bo_sep)
  expect_is(p, "ggplot")
  expect_equal(
    names(p$data),
    c('Distance Weighting Function', 'mean', '# resamples', '.metric', 'name', 'value')
  )
  expect_equal(rlang::get_expr(p$mapping$x), expr(value))
  expect_equal(rlang::get_expr(p$mapping$y), expr(mean))
  expect_equal(p$labels$y, "")
  expect_equal(p$labels$x, "")
  expect_equal(p$labels$colour, "Distance Weighting Function")

  p <- autoplot(mt_spln_knn_bo_sep, metric = "rmse")
  expect_true(isTRUE(all.equal(unique(p$data$.metric), "rmse")))
})


test_that("performance plot for iterative search",{
  p <- autoplot(mt_spln_knn_bo_sep, type = "performance")
  expect_is(p, "ggplot")
  expect_equal(names(p$data),
               c('K', 'weight_func', 'deg_free', '.metric',
                 '.estimator', 'mean', 'n', 'std_err', '.config', '.iter'))
  expect_equal(rlang::get_expr(p$mapping$x), expr(.iter))
  expect_equal(rlang::get_expr(p$mapping$y), expr(mean))
  expect_equal(p$labels$x, "Iteration")
  expect_equal(p$labels$y, "mean")
  expect_equal(p$labels$ymin, "mean - const * std_err")
  expect_equal(p$labels$ymax, "mean + const * std_err")

  p <- autoplot(mt_spln_knn_bo_sep, type = "performance", metric = "rmse")
  expect_true(isTRUE(all.equal(unique(p$data$.metric), "rmse")))

  p <- autoplot(mt_spln_knn_bo_sep, type = "performance", width = 0)
  expect_true(isTRUE(all.equal(names(p$mapping), c("x", "y"))))
})


test_that("parameter plot for iterative search",{
  p <- autoplot(mt_spln_knn_bo_sep, type = "parameters")
  expect_is(p, "ggplot")
  expect_equal(names(p$data), c('.iter', 'name', 'value'))

  name_vals <- sort(unique(p$data$name))
  expect_equal(name_vals, c("K", "Piecewise Polynomial Degree"))
  expect_equal(rlang::get_expr(p$mapping$x), expr(.iter))
  expect_equal(rlang::get_expr(p$mapping$y), expr(value))
  expect_equal(p$labels$y, "")
  expect_equal(p$labels$x, "Iteration")
})


test_that("regular grid plot",{
  p <- autoplot(rcv_results)
  expect_is(p, "ggplot")
  expect_equal(
    names(p$data),
    c("degree", "wt df", "wt degree", "mean", '# resamples', ".metric", "name", "value")
  )
  expect_equal(rlang::get_expr(p$mapping$x), expr(value))
  expect_equal(rlang::get_expr(p$mapping$y), expr(mean))
  expect_equal(rlang::get_expr(p$mapping$col), sym("wt df"))
  expect_equal(rlang::get_expr(p$mapping$group), sym("wt df"))
  expect_equal(p$facet$vars(), c(".metric", "degree", "wt degree"))

  expect_equal(p$labels$y, "")
  expect_equal(p$labels$x, "deg_free")

  p <- autoplot(rcv_results, metric = "rmse")
  expect_is(p, "ggplot")
  expect_equal(
    names(p$data),
    c("degree", "wt df", "wt degree", "mean", '# resamples', ".metric", "name", "value")
  )
  expect_equal(rlang::get_expr(p$mapping$x), expr(value))
  expect_equal(rlang::get_expr(p$mapping$y), expr(mean))
  expect_equal(rlang::get_expr(p$mapping$col), sym("wt df"))
  expect_equal(rlang::get_expr(p$mapping$group), sym("wt df"))
  expect_equal(p$facet$vars(), c("degree", "wt degree"))

  expect_equal(p$labels$y, "rmse")
  expect_equal(p$labels$x, "deg_free")

  p <- autoplot(svm_reg_results)
  expect_is(p, "ggplot")
  expect_equal(
    names(p$data),
    c("%^*#", "Scale Factor",  "mean", '# resamples', ".metric", "name", "value")
  )
  expect_equal(rlang::get_expr(p$mapping$x), expr(value))
  expect_equal(rlang::get_expr(p$mapping$y), expr(mean))
  expect_equal(rlang::get_expr(p$mapping$col), sym("%^*#"))
  expect_equal(rlang::get_expr(p$mapping$group), sym("%^*#"))
  expect_equal(p$facet$vars(), c(".metric", "Scale Factor"))

  expect_equal(p$labels$y, "")
  expect_equal(p$labels$colour, as.name("%^*#"))
  expect_equal(p$labels$x, "Cost")
  expect_equal(p$labels$group, "%^*#")

  expect_equal(class(p$scales$scales[[1]]$trans), "trans")
  expect_equal(p$scales$scales[[1]]$trans$name, "log-2")
  expect_equal(unique(p$data$name), "Cost")
})



test_that("coord_obs_pred",{
  data(solubility_test, package = "modeldata")

  library(ggplot2)
  p <-
    ggplot(solubility_test, aes(x = solubility, y = prediction)) +
    geom_abline(lty = 2) +
    geom_point(alpha = 0.5)

  rng <- range(solubility_test[[1]], solubility_test[[2]])

  p2 <- p + coord_obs_pred()

  expect_error(print(p2), regexp = NA)

  expect_true(inherits(p2$coordinates, "CoordObsPred"))
  expect_equal(p2$coordinates$limits$x, rng)
  expect_equal(p2$coordinates$limits$y, rng)
  expect_equal(p2$coordinates$ratio, 1)

  solubility_test$solubility[1] <- NA
  p3 <-
    ggplot(solubility_test, aes(x = solubility, y = prediction)) +
    geom_abline(lty = 2) +
    geom_point(alpha = 0.5)

  expect_warning(
    print(p3 + coord_obs_pred()),
    "Removed 1 rows containing missing values"
  )


})



