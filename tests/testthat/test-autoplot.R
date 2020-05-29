context("autoplot")

# ------------------------------------------------------------------------------

source(test_path("../helper-objects.R"))
knn_results <- readRDS(test_path("knn_results.rds"))
load(test_path("svm_results.RData"))
load(test_path("bayes_example.RData"))
rcv_results <- readRDS(test_path("rcv_results.rds"))

# ------------------------------------------------------------------------------

# move these to test_objects.R after merged.
library(parsnip)
library(rsample)

svm_mod <-
  svm_rbf(cost = tune("svm cost"), rbf_sigma = tune(), margin = tune()) %>%
  set_engine("kernlab") %>%
  set_mode("regression")

set.seed(92393)
mt_bt <- bootstraps(mtcars, times = 3)

svm_reg <-
  svm_mod %>%
  tune_grid(mpg ~ .,
            resamples = mt_bt,
            grid = grid_regular(parameters(svm_mod), levels = c(3, 2, 2)))

set.seed(38)
svm_irreg <-
  svm_mod %>%
  tune_grid(mpg ~ .,
            resamples = mt_bt,
            grid = grid_latin_hypercube(parameters(svm_mod), size = 5))

set.seed(4189)
svm_bo <-
  svm_mod %>%
  tune_bayes(mpg ~ .,
             resamples = svm_reg,
             initial = svm_irreg,
             iter = 2)


# ------------------------------------------------------------------------------

test_that("two quantitative predictor marginal plot",{
  p <- autoplot(svm_results)
  expect_is(p, "ggplot")
  expect_equal(names(p$data), c('mean', '.metric', 'name', 'value'))
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
    c('Distance Weighting Function', 'mean', '.metric', 'name', 'value')
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

# ------------------------------------------------------------------------------

test_that("marginal plot for iterative search",{
  p <- autoplot(search_res)
  expect_is(p, "ggplot")
  expect_equal(
    names(p$data),
    c('Distance Weighting Function', 'mean', '.metric', 'name', 'value')
  )
  expect_equal(rlang::get_expr(p$mapping$x), expr(value))
  expect_equal(rlang::get_expr(p$mapping$y), expr(mean))
  expect_equal(p$labels$y, "")
  expect_equal(p$labels$x, "")
  expect_equal(p$labels$colour, "Distance Weighting Function")

  p <- autoplot(svm_results, metric = "accuracy")
  expect_true(isTRUE(all.equal(unique(p$data$.metric), "accuracy")))
})


test_that("performance plot for iterative search",{
  p <- autoplot(search_res, type = "performance")
  expect_is(p, "ggplot")
  expect_equal(names(p$data),
               c('neighbors', 'weight_func',
                 'dist_power', 'degrees of freedom',
                 '.iter', '.metric', '.estimator', 'mean', 'n', 'std_err'))
  expect_equal(rlang::get_expr(p$mapping$x), expr(.iter))
  expect_equal(rlang::get_expr(p$mapping$y), expr(mean))
  expect_equal(p$labels$x, "Iteration")
  expect_equal(p$labels$y, "mean")
  expect_equal(p$labels$ymin, "mean - const * std_err")
  expect_equal(p$labels$ymax, "mean + const * std_err")

  p <- autoplot(search_res, type = "performance", metric = "accuracy")
  expect_true(isTRUE(all.equal(unique(p$data$.metric), "accuracy")))

  p <- autoplot(search_res, type = "performance", width = 0)
  expect_true(isTRUE(all.equal(names(p$mapping), c("x", "y"))))
})


test_that("parameter plot for iterative search",{
  p <- autoplot(search_res, type = "parameters")
  expect_is(p, "ggplot")
  expect_equal(names(p$data), c('.iter', 'name', 'value'))
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
    c("degree", "wt df", "wt degree", "mean", ".metric", "name", "value")
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
    c("degree", "wt df", "wt degree", "mean", ".metric", "name", "value")
  )
  expect_equal(rlang::get_expr(p$mapping$x), expr(value))
  expect_equal(rlang::get_expr(p$mapping$y), expr(mean))
  expect_equal(rlang::get_expr(p$mapping$col), sym("wt df"))
  expect_equal(rlang::get_expr(p$mapping$group), sym("wt df"))
  expect_equal(p$facet$vars(), c("degree", "wt degree"))

  expect_equal(p$labels$y, "rmse")
  expect_equal(p$labels$x, "deg_free")
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



