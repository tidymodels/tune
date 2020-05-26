context("autoplot")

# ------------------------------------------------------------------------------

source(test_path("../helper-objects.R"))
knn_results <- readRDS(test_path("knn_results.rds"))
load(test_path("svm_results.RData"))
load(test_path("bayes_example.RData"))
rcv_results <- readRDS(test_path("rcv_results.rds"))

# ------------------------------------------------------------------------------

test_that("two quantitative predictor marginal plot",{
  p <- autoplot(svm_results)
  expect_is(p, "ggplot")
  expect_equal(names(p$data), c('mean', '.metric', 'name', 'value'))
  expect_equal(rlang::get_expr(p$mapping$x), expr(value))
  expect_equal(rlang::get_expr(p$mapping$y), expr(mean))
  expect_equal(p$labels$y, "Performance")
  expect_equal(p$labels$x, "Parameter Value")

  p <- autoplot(svm_results, metric = "accuracy")
  expect_true(isTRUE(all.equal(unique(p$data$.metric), "accuracy")))
})


test_that("two quantitative predictor and one qualitative marginal plot",{
  p <- autoplot(knn_results)
  expect_is(p, "ggplot")
  expect_equal(names(p$data), c('weight_func', 'mean', '.metric', 'name', 'value'))
  expect_equal(rlang::get_expr(p$mapping$x), expr(value))
  expect_equal(rlang::get_expr(p$mapping$y), expr(mean))
  expect_equal(p$labels$y, "Performance")
  expect_equal(p$labels$x, "Parameter Value")
  expect_equal(p$labels$colour, "weight_func")
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
  expect_equal(names(p$data), c('weight_func', 'mean', '.metric', 'name', 'value'))
  expect_equal(rlang::get_expr(p$mapping$x), expr(value))
  expect_equal(rlang::get_expr(p$mapping$y), expr(mean))
  expect_equal(p$labels$y, "Performance")
  expect_equal(p$labels$x, "Parameter Value")
  expect_equal(p$labels$colour, "weight_func")

  p <- autoplot(svm_results, metric = "accuracy")
  expect_true(isTRUE(all.equal(unique(p$data$.metric), "accuracy")))
})


test_that("performance plot for iterative search",{
  p <- autoplot(search_res, type = "performance")
  expect_is(p, "ggplot")
  expect_equal(names(p$data),
               c('neighbors', 'weight_func', 'dist_power', 'degrees of freedom',
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
  expect_equal(p$labels$y, "Parameter Value")
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

  # expect_equal(p$labels$y, "Performance")
  # expect_equal(p$labels$x, "Parameter Value")

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

  # expect_equal(p$labels$y, "Performance")
  # expect_equal(p$labels$x, "Parameter Value")
})

