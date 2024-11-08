test_that("compute_grid_info - recipe only", {
  library(workflows)
  library(recipes)
  library(parsnip)
  library(dials)

  rec <- recipe(mpg ~ ., mtcars) %>% step_spline_natural(deg_free = tune())

  wflow <- workflow()
  wflow <- add_model(wflow, boost_tree(mode = "regression"))
  wflow <- add_recipe(wflow, rec)

  grid <- grid_space_filling(extract_parameter_set_dials(wflow))
  res <- compute_grid_info(wflow, grid)

  expect_equal(res$.iter_preprocessor, 1:5)
  expect_equal(res$.msg_preprocessor, paste0("preprocessor ", 1:5, "/5"))
  expect_equal(res$deg_free, grid$deg_free)
  expect_equal(res$.iter_model, rep(1, 5))
  expect_equal(res$.iter_config, as.list(paste0("Preprocessor", 1:5, "_Model1")))
  expect_equal(res$.msg_model, paste0("preprocessor ", 1:5, "/5, model 1/1"))
  expect_equal(res$.submodels, list(list(), list(), list(), list(), list()))
  expect_named(
    res,
    c(".iter_preprocessor", ".msg_preprocessor", "deg_free", ".iter_model",
      ".iter_config", ".msg_model", ".submodels"),
    ignore.order = TRUE
  )
  expect_equal(nrow(res), 5)
})

test_that("compute_grid_info - model only (no submodels)", {
  library(workflows)
  library(parsnip)
  library(dials)

  spec <- boost_tree(mode = "regression", learn_rate = tune())

  wflow <- workflow()
  wflow <- add_model(wflow, spec)
  wflow <- add_formula(wflow, mpg ~ .)

  grid <- grid_space_filling(extract_parameter_set_dials(wflow))
  res <- compute_grid_info(wflow, grid)

  expect_equal(res$.iter_preprocessor, rep(1, 5))
  expect_equal(res$.msg_preprocessor, rep("preprocessor 1/1", 5))
  expect_equal(res$learn_rate, grid$learn_rate)
  expect_equal(res$.iter_model, 1:5)
  expect_equal(res$.iter_config, as.list(paste0("Preprocessor1_Model", 1:5)))
  expect_equal(res$.msg_model, paste0("preprocessor 1/1, model ", 1:5, "/5"))
  expect_equal(res$.submodels, list(list(), list(), list(), list(), list()))
  expect_named(
    res,
    c(".iter_preprocessor", ".msg_preprocessor", "learn_rate", ".iter_model",
      ".iter_config", ".msg_model", ".submodels"),
    ignore.order = TRUE
  )
  expect_equal(nrow(res), 5)
})

test_that("compute_grid_info - model only (with submodels)", {
  library(workflows)
  library(parsnip)
  library(dials)

  spec <- boost_tree(mode = "regression", trees = tune())

  wflow <- workflow()
  wflow <- add_model(wflow, spec)
  wflow <- add_formula(wflow, mpg ~ .)

  grid <- grid_space_filling(extract_parameter_set_dials(wflow))
  res <- compute_grid_info(wflow, grid)

  expect_equal(res$.iter_preprocessor, 1)
  expect_equal(res$.msg_preprocessor, "preprocessor 1/1")
  expect_equal(res$trees, max(grid$trees))
  expect_equal(res$.iter_model, 1)
  expect_equal(res$.iter_config, list(paste0("Preprocessor1_Model", 1:5)))
  expect_equal(res$.msg_model, "preprocessor 1/1, model 1/1")
  expect_equal(res$.submodels, list(list(trees = grid$trees[-which.max(grid$trees)])))
  expect_named(
    res,
    c(".iter_preprocessor", ".msg_preprocessor", "trees", ".iter_model",
      ".iter_config", ".msg_model", ".submodels"),
    ignore.order = TRUE
  )
  expect_equal(nrow(res), 1)
})

test_that("compute_grid_info - recipe and model (no submodels)", {
  library(workflows)
  library(parsnip)
  library(recipes)
  library(dials)

  rec <- recipe(mpg ~ ., mtcars) %>% step_spline_natural(deg_free = tune())
  spec <- boost_tree(mode = "regression", learn_rate = tune())

  wflow <- workflow()
  wflow <- add_model(wflow, spec)
  wflow <- add_recipe(wflow, rec)

  grid <- grid_space_filling(extract_parameter_set_dials(wflow))
  res <- compute_grid_info(wflow, grid)

  expect_equal(res$.iter_preprocessor, 1:5)
  expect_equal(res$.msg_preprocessor, paste0("preprocessor ", 1:5, "/5"))
  expect_equal(res$learn_rate, grid$learn_rate)
  expect_equal(res$deg_free, grid$deg_free)
  expect_equal(res$.iter_model, rep(1, 5))
  expect_equal(res$.iter_config, as.list(paste0("Preprocessor", 1:5, "_Model1")))
  expect_equal(res$.msg_model, paste0("preprocessor ", 1:5, "/5, model 1/1"))
  expect_equal(res$.submodels, list(list(), list(), list(), list(), list()))
  expect_named(
    res,
    c(".iter_preprocessor", ".msg_preprocessor", "deg_free", "learn_rate",
      ".iter_model", ".iter_config", ".msg_model", ".submodels"),
    ignore.order = TRUE
  )
  expect_equal(nrow(res), 5)
})

test_that("compute_grid_info - recipe and model (with submodels)", {
  library(workflows)
  library(parsnip)
  library(recipes)
  library(dials)

  rec <- recipe(mpg ~ ., mtcars) %>% step_spline_natural(deg_free = tune())
  spec <- boost_tree(mode = "regression", trees = tune())

  wflow <- workflow()
  wflow <- add_model(wflow, spec)
  wflow <- add_recipe(wflow, rec)

  # use grid_regular to trigger submodel trick
  set.seed(1)
  grid <- grid_regular(extract_parameter_set_dials(wflow))
  res <- compute_grid_info(wflow, grid)

  expect_equal(res$.iter_preprocessor, 1:3)
  expect_equal(res$.msg_preprocessor, paste0("preprocessor ", 1:3, "/3"))
  expect_equal(res$trees, rep(max(grid$trees), 3))
  expect_equal(res$.iter_model, rep(1, 3))
  expect_equal(
    res$.iter_config,
    list(
      c("Preprocessor1_Model1", "Preprocessor1_Model2", "Preprocessor1_Model3"),
      c("Preprocessor2_Model1", "Preprocessor2_Model2", "Preprocessor2_Model3"),
      c("Preprocessor3_Model1", "Preprocessor3_Model2", "Preprocessor3_Model3")
    )
  )
  expect_equal(res$.msg_model, paste0("preprocessor ", 1:3, "/3, model 1/1"))
  expect_equal(
    res$.submodels,
    list(
      list(trees = c(1L, 1000L)),
      list(trees = c(1L, 1000L)),
      list(trees = c(1L, 1000L))
    )
  )
  expect_named(
    res,
    c(".iter_preprocessor", ".msg_preprocessor", "deg_free", "trees",
      ".iter_model", ".iter_config", ".msg_model", ".submodels"),
    ignore.order = TRUE
  )
  expect_equal(nrow(res), 3)
})
