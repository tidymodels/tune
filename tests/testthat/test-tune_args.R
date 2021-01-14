context("discovering tune() in arguments")

# ------------------------------------------------------------------------------

source(test_path("../helper-objects.R"))

# ------------------------------------------------------------------------------

test_that('recipe with no steps', {
  bare_info <- tune_args(bare_rec)
  check_tune_args_tibble(bare_info)
  expect_equal(nrow(bare_info), 0)
})

test_that('recipe with no tunable parameters', {
  rm_info <- tune_args(rm_rec)
  check_tune_args_tibble(rm_info)
  expect_equal(nrow(rm_info), 0)
})

test_that('recipe with tunable parameters', {
  spline_info <- tune_args(spline_rec)
  check_tune_args_tibble(spline_info)
  if (utils::packageVersion("recipes") <= "0.1.15") {
    expected_cols <- c('step_knnimpute', 'step_other', 'step_bs', 'step_bs')
  } else {
    expected_cols <- c('step_impute_knn', 'step_other', 'step_bs', 'step_bs')
  }
  expect_equal(
    spline_info$component,
    expected_cols,
  )
  expect_true(all(spline_info$source == "recipe"))
  nms <- c('neighbors', 'threshold', 'deg_free', 'degree')
  expect_equal(spline_info$name, nms)
  ids <- c('imputation', 'threshold', 'deg_free', 'degree')
  expect_equal(spline_info$id, ids)
})

# ------------------------------------------------------------------------------

test_that('model with no parameters', {
  skip_if_not_installed("parsnip")
  lm_info <- tune_args(lm_model)
  check_tune_args_tibble(lm_info)
  expect_equal(nrow(lm_info), 0)
})

test_that('model with main and engine parameters', {
  skip_if_not_installed("parsnip")
  c5_info <- tune_args(bst_model)
  check_tune_args_tibble(c5_info)
  expect_equal(nrow(c5_info), 2)
  expect_true(all(c5_info$source == "model_spec"))
  expect_true(all(c5_info$component == "boost_tree"))
  expect_true(all(is.na(c5_info$component_id)))
  nms <- c("trees", "rules")
  expect_equal(c5_info$name, nms)
  ids <- c("funky name \n", "rules")
  expect_equal(c5_info$id, ids)
})


