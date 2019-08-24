context("discovering tune() in arguments")

# ------------------------------------------------------------------------------

source("../helper-objects.R")

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
  isomap_info <- tune_args(isomap_rec)
  check_tune_args_tibble(isomap_info)
  expect_equal(
    isomap_info$component,
    c('step_knnimpute', 'step_other', 'step_isomap', 'step_isomap'),
  )
  expect_true(all(isomap_info$source == "recipe"))
  nms <- c('neighbors', 'threshold', 'num_terms', 'neighbors')
  expect_equal(isomap_info$name, nms)
  ids <- c('imputation', 'threshold', 'num_terms', 'neighbors')
  expect_equal(isomap_info$id, ids)
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


