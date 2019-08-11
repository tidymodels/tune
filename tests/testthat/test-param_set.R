context("parameter sets from complex objects")

# ------------------------------------------------------------------------------

source("helper-objects.R")

# ------------------------------------------------------------------------------

test_that('recipe with no steps', {
  bare_info <- param_set(bare_rec)
  check_param_set_tibble(bare_info)
  expect_equal(nrow(bare_info), 0)
})

test_that('recipe with no tunable parameters', {
  rm_info <- param_set(rm_rec)
  check_param_set_tibble(rm_info)
  expect_equal(nrow(rm_info), 0)
})

test_that('recipe with tunable parameters', {
  isomap_info <- param_set(isomap_rec)
  check_param_set_tibble(isomap_info)
  expect_equal(
    isomap_info$component,
    c('step_knnimpute', 'step_other', 'step_isomap', 'step_isomap'),
  )
  expect_true(all(isomap_info$source == "recipe"))
  nms <- c('neighbors', 'threshold', 'num_terms', 'neighbors')
  expect_equal(isomap_info$name, nms)
  ids <- c('imputation', 'threshold', 'num_terms', 'neighbors')
  expect_equal(isomap_info$id, ids)

  expect_equal(isomap_info$object[[1]], dials::neighbors())
  expect_equal(isomap_info$object[[2]], dials::threshold())
  expect_equal(isomap_info$object[[3]], dials::num_terms())
  expect_equal(isomap_info$object[[4]], dials::neighbors())

})

# ------------------------------------------------------------------------------

test_that('model with no parameters', {
  skip_if_not_installed("parsnip")
  lm_info <- param_set(lm_model)
  check_param_set_tibble(lm_info)
  expect_equal(nrow(lm_info), 0)
})

test_that('model with main and engine parameters', {
  skip_if_not_installed("parsnip")
  c5_info <- param_set(bst_model)
  check_param_set_tibble(c5_info)
  expect_equal(nrow(c5_info), 2)
  expect_true(all(c5_info$source == "model_spec"))
  expect_true(all(c5_info$component == "boost_tree"))
  expect_equal(c5_info$component_id, c("main", "engine"))
  nms <- c("trees", "rules")
  expect_equal(c5_info$name, nms)
  ids <- c("funky name \n", "rules")
  expect_equal(c5_info$id, ids)

  expect_equal(c5_info$object[[1]], dials::trees())
  expect_equal(c5_info$object[[2]], NA)
})


