context("parameter sets from complex objects")

# ------------------------------------------------------------------------------

source(test_path("../helper-objects.R"))

# ------------------------------------------------------------------------------

test_that('recipe with no steps', {
  bare_info <- dials::parameters(bare_rec)
  check_param_set_tibble(bare_info)
  expect_equal(nrow(bare_info), 0)
})

test_that('recipe with no tunable parameters', {
  rm_info <- dials::parameters(rm_rec)
  check_param_set_tibble(rm_info)
  expect_equal(nrow(rm_info), 0)
})

test_that('recipe with tunable parameters', {
  spline_info <- dials::parameters(spline_rec)
  check_param_set_tibble(spline_info)
  expect_equal(
    spline_info$component,
    c('step_knnimpute', 'step_other', 'step_bs', 'step_bs'),
  )
  expect_true(all(spline_info$source == "recipe"))
  nms <- c('neighbors', 'threshold', 'deg_free', 'degree')
  expect_equal(spline_info$name, nms)
  ids <- c('imputation', 'threshold', 'deg_free', 'degree')
  expect_equal(spline_info$id, ids)

  expect_equal(spline_info$object[[1]], dials::neighbors(c(1, 10)))
  expect_equal(spline_info$object[[2]], dials::threshold(c(0, 1/10)))
  expect_equal(spline_info$object[[3]], dials::deg_free(c(3, 15)))
  expect_equal(spline_info$object[[4]], dials::degree_int(c(1, 2)))

})

# ------------------------------------------------------------------------------

test_that('model with no parameters', {
  skip_if_not_installed("parsnip")
  lm_info <- dials::parameters(lm_model)
  check_param_set_tibble(lm_info)
  expect_equal(nrow(lm_info), 0)
})

test_that('model with main and engine parameters', {
  skip_if_not_installed("parsnip")
  c5_info <- dials::parameters(bst_model)
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


