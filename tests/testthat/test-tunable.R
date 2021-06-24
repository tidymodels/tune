context("tunable parameter info")

# ------------------------------------------------------------------------------

source(test_path("../helper-objects.R"))

# ------------------------------------------------------------------------------

test_that('recipe with no steps', {
  bare_info <- tunable(bare_rec)
  check_tunable_tibble(bare_info)
  expect_equal(nrow(bare_info), 0)
})

test_that('recipe with no tunable parameters', {
  rm_info <- tunable(rm_rec)
  check_tunable_tibble(rm_info)
  expect_equal(nrow(rm_info), 0)
})

test_that('recipe with tunable parameters', {
  spline_info <- tunable(spline_rec)
  check_tunable_tibble(spline_info)
  if (utils::packageVersion("recipes") <= "0.1.15") {
    expected_cols <- c('step_knnimpute', 'step_other', 'step_bs', 'step_bs')
  } else {
    expected_cols <- c('step_impute_knn', 'step_other', 'step_bs', 'step_bs')
  }
  expect_equal(
    spline_info$component,
    expected_cols
  )
  expect_true(all(spline_info$source == "recipe"))
  nms <- c('neighbors', 'threshold', 'deg_free', 'degree')
  expect_equal(spline_info$name, nms)
  expect_true(all(purrr::map_lgl(spline_info$call_info, ~ .x$pkg == "dials")))
  nms <- c('neighbors', 'threshold', 'spline_degree', 'degree_int')
  expect_equal(purrr::map_chr(spline_info$call_info, ~ .x$fun), nms)
})

# ------------------------------------------------------------------------------

test_that('model with no parameters', {
  skip_if_not_installed("parsnip")
  lm_info <- tunable(lm_model)
  check_tunable_tibble(lm_info)
  expect_equal(nrow(lm_info), 0)
})

test_that('model with main and engine parameters', {
  skip_if_not_installed("parsnip")
  c5_info <- tunable(bst_model)
  check_tunable_tibble(c5_info)
  expect_equal(nrow(c5_info), 9)
  expect_true(all(c5_info$source == "model_spec"))
  expect_true(all(c5_info$component == "boost_tree"))
  expect_true(all(c5_info$component_id[1:3] == "main"))
  expect_true(all(c5_info$component_id[-(1:3)] == "engine"))
  nms <- c("trees", "min_n", "sample_size", "rules", "CF", "noGlobalPruning",
           "winnow", "fuzzyThreshold", "bands")
  expect_equal(c5_info$name, nms)
  expect_true(all(purrr::map_lgl(c5_info$call_info[1:3], ~ .x$pkg == "dials")))
  expect_equal(
    purrr::map_chr(c5_info$call_info[1:3], ~ .x$fun),
    c("trees", "min_n", "sample_prop")
  )
  expect_true(sum(purrr::map_lgl(c5_info$call_info, is.null)) == 1)
})


test_that('bad model inputs', {
  skip_if_not_installed("parsnip")
  bad_class <- lm_model
  class(bad_class) <- c("potato", "model_spec")
  expect_error(
    tunable(bad_class),
    "model database doesn't know about the arguments for model"
  )
})


