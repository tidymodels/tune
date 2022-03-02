# ------------------------------------------------------------------------------

test_that('parameters.recipe() still works', {
  withr::local_options(lifecycle_verbosity = "quiet")

  spline_info <- dials::parameters(spline_rec)
  check_param_set_tibble(spline_info)
})

# ------------------------------------------------------------------------------

test_that('parameters.model_spec() still works', {
  withr::local_options(lifecycle_verbosity = "quiet")

  skip_if_not_installed("parsnip")
  c5_info <- dials::parameters(bst_model)
  check_param_set_tibble(c5_info)
})


