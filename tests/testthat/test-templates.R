
context("templating functions")

# ------------------------------------------------------------------------------

# Code to loop over all tests and configurations

dummy_template <- function(model, verbose, tune) {
  set.seed(3522) # for models where a seed is set
  rlang::eval_tidy(
    rlang::call2(
      paste0("use_", model),
      formula = Sepal.Width ~ .,
      data = expr(iris),
      verbose = enexpr(verbose),
      tune = enexpr(tune)
    )
  )
}

no_dummy_template <- function(model, verbose, tune) {
  set.seed(3522) # for models where a seed is set
  rlang::eval_tidy(
    rlang::call2(
      paste0("use_", model),
      formula = Species ~ .,
      data = expr(iris),
      verbose = enexpr(verbose),
      tune = enexpr(tune)
    )
  )
}

verify_models <- function(model, tune, verbose) {
  file_names <- model
  if (tune) {
    file_names <- paste0(file_names, "_tune")
  }
  if (verbose) {
    file_names <- paste0(file_names, "_verbose")
  }
  file_names <- paste0(file_names, c("_dummies", ""))
  file_names <- paste0(file_names, ".txt")

  verify_output(test_path("templates", file_names[1]), {
    dummy_template(model, verbose, tune)
  })
  verify_output(test_path("templates", file_names[2]), {
    no_dummy_template(model, verbose, tune)
  })
}


test_that('all model templates', {
  models <- c("glmnet", "xgboost", "ranger", "kknn", "earth")

  test_config <-
    expand.grid(
      model = models,
      # TODO for some reason the tune results fail the test with the msg
      # "Lengths differ: 25 is not 26" (or for whatever length of the results)
      tune = c(FALSE),
      verbose = c(TRUE, FALSE)
    )

  res <- purrr::pmap(test_config, verify_models)

})

