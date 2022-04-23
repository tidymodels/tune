new_rng_snapshots <- utils::compareVersion("3.6.0", as.character(getRversion())) > 0

helper_objects_tune <- function() {
  rec_tune_1 <-
    recipes::recipe(mpg ~ ., data = mtcars) %>%
    recipes::step_normalize(recipes::all_predictors()) %>%
    recipes::step_pca(recipes::all_predictors(), num_comp = tune())

  rec_no_tune_1 <-
    recipes::recipe(mpg ~ ., data = mtcars) %>%
    recipes::step_normalize(recipes::all_predictors())

  lm_mod <- parsnip::linear_reg() %>% parsnip::set_engine("lm")

  svm_mod <- parsnip::svm_rbf(mode = "regression", cost = tune()) %>%
    parsnip::set_engine("kernlab")

  list(
    rec_tune_1 = rec_tune_1,
    rec_no_tune_1 = rec_no_tune_1,
    lm_mod = lm_mod,
    svm_mod = svm_mod
  )
}


expect_error_free <- function(...) {
  testthat::expect_error(..., regexp = NA)
}

expect_unequal <-
  function(object, expected, ...,
           tolerance = if (edition_get() >= 3) testthat_tolerance()) {
    expect_true(!compare(object, expected, tolerance = tolerance, ...)$equal)
  }

