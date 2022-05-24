expect_s3_class_tune_results <- function(x) {
  expect_s3_class(x, "tune_results")
}

expect_s3_class_bare_tibble <- function(x) {
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"), exact = TRUE)
}

# ------------------------------------------------------------------------------
# Test object used when testing vctrs/dplyr compatibility.
# Delayed assign to only load it in once.

load_helper_tune_results <- function() {
  list(
    resample = readRDS(test_path("data", "lm_resamples.rds")),
    tune = readRDS(test_path("data", "knn_results.rds")),
    bayes = readRDS(test_path("data", "lm_bayes.rds"))
  )
}

delayedAssign("helper_tune_results", load_helper_tune_results())
