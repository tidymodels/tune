library(parsnip)
library(rsample)

expect_error_free <- function(...) {
  testthat::expect_error(..., regexp = NA)
}

expect_unequal <-
  function(object, expected, ...,
           tolerance = if (edition_get() >= 3) testthat_tolerance()) {
    expect_true(!compare(object, expected, tolerance = tolerance, ...)$equal)
  }


test_that("case weight identification", {
  folds1 <- vfold_cv(mtcars)
  expect_null(tune:::maybe_assessment_weights(folds1$splits[[1]]))
  expect_null(tune:::get_case_weight_data(mtcars))

  mtcars$.case_weight <- runif(nrow(mtcars))
  mtcars$.case_weight <- importance_weights(mtcars$.case_weight)
  folds2 <- vfold_cv(mtcars)
  expect_null(tune:::maybe_assessment_weights(folds2$splits[[1]]))
  expect_equal(tune:::get_case_weight_data(mtcars), mtcars$.case_weight)

  mtcars$.case_weight <- frequency_weights(1:32)
  folds3 <- vfold_cv(mtcars)
  expect_equal(
    tune:::maybe_assessment_weights(folds3$splits[[1]]),
    assessment(folds3$splits[[1]])$.case_weight
  )
  expect_equal(tune:::get_case_weight_data(mtcars), mtcars$.case_weight)

  expect_true(.use_case_weights_with_yardstick(frequency_weights(1:10)))
  expect_false(.use_case_weights_with_yardstick(importance_weights(seq(1, 10, by = .1))))
  expect_false(.use_case_weights_with_yardstick(seq(1, 10, by = .1)))

  expect_error(
    res1 <- fit_resamples(linear_reg(), mpg ~ ., resamples = folds1)
  )

  expect_error_free(res1 <- fit_resamples(linear_reg(), mpg ~ ., resamples = folds1))
  expect_error_free(res2 <- fit_resamples(linear_reg(), mpg ~ ., resamples = folds2))
  # expect_error_free(res3 <- fit_resamples(linear_reg(), mpg ~ ., resamples = folds2))

  # check for inequality

})
