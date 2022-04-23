library(parsnip)
library(rsample)


test_that("case weight identification", {
  set.seed(1)
  folds_no_wts <- vfold_cv(mtcars)
  expect_null(tune:::maybe_assessment_weights(folds_no_wts$splits[[1]]))
  expect_null(tune:::get_case_weight_data(mtcars))

  # ------------------------------------------------------------------------------

  mtcars$.case_weight <- seq(0, 1, length.out = 32)
  mtcars$.case_weight <- importance_weights(mtcars$.case_weight)
  set.seed(1)
  folds_imp_wts <- vfold_cv(mtcars)
  expect_null(tune:::maybe_assessment_weights(folds_imp_wts$splits[[1]]))
  expect_equal(tune:::get_case_weight_data(mtcars), mtcars$.case_weight)

  expect_error_free(
    res_no_wts <- fit_resamples(linear_reg(), mpg ~ ., resamples = folds_no_wts)
  )

  # ------------------------------------------------------------------------------

  mtcars$.case_weight <- frequency_weights(1:32)
  set.seed(1)
  folds_frq_wts <- vfold_cv(mtcars)
  expect_equal(
    tune:::maybe_assessment_weights(folds_frq_wts$splits[[1]]),
    assessment(folds_frq_wts$splits[[1]])$.case_weight
  )
  expect_equal(tune:::get_case_weight_data(mtcars), mtcars$.case_weight)

  expect_true(.use_case_weights_with_yardstick(frequency_weights(1:10)))
  expect_false(.use_case_weights_with_yardstick(importance_weights(seq(1, 10, by = .1))))
  expect_false(.use_case_weights_with_yardstick(seq(1, 10, by = .1)))

  expect_error_free(
    res_imp_wts <- fit_resamples(linear_reg(), mpg ~ ., resamples = folds_imp_wts)
  )

  # Error:
  # ! In metric: `rmse`
  # Problem while computing `.estimate = metric_fn(truth = mpg, estimate = .pred, na_rm = na_rm, case_weights = case_wts)`.
  # Caused by error in `vec_arith()`:
  # ! <double> * <frequency_weights> is not permitted
  # expect_error_free(res_frq_wts <- fit_resamples(linear_reg(), mpg ~ ., resamples = folds_frq_wts))

  # ------------------------------------------------------------------------------

  expect_unequal(collect_metrics(res_no_wts), collect_metrics(res_imp_wts))

})
