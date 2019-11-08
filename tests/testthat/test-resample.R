test_that("cannot autoplot `fit_resamples()` results", {
  set.seed(6735)
  folds <- vfold_cv(mtcars, v = 2)

  lin_mod <- linear_reg() %>%
    set_engine("lm")

  result <- fit_resamples(mpg ~ ., lin_mod, folds)

  expect_error(autoplot(result), "no `autoplot[(][])]` implementation for `resample_results`")
})
