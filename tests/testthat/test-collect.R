context("collect")

test_that("`collect_predictions()` errors informatively if there is no `.predictions` column", {
  set.seed(6735)
  folds <- vfold_cv(mtcars, v = 2)

  lin_mod <- linear_reg() %>%
    set_engine("lm")

  result <- fit_resamples(mpg ~ ., lin_mod, folds)

  expect_error(
    collect_predictions(result),
    "The `.predictions` column does not exist."
  )
})
