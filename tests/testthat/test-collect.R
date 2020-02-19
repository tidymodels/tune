context("collect")

test_that("`collect_predictions()` errors informatively if there is no `.predictions` column", {

  set.seed(6735)
  folds <- rsample::vfold_cv(mtcars, v = 2)

  lin_mod <- parsnip::linear_reg() %>%
    parsnip::set_engine("lm")

  result <- lin_mod %>% fit_resamples(mpg ~ ., folds)

  expect_error(
    collect_predictions(result),
    "The `.predictions` column does not exist."
  )
})
