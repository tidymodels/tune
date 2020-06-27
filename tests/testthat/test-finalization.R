context("finalization")

library(parsnip)
library(recipes)

## -----------------------------------------------------------------------------

set.seed(21983)
rs <- rsample::vfold_cv(mtcars)

mod_1 <-
  rand_forest(mtry = tune(), trees = 20, min_n = tune()) %>%
  set_engine("randomForest") %>%
  set_mode("regression")

rec_1 <-
  recipe(mpg ~ ., data = mtcars) %>%
  step_ns(disp, deg_free = tune())

rec_2 <-
  recipe(mpg ~ ., data = mtcars) %>%
  step_ns(disp, deg_free = 3)

test_that('cannot finalize with recipe parameters', {
  skip_if_not_installed("randomForest")

  expect_error(
    mod_1 %>% tune_grid(rec_1, resamples = rs, grid = 3),
    "Some tuning parameters require finalization"
  )

  set.seed(987323)
  expect_error(
    mod_1 %>% tune_grid(rec_2, resamples = rs, grid = 3),
    regex = NA
  )

})


test_that('skip error if grid is supplied', {
  skip_if_not_installed("randomForest")

  grid <- tibble::tibble(mtry = 1:3, deg_free = c(3, 3, 4), min_n = c(5,4,6))

  set.seed(987323)
  expect_error(
    mod_1 %>% tune_grid(rec_1, resamples = rs, grid = grid),
    regex = NA
  )

})

