library(parsnip)
library(recipes)
library(modeldata)

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


test_that('finalize recipe step with multiple tune parameters', {
  data(biomass)

  model_spec <- linear_reg() %>%
    set_engine("lm")

  rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur, data = biomass) %>%
    step_bs(carbon, hydrogen, deg_free = tune(), degree = tune())

  best <- tibble(deg_free = 2, degree = 1, .config = "Preprocessor1_Model1")

  expect_s3_class(finalize_recipe(rec, best), "recipe")
  expect_equal(finalize_recipe(rec, best)$steps[[1]]$degree, 1)
  expect_equal(finalize_recipe(rec, best)$steps[[1]]$deg_free, 2)
})
