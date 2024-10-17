test_that("cannot finalize with recipe parameters", {
  skip_if_not_installed("randomForest")
  skip_if_not_installed("splines2")

  set.seed(21983)
  rs <- rsample::vfold_cv(mtcars)

  mod_1 <-
    parsnip::rand_forest(mtry = tune(), trees = 20, min_n = tune()) %>%
    parsnip::set_engine("randomForest") %>%
    parsnip::set_mode("regression")

  rec_1 <-
    recipes::recipe(mpg ~ ., data = mtcars) %>%
    recipes::step_spline_natural(disp, deg_free = tune())

  rec_2 <-
    recipes::recipe(mpg ~ ., data = mtcars) %>%
    recipes::step_spline_natural(disp, deg_free = 3)

  expect_snapshot(error = TRUE, {
    mod_1 %>% tune_grid(rec_1, resamples = rs, grid = 3)
  })

  set.seed(987323)
  expect_error(
    suppressMessages(mod_1 %>% tune_grid(rec_2, resamples = rs, grid = 3)),
    regex = NA
  )
})


test_that("skip error if grid is supplied", {
  skip_if_not_installed("randomForest")
  skip_if_not_installed("splines2")


  set.seed(21983)
  rs <- rsample::vfold_cv(mtcars)

  mod_1 <-
    parsnip::rand_forest(mtry = tune(), trees = 20, min_n = tune()) %>%
    parsnip::set_engine("randomForest") %>%
    parsnip::set_mode("regression")

  rec_1 <-
    recipes::recipe(mpg ~ ., data = mtcars) %>%
    recipes::step_spline_natural(disp, deg_free = tune())

  grid <- tibble::tibble(mtry = 1:3, deg_free = c(3, 3, 4), min_n = c(5, 4, 6))

  set.seed(987323)
  expect_error(
    mod_1 %>% tune_grid(rec_1, resamples = rs, grid = grid),
    regex = NA
  )
})


test_that("finalize recipe step with multiple tune parameters", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("splines2")

  data(biomass, package = "modeldata")

  model_spec <- parsnip::linear_reg() %>%
    parsnip::set_engine("lm")

  rec <- recipes::recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur, data = biomass) %>%
    recipes::step_spline_b(carbon, hydrogen, deg_free = tune(), degree = tune())

  best <- tibble(deg_free = 2, degree = 1, .config = "Preprocessor1_Model1")

  expect_s3_class(finalize_recipe(rec, best), "recipe")
  expect_equal(finalize_recipe(rec, best)$steps[[1]]$degree, 1)
  expect_equal(finalize_recipe(rec, best)$steps[[1]]$deg_free, 2)
})
