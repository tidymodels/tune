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
  expect_no_error(
    suppressMessages(mod_1 %>% tune_grid(rec_2, resamples = rs, grid = 3))
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
  expect_no_error(
    mod_1 %>% tune_grid(rec_1, resamples = rs, grid = grid)
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

# ------------------------------------------------------------------------------
# post-processing

test_that("finalize tailors", {
  skip_if_not_installed("probably")
  skip_if_not_installed("dials", "1.3.0.9000")
  library(tailor)

  adjust_rng <-
    tailor() %>%
    adjust_numeric_range(lower_limit = tune(), upper_limit = tune())

  adj_1 <- finalize_tailor(adjust_rng, tibble(lower_limit = 2))
  expect_equal(adj_1$adjustments[[1]]$arguments$lower_limit, 2)
  expect_equal(adj_1$adjustments[[1]]$arguments$upper_limit, tune())

  adj_2 <- finalize_tailor(adjust_rng, tibble(lower_limit = 2, upper_limit = 3))
  expect_equal(adj_2$adjustments[[1]]$arguments$lower_limit, 2)
  expect_equal(adj_2$adjustments[[1]]$arguments$upper_limit, 3)

  adj_3 <- finalize_tailor(adjust_rng, tibble(lower_limit = 2, upper_limit = 3, a = 2))
  expect_equal(adj_3$adjustments[[1]]$arguments$lower_limit, 2)
  expect_equal(adj_3$adjustments[[1]]$arguments$upper_limit, 3)

  adj_4 <- finalize_tailor(adjust_rng, tibble())
  expect_equal(adj_4, adjust_rng)

  expect_snapshot(
    finalize_tailor(linear_reg(), tibble()),
    error = TRUE
  )
})

test_that("finalize workflows with tailors", {
  skip_if_not_installed("probably")
  skip_if_not_installed("dials", "1.3.0.9000")
  library(tailor)
  library(purrr)

  adjust_rng <-
    tailor() %>%
    adjust_numeric_range(lower_limit = tune(), upper_limit = tune())
  wflow <- workflow(y ~ ., linear_reg(), adjust_rng)

  wflow_1 <- finalize_workflow(wflow, tibble(lower_limit = 2))
  expect_equal(
    wflow_1 %>%
      extract_postprocessor() %>%
      pluck("adjustments") %>%
      pluck(1) %>%
      pluck("arguments") %>%
      pluck("lower_limit"),
    2
  )
  expect_equal(
    wflow_1 %>%
      extract_postprocessor() %>%
      pluck("adjustments") %>%
      pluck(1) %>%
      pluck("arguments") %>%
      pluck("upper_limit"),
    tune()
  )

  wflow_2 <- finalize_workflow(wflow, tibble(lower_limit = 2, upper_limit = 3))
  expect_equal(
    wflow_2 %>%
      extract_postprocessor() %>%
      pluck("adjustments") %>%
      pluck(1) %>%
      pluck("arguments") %>%
      pluck("lower_limit"),
    2
  )
  expect_equal(
    wflow_2 %>%
      extract_postprocessor() %>%
      pluck("adjustments") %>%
      pluck(1) %>%
      pluck("arguments") %>%
      pluck("upper_limit"),
    3
  )

  wflow_3 <- finalize_workflow(wflow, tibble(lower_limit = 2, upper_limit = 3, a = 2))
  expect_equal(
    wflow_3 %>%
      extract_postprocessor() %>%
      pluck("adjustments") %>%
      pluck(1) %>%
      pluck("arguments") %>%
      pluck("lower_limit"),
    2
  )
  expect_equal(
    wflow_3 %>%
      extract_postprocessor() %>%
      pluck("adjustments") %>%
      pluck(1) %>%
      pluck("arguments") %>%
      pluck("upper_limit"),
    3
  )

  wflow_4 <- finalize_workflow(wflow, tibble())
  expect_equal(wflow_4, wflow)

  # https://github.com/tidymodels/tune/issues/998
  cls_post <- tailor() %>%
    adjust_probability_threshold(threshold = tune("cut"))
  wflow_thrsh <- workflow(y ~ ., logistic_reg(), cls_post)
  thrsh_param <- tibble(min_n = 2, cut = 1/3)
  wflow_thrsh <- wflow_thrsh %>% finalize_workflow(thrsh_param)

  expect_equal(
    wflow_thrsh %>%
      extract_postprocessor() %>%
      pluck("adjustments") %>%
      pluck(1) %>%
      pluck("arguments") %>%
      pluck("threshold"),
    1 / 3
  )

})
