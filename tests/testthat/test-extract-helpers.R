test_that("extract methods for last_fit objects", {
  lm_spec <- parsnip::linear_reg() %>% parsnip::set_engine("lm")
  lm_prn_fit <- parsnip::fit(lm_spec, mpg ~ ., data = mtcars)
  lm_wflow <-
    workflow() %>%
    add_model(lm_spec) %>%
    add_formula(mpg ~ .)
  lm_res <- last_fit(lm_wflow, split = rsample::initial_split(mtcars))

  expect_true(inherits(extract_fit_engine(lm_res), "lm"))
  expect_true(inherits(extract_fit_parsnip(lm_res), "model_fit"))
  expect_true(inherits(extract_mold(lm_res), "list"))
  expect_true(inherits(extract_preprocessor(lm_res), "formula"))
  expect_true(inherits(extract_spec_parsnip(lm_res), "model_spec"))
  expect_true(inherits(extract_workflow(lm_res), "workflow"))
})

test_that("extract methods for resample_results objects", {
  lm_spec <- parsnip::linear_reg() %>% parsnip::set_engine("lm")
  lm_rec_wflow <-
    workflow() %>%
    add_model(lm_spec) %>%
    add_recipe(recipes::recipe(mpg ~ ., data = mtcars) %>%
      recipes::step_normalize(recipes::all_numeric_predictors()))
  lm_rec_res <- fit_resamples(
    lm_rec_wflow,
    resamples = rsample::vfold_cv(mtcars, v = 2),
    control = control_resamples(save_workflow = TRUE)
  )

  expect_true(inherits(extract_recipe(lm_rec_res, estimated = FALSE), "recipe"))
  expect_true(!extract_recipe(lm_rec_res, estimated = FALSE)$steps[[1]]$trained)
  expect_true(inherits(extract_workflow(lm_rec_res), "workflow"))
})
