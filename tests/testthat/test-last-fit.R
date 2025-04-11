test_that("formula method", {
  set.seed(23598723)
  split <- rsample::initial_split(mtcars)

  f <- mpg ~ cyl + poly(disp, 2) + hp + drat + wt + qsec + vs + am + gear + carb
  lm_fit <- lm(f, data = rsample::training(split))
  test_pred <- predict(lm_fit, rsample::testing(split))
  rmse_test <- yardstick::rsq_vec(rsample::testing(split) %>% pull(mpg), test_pred)

  res <- parsnip::linear_reg() %>%
    parsnip::set_engine("lm") %>%
    last_fit(f, split)

  expect_equal(res, .Last.tune.result)

  expect_equal(
    coef(extract_fit_engine(res$.workflow[[1]])),
    coef(lm_fit),
    ignore_attr = TRUE
  )
  expect_equal(res$.metrics[[1]]$.estimate[[2]], rmse_test)
  expect_equal(res$.predictions[[1]]$.pred, unname(test_pred))
  expect_true(res$.workflow[[1]]$trained)
  expect_equal(
    nrow(predict(res$.workflow[[1]], rsample::testing(split))),
    nrow(rsample::testing(split))
  )
  expect_null(.get_tune_eval_times(res))
  expect_null(.get_tune_eval_time_target(res))

})

test_that("recipe method", {
  set.seed(23598723)
  split <- rsample::initial_split(mtcars)

  f <- mpg ~ cyl + poly(disp, 2) + hp + drat + wt + qsec + vs + am + gear + carb
  lm_fit <- lm(f, data = rsample::training(split))
  test_pred <- predict(lm_fit, rsample::testing(split))
  rmse_test <- yardstick::rsq_vec(rsample::testing(split) %>% pull(mpg), test_pred)

  rec <- recipes::recipe(mpg ~ ., data = mtcars) %>%
    recipes::step_poly(disp)
  res <- parsnip::linear_reg() %>%
    parsnip::set_engine("lm") %>%
    last_fit(rec, split)

  expect_equal(
    sort(coef(extract_fit_engine(res$.workflow[[1]]))),
    sort(coef(lm_fit)),
    ignore_attr = TRUE
  )
  expect_equal(res$.metrics[[1]]$.estimate[[2]], rmse_test)
  expect_equal(res$.predictions[[1]]$.pred, unname(test_pred))
  expect_true(res$.workflow[[1]]$trained)
  expect_equal(
    nrow(predict(res$.workflow[[1]], rsample::testing(split))),
    nrow(rsample::testing(split))
  )
})

test_that("model_fit method", {
  library(parsnip)

  lm_fit <- linear_reg() %>% fit(mpg ~ ., data = mtcars)

  expect_snapshot(last_fit(lm_fit), error = TRUE)
})

test_that("workflow method", {
  library(parsnip)

  lm_fit <- workflows::workflow(mpg ~ ., linear_reg()) %>% fit(data = mtcars)

  expect_snapshot(last_fit(lm_fit), error = TRUE)
})

test_that("collect metrics of last fit", {
  set.seed(23598723)
  split <- rsample::initial_split(mtcars)
  f <- mpg ~ cyl + poly(disp, 2) + hp + drat + wt + qsec + vs + am + gear + carb
  res <- parsnip::linear_reg() %>%
    parsnip::set_engine("lm") %>%
    last_fit(f, split)
  met <- collect_metrics(res)
  expect_true(inherits(met, "tbl_df"))
  expect_equal(names(met), c(".metric", ".estimator", ".estimate", ".config"))
})


test_that("ellipses with last_fit", {
  options(width = 200, pillar.advice = FALSE, pillar.min_title_chars = Inf)

  set.seed(23598723)
  split <- rsample::initial_split(mtcars)
  f <- mpg ~ cyl + poly(disp, 2) + hp + drat + wt + qsec + vs + am + gear + carb

  expect_snapshot(
    linear_reg() %>% set_engine("lm") %>% last_fit(f, split, something = "wrong")
  )
})

test_that("argument order gives errors for recipe/formula", {
  options(width = 200, pillar.advice = FALSE, pillar.min_title_chars = Inf)

  set.seed(23598723)
  split <- rsample::initial_split(mtcars)

  f <- mpg ~ cyl + poly(disp, 2) + hp + drat + wt + qsec + vs + am + gear + carb

  rec <- recipes::recipe(mpg ~ ., data = mtcars) %>% recipes::step_poly(disp)
  lin_mod <- parsnip::linear_reg() %>%
    parsnip::set_engine("lm")

  expect_snapshot(error = TRUE, {
    last_fit(rec, lin_mod, split)
  })
  expect_snapshot(error = TRUE, {
    last_fit(f, lin_mod, split)
  })
})

test_that("same results of last_fit() and fit() (#300)", {
  skip_if_not_installed("randomForest")

  rf <- parsnip::rand_forest(mtry = 2, trees = 5) %>%
    parsnip::set_engine("randomForest") %>%
    parsnip::set_mode("regression")

  wflow <- workflows::workflow() %>%
    workflows::add_model(rf) %>%
    workflows::add_formula(mpg ~ .)

  set.seed(23598723)
  split <- rsample::initial_split(mtcars)

  set.seed(1)
  lf_obj <- last_fit(wflow, split = split)

  set.seed(1)
  r_obj <- fit(wflow, data = rsample::analysis(split))
  r_pred <- predict(r_obj, rsample::assessment(split))

  expect_equal(
    lf_obj$.predictions[[1]]$.pred,
    r_pred$.pred
  )
})


test_that("`last_fit()` when objects need tuning", {
  skip_if_not_installed("splines2")

  options(width = 200, pillar.advice = FALSE, pillar.min_title_chars = Inf)

  rec <- recipe(mpg ~ ., data = mtcars) %>% step_spline_natural(disp, deg_free = tune())
  spec_1 <- linear_reg(penalty = tune()) %>% set_engine("glmnet")
  spec_2 <- linear_reg()
  wflow_1 <- workflow(rec, spec_1)
  wflow_2 <- workflow(mpg ~ ., spec_1)
  wflow_3 <- workflow(rec, spec_2)
  split <- rsample::initial_split(mtcars)

  expect_snapshot_error(last_fit(wflow_1, split))
  expect_snapshot_error(last_fit(wflow_2, split))
  expect_snapshot_error(last_fit(wflow_3, split))
})

test_that("last_fit() excludes validation set for initial_validation_split objects", {
  skip_if_not_installed("modeldata")
  data(ames, package = "modeldata", envir = rlang::current_env())

  set.seed(23598723)
  split <- rsample::initial_validation_split(ames)

  f <- Sale_Price ~ Gr_Liv_Area + Year_Built
  lm_fit <- lm(f, data = rsample::training(split))
  test_pred <- predict(lm_fit, rsample::testing(split))
  rmse_test <- yardstick::rsq_vec(rsample::testing(split) %>% pull(Sale_Price), test_pred)

  res <- parsnip::linear_reg() %>%
    parsnip::set_engine("lm") %>%
    last_fit(f, split)

  expect_equal(res, .Last.tune.result)

  expect_equal(
    coef(extract_fit_engine(res$.workflow[[1]])),
    coef(lm_fit),
    ignore_attr = TRUE
  )
  expect_equal(res$.metrics[[1]]$.estimate[[2]], rmse_test)
  expect_equal(res$.predictions[[1]]$.pred, unname(test_pred))
  expect_true(res$.workflow[[1]]$trained)
  expect_equal(
    nrow(predict(res$.workflow[[1]], rsample::testing(split))),
    nrow(rsample::testing(split))
  )
})

test_that("last_fit() can include validation set for initial_validation_split objects", {
  skip_if_not_installed("modeldata")
  data(ames, package = "modeldata", envir = rlang::current_env())

  set.seed(23598723)
  split <- rsample::initial_validation_split(ames)

  f <- Sale_Price ~ Gr_Liv_Area + Year_Built
  train_val <- rbind(rsample::training(split), rsample::validation(split))
  lm_fit <- lm(f, data = train_val)
  test_pred <- predict(lm_fit, rsample::testing(split))
  rmse_test <- yardstick::rsq_vec(rsample::testing(split) %>% pull(Sale_Price), test_pred)

  res <- parsnip::linear_reg() %>%
    parsnip::set_engine("lm") %>%
    last_fit(f, split, add_validation_set = TRUE)

  expect_equal(res, .Last.tune.result)

  expect_equal(
    coef(extract_fit_engine(res$.workflow[[1]])),
    coef(lm_fit),
    ignore_attr = TRUE
  )
  expect_equal(res$.metrics[[1]]$.estimate[[2]], rmse_test)
  expect_equal(res$.predictions[[1]]$.pred, unname(test_pred))
  expect_true(res$.workflow[[1]]$trained)
  expect_equal(
    nrow(predict(res$.workflow[[1]], rsample::testing(split))),
    nrow(rsample::testing(split))
  )
})

test_that("can use `last_fit()` with a workflow - postprocessor (requires training)", {
  skip_if_not_installed("mgcv")
  skip_if_not_installed("tailor", minimum_version = "0.0.0.9002")
  skip_if_not_installed("probably")

  y <- seq(0, 7, .001)
  dat <- data.frame(y = y, x = y + (y-3)^2)

  dat

  set.seed(1)
  split <- rsample::initial_split(dat)

  wflow <-
    workflows::workflow(
      y ~ x,
      parsnip::linear_reg()
    ) %>%
    workflows::add_tailor(
      tailor::tailor() %>% tailor::adjust_numeric_calibration("linear")
    )

  set.seed(1)
  last_fit_res <-
    last_fit(
      wflow,
      split
    )

  last_fit_preds <- collect_predictions(last_fit_res)

  set.seed(1)
  inner_split <- rsample::inner_split(split, split_args = list())

  set.seed(1)
  wflow_res <-
    generics::fit(
      wflow,
      rsample::analysis(inner_split),
      calibration = rsample::assessment(inner_split)
    )
  wflow_preds <- predict(wflow_res, rsample::assessment(split))

  expect_equal(last_fit_preds[".pred"], wflow_preds)
})

test_that("can use `last_fit()` with a workflow - postprocessor (does not require training)", {
  skip_if_not_installed("tailor", minimum_version = "0.0.0.9002")
  skip_if_not_installed("probably")

  y <- seq(0, 7, .001)
  dat <- data.frame(y = y, x = y + (y-3)^2)

  dat

  set.seed(1)
  split <- rsample::initial_split(dat)

  wflow <-
    workflows::workflow(
      y ~ x,
      parsnip::linear_reg()
    ) %>%
    workflows::add_tailor(
      tailor::tailor() %>% tailor::adjust_numeric_range(lower_limit = 1)
    )

  set.seed(1)
  last_fit_res <-
    last_fit(
      wflow,
      split
    )

  last_fit_preds <- collect_predictions(last_fit_res)

  set.seed(1)
  wflow_res <- generics::fit(wflow, rsample::analysis(split))
  wflow_preds <- predict(wflow_res, rsample::assessment(split))

  expect_equal(last_fit_preds[".pred"], wflow_preds)
})
