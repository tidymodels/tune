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
  set.seed(23598723)
  split <- rsample::initial_split(mtcars)
  f <- mpg ~ cyl + poly(disp, 2) + hp + drat + wt + qsec + vs + am + gear + carb

  expect_snapshot(
    linear_reg() %>% set_engine("lm") %>% last_fit(f, split, something = "wrong")
  )
})

test_that("argument order gives errors for recipe/formula", {
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
