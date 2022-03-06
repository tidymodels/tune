test_that("base R objects", {
  lm_1 <- lm(mpg ~ ., data = mtcars)
  expect_equal(outcome_names(lm_1$terms), "mpg")

  lm_2 <- lm(cbind(mpg, wt) ~ ., data = mtcars)
  expect_equal(outcome_names(lm_2$terms), c("mpg", "wt"))

  f_1 <- mpg ~ .
  expect_equal(outcome_names(f_1), "mpg")

  f_2 <- cbind(mpg, wt) ~ .
  expect_equal(outcome_names(f_2), c("mpg", "wt"))

  f_3 <-  ~ .
  expect_equal(outcome_names(f_3), character(0))
})

## -----------------------------------------------------------------------------

test_that("recipes", {
  rec_1 <- recipes::recipe(mpg ~ ., data = mtcars)
  expect_equal(outcome_names(rec_1), "mpg")
  expect_equal(outcome_names(recipes::prep(rec_1)), "mpg")

  rec_2 <- recipes::recipe(mpg + wt ~ ., data = mtcars)
  expect_equal(outcome_names(rec_2), c("mpg", "wt"))
  expect_equal(outcome_names(recipes::prep(rec_2)), c("mpg", "wt"))

  rec_3 <- recipes::recipe( ~ ., data = mtcars)
  expect_equal(outcome_names(rec_3), character(0))
  expect_equal(outcome_names(recipes::prep(rec_3)), character(0))

  rec_4 <- recipes::recipe(mpg ~ ., data = mtcars) %>% recipes::step_rm(mpg)
  expect_equal(outcome_names(rec_4), "mpg")
  expect_equal(outcome_names(recipes::prep(rec_4)), character(0))
})


## -----------------------------------------------------------------------------

test_that("workflows + recipes", {
  rec_1 <- recipes::recipe(mpg ~ ., data = mtcars)
  rec_2 <- recipes::recipe(mpg + wt ~ ., data = mtcars)
  lm_mod <- parsnip::linear_reg() %>% parsnip::set_engine("lm")
  wflow <- workflow() %>% add_model(lm_mod)

  wflow_1 <- wflow %>% add_recipe(rec_1)
  expect_equal(outcome_names(wflow_1), "mpg")
  expect_equal(outcome_names(parsnip::fit(wflow_1, mtcars)), "mpg")

  wflow_2 <- wflow %>% add_recipe(rec_2)
  expect_equal(outcome_names(wflow_2), c("mpg", "wt"))
  expect_equal(outcome_names(parsnip::fit(wflow_2, mtcars)), c("mpg", "wt"))
})


## -----------------------------------------------------------------------------

test_that("workflows + formulas", {
  lm_mod <- parsnip::linear_reg() %>% parsnip::set_engine("lm")
  wflow <- workflow() %>% add_model(lm_mod)

  wflow_1 <- wflow %>% add_formula(mpg ~ .)
  expect_equal(outcome_names(wflow_1), "mpg")
  expect_equal(outcome_names(parsnip::fit(wflow_1, mtcars)), "mpg")

  wflow_2 <- wflow %>% add_formula(mpg + wt ~ .)
  expect_equal(outcome_names(wflow_2), c("mpg", "wt"))
  expect_equal(outcome_names(parsnip::fit(wflow_2, mtcars)), c("mpg", "wt"))
})


## -----------------------------------------------------------------------------

test_that("tune_results objects", {
  set.seed(6735)
  folds <- rsample::vfold_cv(mtcars, v = 2)

  lin_mod <- parsnip::linear_reg() %>%
    parsnip::set_engine("lm")

  res <- lin_mod %>%
    fit_resamples(mpg ~ ., folds) %>%
    outcome_names()
  expect_equal(res, "mpg")
})
