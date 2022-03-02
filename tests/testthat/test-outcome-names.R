library(recipes)
library(parsnip)

rec_1 <- recipe(mpg ~ ., data = mtcars)
rec_2 <- recipe(mpg + wt ~ ., data = mtcars)
rec_3 <- recipe( ~ ., data = mtcars)
rec_4 <- recipe(mpg ~ ., data = mtcars) %>% step_rm(mpg)
lm_mod <- linear_reg() %>% set_engine("lm")
wflow <- workflow() %>% add_model(lm_mod)

## -----------------------------------------------------------------------------

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
  expect_equal(outcome_names(rec_1), "mpg")
  expect_equal(outcome_names(prep(rec_1)), "mpg")

  expect_equal(outcome_names(rec_2), c("mpg", "wt"))
  expect_equal(outcome_names(prep(rec_2)), c("mpg", "wt"))

  expect_equal(outcome_names(rec_3), character(0))
  expect_equal(outcome_names(prep(rec_3)), character(0))

  expect_equal(outcome_names(rec_4), "mpg")
  expect_equal(outcome_names(prep(rec_4)), character(0))
})


## -----------------------------------------------------------------------------

test_that("workflows + recipes", {
  wflow_1 <- wflow %>% add_recipe(rec_1)
  expect_equal(outcome_names(wflow_1), "mpg")
  expect_equal(outcome_names(fit(wflow_1, mtcars)), "mpg")

  wflow_2 <- wflow %>% add_recipe(rec_2)
  expect_equal(outcome_names(wflow_2), c("mpg", "wt"))
  expect_equal(outcome_names(fit(wflow_2, mtcars)), c("mpg", "wt"))
})


## -----------------------------------------------------------------------------

test_that("workflows + formulas", {
  wflow_1 <- wflow %>% add_formula(mpg ~ .)
  expect_equal(outcome_names(wflow_1), "mpg")
  expect_equal(outcome_names(fit(wflow_1, mtcars)), "mpg")

  wflow_2 <- wflow %>% add_formula(mpg + wt ~ .)
  expect_equal(outcome_names(wflow_2), c("mpg", "wt"))
  expect_equal(outcome_names(fit(wflow_2, mtcars)), c("mpg", "wt"))
})


## -----------------------------------------------------------------------------

test_that("tune_results objects", {
  set.seed(6735)
  folds <- rsample::vfold_cv(mtcars, v = 2)

  lin_mod <- linear_reg() %>%
    set_engine("lm")

  res <- lin_mod %>%
    fit_resamples(mpg ~ ., folds) %>%
    outcome_names()
  expect_equal(res, "mpg")

})

