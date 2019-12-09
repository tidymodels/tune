context("last fit")

library(parsnip)
library(rsample)

set.seed(23598723)
split <- initial_split(mtcars)

f <- mpg ~ cyl + poly(disp, 2) + hp + drat + wt + qsec + vs + am + gear + carb
lm_fit <- lm(f, data = training(split))
test_pred <- predict(lm_fit, testing(split))
rmse_test <- yardstick::rsq_vec(testing(split) %>% pull(mpg), test_pred)

test_that("formula method", {
  res <- last_fit(f, linear_reg() %>% set_engine("lm"), split)
  expect_equivalent(coef(extract_model(res$.workflow[[1]])), coef(lm_fit))
  expect_equal(res$.metrics[[1]]$.estimate[[2]], rmse_test)
  expect_equal(res$.predictions[[1]]$.pred, unname(test_pred))
})

test_that("recipe method", {
  rec <- recipe(mpg ~ ., data = mtcars) %>% step_poly(disp)
  res <- last_fit(rec, linear_reg() %>% set_engine("lm"), split)
  expect_equivalent(sort(coef(extract_model(res$.workflow[[1]]))), sort(coef(lm_fit)))
  expect_equal(res$.metrics[[1]]$.estimate[[2]], rmse_test)
  expect_equal(res$.predictions[[1]]$.pred, unname(test_pred))
})

test_that("split_to_rset", {
  res <- tune:::split_to_rset(split)
  expect_true(inherits(res, "mc_cv"))
  expect_true(nrow(res) == 1)
  expect_true(nrow(res) == 1)

  res <- last_fit(f, linear_reg() %>% set_engine("lm"), split)
  expect_true(is.list(res$.workflow))
  expect_true(inherits(res$.workflow[[1]], "workflow"))
  expect_true(is.list(res$.predictions))
  expect_true(inherits(res$.predictions[[1]], "tbl_df"))
})

test_that("collect metrics of last fit", {
  res <- last_fit(f, linear_reg() %>% set_engine("lm"), split)
  met <- collect_metrics(res)
  expect_true(inherits(met, "tbl_df"))
  expect_equal(names(met), c(".metric", ".estimator", ".estimate"))
})
