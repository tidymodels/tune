context("last fit")

source(test_path("../helper-objects.R"))

# ------------------------------------------------------------------------------

set.seed(23598723)
split <- initial_split(mtcars)

f <- mpg ~ cyl + poly(disp, 2) + hp + drat + wt + qsec + vs + am + gear + carb
lm_fit <- lm(f, data = training(split))
test_pred <- predict(lm_fit, testing(split))
rmse_test <- yardstick::rsq_vec(testing(split) %>% pull(mpg), test_pred)

test_that("formula method", {

  res <- linear_reg() %>% set_engine("lm") %>% last_fit(f, split)
  expect_equivalent(coef(extract_model(res$.workflow[[1]])), coef(lm_fit))
  expect_equal(res$.metrics[[1]]$.estimate[[2]], rmse_test)
  expect_equal(res$.predictions[[1]]$.pred, unname(test_pred))
  expect_true(res$.workflow[[1]]$trained)
  expect_equal(nrow(predict(res$.workflow[[1]], testing(split))), nrow(testing(split)))
})

test_that("recipe method", {

  rec <- recipe(mpg ~ ., data = mtcars) %>% step_poly(disp)
  res <- linear_reg() %>% set_engine("lm") %>% last_fit(rec, split)
  expect_equivalent(sort(coef(extract_model(res$.workflow[[1]]))), sort(coef(lm_fit)))
  expect_equal(res$.metrics[[1]]$.estimate[[2]], rmse_test)
  expect_equal(res$.predictions[[1]]$.pred, unname(test_pred))
  expect_true(res$.workflow[[1]]$trained)
  expect_equal(nrow(predict(res$.workflow[[1]], testing(split))), nrow(testing(split)))
})

test_that("collect metrics of last fit", {

  res <- linear_reg() %>% set_engine("lm") %>% last_fit(f, split)
  met <- collect_metrics(res)
  expect_true(inherits(met, "tbl_df"))
  expect_equal(names(met), c(".metric", ".estimator", ".estimate"))
})


test_that("ellipses with last_fit", {

  expect_warning(
    linear_reg() %>% set_engine("lm") %>% last_fit(f, split, something = "wrong"),
    "The `...` are not used in this function but one or more objects"
  )
})

test_that("argument order gives warnings for recipe/formula", {
  rec <- recipe(mpg ~ ., data = mtcars) %>% step_poly(disp)
  lin_mod <- linear_reg() %>%
    set_engine("lm")

  expect_warning(
    last_fit(rec, lin_mod, split),
    "is deprecated as of lifecycle"
  )
  expect_warning(
    last_fit(f, lin_mod, split),
    "is deprecated as of lifecycle"
  )
})
