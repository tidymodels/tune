context("collect")

library(recipes)

# ------------------------------------------------------------------------------

set.seed(6735)
rep_folds <- rsample::vfold_cv(mtcars, v = 2, repeats = 2)

spline_rec <- recipe(mpg ~ ., data = mtcars) %>% step_ns(disp, deg_free = tune())

lin_mod <- parsnip::linear_reg() %>%
  parsnip::set_engine("lm")

grd <- tibble::tibble(deg_free = 3:5)

lm_splines <-
  tune_grid(
    spline_rec,
    lin_mod,
    rep_folds,
    grid = grd,
    control = control_grid(save_pred = TRUE)
  )

# ------------------------------------------------------------------------------

test_that("`collect_predictions()` errors informatively if there is no `.predictions` column", {

  expect_error(
    collect_predictions(lm_splines %>% dplyr::select(-.predictions)),
    "The `.predictions` column does not exist."
  )
})

# ------------------------------------------------------------------------------

test_that("`collect_predictions()`, un-averaged", {
  res <- collect_predictions(lm_splines)
  exp_res <-
    unnest(lm_splines %>% dplyr::select(.predictions, starts_with("id")),
           cols = c(.predictions)) %>% dplyr::select(one_of(names(res)))
 expect_equal(res, exp_res)

 res_subset <- collect_predictions(lm_splines, grd[1,])
 exp_res_subset <- dplyr::filter(exp_res, deg_free == grd$deg_free[[1]])
 expect_equal(res_subset, exp_res_subset)
})

# ------------------------------------------------------------------------------

test_that("bad filter grid", {
  expect_warning(
    expect_error(
      collect_predictions(lm_splines, tibble(wrong = "value")),
      "`parameters` should only have columns: 'deg_free'"
    ),
    "Unknown columns: `deg_free`"
  )
  expect_true(nrow(collect_predictions(lm_splines, tibble(deg_free = 1))) == 0)
})

