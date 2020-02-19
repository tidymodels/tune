context("collect")

library(recipes)
data(two_class_dat, package = "modeldata")

# ------------------------------------------------------------------------------

set.seed(6735)
rep_folds <- rsample::vfold_cv(mtcars, v = 2, repeats = 2)

spline_rec <- recipe(mpg ~ ., data = mtcars) %>% step_ns(disp, deg_free = 3)

lin_mod <- parsnip::linear_reg() %>%
  parsnip::set_engine("lm")

lm_splines <-
  fit_resamples(
    spline_rec,
    lin_mod,
    rep_folds,
    control = control_grid(save_pred = TRUE)
  )

set.seed(93114)
rep_folds_class <- rsample::vfold_cv(two_class_dat, v = 2, repeats = 3)

svm_mod <-
  parsnip::svm_rbf(cost = tune("cost value")) %>%
  parsnip::set_engine("kernlab") %>%
  parsnip::set_mode("classification")

svm_tune <-
  tune_bayes(
    Class ~ .,
    svm_mod,
    rep_folds_class,
    initial = 2,
    iter = 2,
    control = control_bayes(save_pred = TRUE)
  )

svm_tune_class <- svm_tune
svm_tune_class$.predictions <-
  purrr::map(svm_tune_class$.predictions,
             ~ .x %>% dplyr::select(-.pred_Class1,-.pred_Class2))
attr(svm_tune_class, "metrics") <- yardstick::metric_set(yardstick::kap)

svm_grd <- show_best(svm_tune, "roc_auc") %>% dplyr::select(`cost value`)

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

 res <- collect_predictions(svm_tune)
 exp_res <-
   unnest(svm_tune %>% dplyr::select(.predictions, starts_with("id"), .iter),
          cols = c(.predictions)) %>% dplyr::select(one_of(names(res)))
 res_subset <- collect_predictions(svm_tune, parameters = svm_grd[1,])
 exp_res_subset <- dplyr::filter(exp_res, `cost value` == svm_grd$`cost value`[[1]])
 expect_equal(res_subset, exp_res_subset)
})

# ------------------------------------------------------------------------------

test_that("bad filter grid", {
  expect_warning(
    expect_error(
      collect_predictions(svm_tune, parameters = tibble(wrong = "value")),
      "`parameters` should only have columns: 'cost value'"
    ),
    "Unknown columns: `cost value`"
  )
  expect_true(
    nrow(collect_predictions(svm_tune, parameters = tibble(`cost value` = 1))) == 0
  )
})

# ------------------------------------------------------------------------------

test_that("regression predictions, averaged", {
  all_res <- collect_predictions(lm_splines)
  res <- collect_predictions(lm_splines, summarize = TRUE)
  expect_equal(nrow(res), nrow(mtcars))

  # pull out an example to test
  all_res_subset <- dplyr::filter(all_res, .row == 3)
  res_subset <- dplyr::filter(res, .row == 3)
  expect_equal(mean(all_res_subset$.pred), res_subset$.pred)
})

# ------------------------------------------------------------------------------

test_that("classification class predictions, averaged", {
  all_res <- collect_predictions(svm_tune_class)
  res <- collect_predictions(svm_tune_class, summarize = TRUE)
  expect_equal(nrow(res), nrow(two_class_dat) * nrow(svm_grd))

  # pull out an example to test
  all_res_subset <-
    dplyr::filter(all_res, .row == 5 & `cost value` == svm_grd$`cost value`[1])
  mode_val <- names(sort(table(all_res_subset$.pred_class)))[2]
  exp_val <- factor(mode_val, levels = levels(all_res_subset$Class))
  res_subset <-
    dplyr::filter(res, .row == 5 &  `cost value` == svm_grd$`cost value`[1])
  expect_equal(exp_val, res_subset$.pred_class)
})

# ------------------------------------------------------------------------------

test_that("classification class and prob predictions, averaged", {
  all_res <- collect_predictions(svm_tune)
  res <- collect_predictions(svm_tune, summarize = TRUE)
  expect_equal(nrow(res), nrow(two_class_dat) * nrow(svm_grd))

  # pull out an example to test
  all_res_subset <-
    dplyr::filter(all_res, .row == 5 & `cost value` == svm_grd$`cost value`[1])
  .pred_Class1 <- mean(all_res_subset$.pred_Class1)
  .pred_Class2 <- 1 - .pred_Class1
  .pred_class <- ifelse(.pred_Class2 > .pred_Class1, "Class2", "Class1")
  .pred_class <- factor(.pred_class, levels = levels(all_res_subset$Class))
  res_subset <-
    dplyr::filter(res, .row == 5 &  `cost value` == svm_grd$`cost value`[1])
  expect_equal(.pred_Class1, res_subset$.pred_Class1)
  expect_equal(.pred_Class2, res_subset$.pred_Class2)
  expect_equal(.pred_class,  res_subset$.pred_class)
})


# ------------------------------------------------------------------------------

test_that("ensure that common dplyr verbs don't affect attributes", {
  res_tune <-
    svm_tune %>%
    dplyr::arrange(id) %>%
    dplyr::sample_frac() %>%
    dplyr::filter(id2 == "Fold1") %>%
    # dplyr::mutate(foo = "bar") %>%
    dplyr::rename(.pred = .predictions) %>%
    dplyr::select(-.pred) %>%
    dplyr::slice(1:5)

  expect_true(inherits(attr(res_tune, "metrics"), "metric_set"))
  expect_true(inherits(attr(res_tune, "parameters"), "parameters"))

  res_tune <- res_tune[-1,]
  expect_true(inherits(attr(res_tune, "metrics"), "metric_set"))
  expect_true(inherits(attr(res_tune, "parameters"), "parameters"))

  res_resamples <-
    lm_splines %>%
    dplyr::arrange(id) %>%
    dplyr::sample_frac() %>%
    dplyr::filter(id2 == "Fold1") %>%
    # dplyr::mutate(foo = "bar") %>%
    dplyr::rename(.pred = .predictions) %>%
    dplyr::select(-.pred) %>%
    dplyr::slice(1:5)

  expect_true(inherits(attr(res_resamples, "metrics"), "metric_set"))
  expect_true(inherits(attr(res_resamples, "parameters"), "parameters"))

  res_resamples <- res_resamples[-1,]
  expect_true(inherits(attr(res_resamples, "metrics"), "metric_set"))
  expect_true(inherits(attr(res_resamples, "parameters"), "parameters"))

})

