if (rlang::is_installed(c("modeldata", "splines2", "kernlab"))) {

  data(two_class_dat, package = "modeldata")

  # ------------------------------------------------------------------------------

  set.seed(6735)
  rep_folds <- rsample::vfold_cv(mtcars, v = 2, repeats = 2)

  spline_rec <- recipes::recipe(mpg ~ ., data = mtcars) %>%
    recipes::step_spline_natural(disp, deg_free = 3)

  lin_mod <- parsnip::linear_reg() %>%
    parsnip::set_engine("lm")

  lm_splines <-
    fit_resamples(
      lin_mod,
      spline_rec,
      rep_folds,
      control = control_grid(save_pred = TRUE)
    )

  set.seed(93114)
  rep_folds_class <- rsample::vfold_cv(two_class_dat, v = 2, repeats = 3)

  svm_mod <-
    parsnip::svm_rbf(cost = tune("cost value")) %>%
    parsnip::set_engine("kernlab") %>%
    parsnip::set_mode("classification")

  suppressMessages(
    svm_tune <-
      tune_bayes(
        svm_mod,
        Class ~ .,
        rep_folds_class,
        initial = 2,
        iter = 2,
        control = control_bayes(save_pred = TRUE)
      )
  )

  svm_tune_class <- svm_tune
  svm_tune_class$.predictions <-
    purrr::map(
      svm_tune_class$.predictions,
      ~ .x %>% dplyr::select(-.pred_Class1, -.pred_Class2)
    )
  attr(svm_tune_class, "metrics") <- yardstick::metric_set(yardstick::kap)

  svm_grd <- show_best(svm_tune, metric = "roc_auc") %>% dplyr::select(`cost value`)
}

# ------------------------------------------------------------------------------

test_that("`collect_predictions()` errors informatively if there is no `.predictions` column", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("splines2")

  expect_snapshot(error = TRUE, {
    collect_predictions(lm_splines %>% dplyr::select(-.predictions))
  })
})

test_that("`collect_predictions()` errors informatively applied to unsupported class", {
  expect_snapshot(
    error = TRUE,
    collect_predictions(lm(mpg ~ disp, mtcars))
  )
})

# ------------------------------------------------------------------------------

test_that("`collect_predictions()`, un-averaged", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("splines2")

  res <- collect_predictions(lm_splines)
  exp_res <-
    unnest(lm_splines %>% dplyr::select(.predictions, starts_with("id")),
           cols = c(.predictions)
    ) %>% dplyr::select(all_of(names(res)))
  expect_equal(res, exp_res)

  res <- collect_predictions(svm_tune)
  exp_res <-
    unnest(
      svm_tune %>% dplyr::select(.predictions, starts_with("id"), .iter),
      cols = c(.predictions)
    ) %>%
    dplyr::select(all_of(names(res)))
  res_subset <- collect_predictions(svm_tune, parameters = svm_grd[1, ])
  exp_res_subset <- dplyr::filter(exp_res, `cost value` == svm_grd$`cost value`[[1]])
  expect_equal(res_subset, exp_res_subset)
})

# ------------------------------------------------------------------------------

test_that("bad filter grid", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("kernlab")

  expect_snapshot(
    error = TRUE,
    collect_predictions(svm_tune, parameters = tibble(wrong = "value"))
  )
  expect_true(
    nrow(collect_predictions(svm_tune, parameters = tibble(`cost value` = 1))) == 0
  )
})

# ------------------------------------------------------------------------------

test_that("regression predictions, averaged", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("splines2")

  all_res <- collect_predictions(lm_splines)
  res <- collect_predictions(lm_splines, summarize = TRUE)
  expect_equal(nrow(res), nrow(mtcars))
  expect_false(dplyr::is_grouped_df(res))

  # pull out an example to test
  all_res_subset <- dplyr::filter(all_res, .row == 3)
  res_subset <- dplyr::filter(res, .row == 3)
  expect_equal(mean(all_res_subset$.pred), res_subset$.pred)
})

# ------------------------------------------------------------------------------

test_that("classification class predictions, averaged", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("kernlab")

  all_res <- collect_predictions(svm_tune_class)
  res <- collect_predictions(svm_tune_class, summarize = TRUE)
  expect_equal(nrow(res), nrow(two_class_dat) * nrow(svm_grd))
  expect_false(dplyr::is_grouped_df(res))
  expect_named(
    collect_predictions(svm_tune, summarize = TRUE),
    c(".pred_class", ".pred_Class1", ".pred_Class2", ".row", "cost value",
      "Class", ".config", ".iter")
  )

  # pull out an example to test
  all_res_subset <-
    dplyr::filter(all_res, .row == 5 & `cost value` == svm_grd$`cost value`[1])
  mode_val <- names(sort(table(all_res_subset$.pred_class)))[2]
  exp_val <- factor(mode_val, levels = levels(all_res_subset$Class))
  res_subset <-
    dplyr::filter(res, .row == 5 & `cost value` == svm_grd$`cost value`[1])
  expect_equal(exp_val, res_subset$.pred_class)
})

# ------------------------------------------------------------------------------

test_that("classification class and prob predictions, averaged", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("kernlab")

  all_res <- collect_predictions(svm_tune)
  res <- collect_predictions(svm_tune, summarize = TRUE)
  expect_equal(nrow(res), nrow(two_class_dat) * nrow(svm_grd))
  expect_false(dplyr::is_grouped_df(res))

  # pull out an example to test
  all_res_subset <-
    dplyr::filter(all_res, .row == 5 & `cost value` == svm_grd$`cost value`[1])
  .pred_Class1 <- mean(all_res_subset$.pred_Class1)
  .pred_Class2 <- 1 - .pred_Class1
  .pred_class <- ifelse(.pred_Class2 > .pred_Class1, "Class2", "Class1")
  .pred_class <- factor(.pred_class, levels = levels(all_res_subset$Class))
  res_subset <-
    dplyr::filter(res, .row == 5 & `cost value` == svm_grd$`cost value`[1])
  expect_equal(.pred_Class1, res_subset$.pred_Class1)
  expect_equal(.pred_Class2, res_subset$.pred_Class2)
  expect_equal(.pred_class, res_subset$.pred_class)
})

# ------------------------------------------------------------------------------

test_that("collecting notes - fit_resamples", {
  skip_if(new_rng_snapshots)
  skip_if(!rankdeficient_version)
  skip_if_not_installed("modeldata")
  skip_if_not_installed("splines2")

  mtcars2 <- mtcars %>% mutate(wt2 = wt)
  set.seed(1)
  flds <- rsample::bootstraps(mtcars2, times = 2)

  lin_mod <- parsnip::linear_reg() %>%
    parsnip::set_engine("lm")

  expect_snapshot(
    lm_splines <- fit_resamples(lin_mod, mpg ~ ., flds)
  )
  expect_snapshot(lm_splines)

  nts <- collect_notes(lm_splines)
  expect_true(all(nts$type == "warning"))
  expect_true(all(grepl("rank", nts$note)))
  expect_equal(names(nts), c("id", "location", "type", "note", "trace"))
})

test_that("collecting notes - last_fit", {
  skip_if(!rankdeficient_version)

  options(pillar.advice = FALSE, pillar.min_title_chars = Inf)

  mtcars2 <- mtcars %>% mutate(wt2 = wt)
  set.seed(1)
  split <- rsample::initial_split(mtcars2)

  lin_mod <- parsnip::linear_reg() %>%
    parsnip::set_engine("lm")

  expect_snapshot(
    lst <- last_fit(lin_mod, mpg ~ ., split)
  )
  expect_snapshot(lst)

  nts <- collect_notes(lst)
  expect_true(all(nts$type == "warning"))
  expect_true(all(grepl("rank", nts$note)))
  expect_equal(names(nts), c("location", "type", "note", "trace"))
})

test_that("`collect_notes()` errors informatively applied to unsupported class", {
  expect_snapshot(
    error = TRUE,
    collect_notes(lm(mpg ~ disp, mtcars))
  )
})

test_that("collecting extracted objects - fit_resamples", {
  # skip pre-R-4.0.0 so that snaps aren't affected by stringsAsFactors change
  skip_if(R.Version()$major < "4")

  spec <- parsnip::linear_reg()
  form <- mpg ~ .
  set.seed(1)
  boots <- rsample::bootstraps(mtcars, 5)

  ctrl_fit <- control_resamples(extract = extract_fit_engine)
  ctrl_err <- control_resamples(extract = function(x) {stop("eeeep! eep!")})

  res_fit <-     fit_resamples(spec, form, boots, control = ctrl_fit)
  res_nothing <- fit_resamples(spec, form, boots)
  suppressMessages({
    res_error <-   fit_resamples(spec, form, boots, control = ctrl_err)
  })

  expect_snapshot(collect_extracts(res_fit))
  expect_snapshot(collect_extracts(res_nothing), error = TRUE)
  expect_snapshot(collect_extracts(res_error))
})

test_that("`collect_extracts()` errors informatively applied to unsupported class", {
  expect_snapshot(
    error = TRUE,
    collect_extracts(lm(mpg ~ disp, mtcars))
  )
})

test_that("`collect_metrics()` errors informatively applied to unsupported class", {
  expect_snapshot(
    error = TRUE,
    collect_metrics(lm(mpg ~ disp, mtcars))
  )
})

test_that("`collect_metrics(type)` errors informatively with bad input", {
  skip_on_cran()

  expect_snapshot(
    error = TRUE,
    collect_metrics(ames_grid_search, type = "boop")
  )

  expect_snapshot(
    error = TRUE,
    collect_metrics(ames_grid_search, type = NULL)
  )
})

test_that("`pivot_metrics()`, grid search, typical metrics, summarized", {
  expect_equal(
    pivot_metrics(ames_grid_search, collect_metrics(ames_grid_search)) %>%
      dplyr::slice(),
    tibble::tibble(
      K = integer(0),
      weight_func = character(0),
      dist_power = numeric(0),
      lon = integer(0),
      lat = integer(0),
      .config = character(0),
      rmse = numeric(0),
      rsq = numeric(0)
    )
  )
})

test_that("`pivot_metrics()`, grid search, typical metrics, unsummarized", {
  expect_equal(
    pivot_metrics(
      ames_grid_search,
      collect_metrics(ames_grid_search, summarize = FALSE)
    ) %>%
      dplyr::slice(),
    tibble::tibble(
      K = integer(0),
      weight_func = character(0),
      dist_power = numeric(0),
      lon = integer(0),
      lat = integer(0),
      .config = character(0),
      id = character(0),
      rmse = numeric(0),
      rsq = numeric(0)
    )
  )
})

test_that("`pivot_metrics()`, iterative search, typical metrics, summarized", {
  expect_equal(
    pivot_metrics(ames_iter_search, collect_metrics(ames_iter_search)) %>%
      dplyr::slice(),
    tibble::tibble(
      K = integer(0),
      weight_func = character(0),
      dist_power = numeric(0),
      lon = integer(0),
      lat = integer(0),
      .config = character(0),
      .iter = integer(0),
      rmse = numeric(0),
      rsq = numeric(0)
    )
  )
})

test_that("`pivot_metrics()`, resampled fits, fairness metrics, summarized", {
  skip_if_not_installed("kknn")

  mtcars_fair <- mtcars
  mtcars_fair$vs <- as.factor(mtcars_fair$vs)
  mtcars_fair$cyl <- as.factor(mtcars_fair$cyl)
  mtcars_fair$am <- as.factor(mtcars_fair$am)
  set.seed(4400)

  ms <-
    yardstick::metric_set(
      yardstick::demographic_parity(cyl),
      yardstick::demographic_parity(am)
    )

  res <-
    fit_resamples(
      nearest_neighbor("classification"),
      vs ~ mpg + hp + cyl,
      rsample::bootstraps(mtcars_fair, 3),
      metrics = ms
    )

  expect_equal(
    pivot_metrics(res, collect_metrics(res)) %>% slice(),
    tibble::tibble(
      .config = character(0),
      `demographic_parity(am)` = integer(0),
      `demographic_parity(cyl)` = numeric(0),
    )
  )
})

