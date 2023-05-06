test_that("basic functionality", {
  skip_on_cran()
  skip_if_not_installed("modeldata")
  library(modeldata)
  library(parsnip)
  library(workflows)
  library(rsample)

  silly_wt_fn <- function(.propensity, .exposure = NULL, ...) {
    .propensity
  }

  propensity_wf <- workflow(Class ~ B, logistic_reg())
  outcome_wf <- workflow(A ~ Class, linear_reg()) %>% add_case_weights(.wts)

  set.seed(1)
  boots <- bootstraps(two_class_dat)

  res_fit_resamples <-
    fit_resamples(
      propensity_wf,
      resamples = boots,
      # can't be `save_workflow = TRUE`, as we need the _fitted_ workflow
      control = control_resamples(extract = identity)
    )

  res_weight_propensity <-
    res_fit_resamples %>%
    weight_propensity(silly_wt_fn)

  # `weight_propensity()` preserves rset properties:
  preserved <- c("class", "times", "apparent", "breaks", "pool")
  expect_equal(attributes(boots)[preserved], attributes(res_weight_propensity)[preserved])
  expect_equal(boots$id, res_weight_propensity$id)
  expect_equal(
    purrr::map(boots$splits, purrr::pluck, "in_id"),
    purrr::map(res_weight_propensity$splits, purrr::pluck, "in_id")
  )
  expect_equal(
    purrr::map(boots$splits, purrr::pluck, "data"),
    purrr::map(res_weight_propensity$splits, purrr::pluck, "data") %>%
      purrr::map(dplyr::select, -.wts)
  )

  # confirming that `.wts` change with every resample. this expectation
  # specifically only works because `wt_fn ~= identity`
  wf_fits <- purrr::map(res_fit_resamples$.extracts, purrr::pluck, ".extracts", 1)
  expect_equal(
    purrr::map(boots$splits, analysis) %>%
      purrr::map2(wf_fits, ., predict, type = "prob") %>%
      purrr::map(dplyr::pull, 2),
    purrr::map(res_weight_propensity$splits, analysis) %>%
      purrr::map(dplyr::pull, .wts) %>%
      purrr::map(as.numeric)
  )

  # output is valid input to another call to `fit_resamples()`
  res_final <-
    res_weight_propensity %>%
    fit_resamples(outcome_wf, resamples = .)

  expect_s3_class(res_final, "tune_results")
})

test_that("errors informatively with bad input", {
  skip_if_not_installed("modeldata")
  library(modeldata)
  library(parsnip)
  library(workflows)
  library(rsample)

  silly_wt_fn <- function(.propensity, .exposure = NULL, ...) {
    .propensity
  }


  propensity_wf <- workflow(Class ~ B, logistic_reg())
  outcome_wf <- workflow(A ~ Class, linear_reg()) %>% add_case_weights(.wts)

  set.seed(1)
  boots <- bootstraps(two_class_dat)

  res_fit_resamples <-
    fit_resamples(
      propensity_wf,
      resamples = boots,
      # can't be `save_workflow = TRUE`, as we need the _fitted_ workflow
      control = control_resamples(extract = identity)
    )

  res_fit_resamples_bad <-
    fit_resamples(propensity_wf, resamples = boots)

  res_weight_propensity <-
    res_fit_resamples %>%
    weight_propensity(silly_wt_fn)

  # did not set `control`
  expect_snapshot(
    error = TRUE,
    weight_propensity(res_fit_resamples_bad, silly_wt_fn)
  )

  # bad `wt_fn`
  expect_snapshot(
    error = TRUE,
    weight_propensity(res_fit_resamples)
  )

  expect_snapshot(
    error = TRUE,
    weight_propensity(res_fit_resamples, "boop")
  )

  expect_snapshot(
    error = TRUE,
    weight_propensity(res_fit_resamples, function(...) {-1L})
  )

  # mistakenly supplied `data`
  expect_snapshot(
    error = TRUE,
    weight_propensity(res_fit_resamples, silly_wt_fn, data = two_class_dat)
  )
})

test_that("results match manual calculation", {
  skip_on_cran()
  skip_if_not_installed("modeldata")
  library(modeldata)
  library(parsnip)
  library(workflows)
  library(rsample)
  library(hardhat)

  silly_wt_fn <- function(.propensity, .exposure = NULL, ...) {
    .propensity
  }

  propensity_wf <- workflow(Class ~ B, logistic_reg())
  outcome_wf <- workflow(A ~ Class, linear_reg()) %>% add_case_weights(.wts)

  set.seed(1)
  boots <- bootstraps(two_class_dat)

  # our way:
  res_tm <-
    fit_resamples(
      propensity_wf,
      resamples = boots,
      control = control_resamples(extract = identity)
    ) %>%
    weight_propensity(silly_wt_fn) %>%
    fit_resamples(
      outcome_wf,
      resamples = .,
      # would usually `extract = tidy` here, but don't want to
      # register broom:::tidy.lm
      control = control_resamples(extract = identity)
    )

  # a la r-causal:
  fit_ipw <- function(split, ...) {
    .df <- analysis(split)

    propensity_model <- fit(propensity_wf, .df)

    preds <- predict(propensity_model, new_data = .df, type = "prob")
    .df$.wts <- importance_weights(silly_wt_fn(preds[[2]]))

    fit(outcome_wf, .df)
  }

  res_rc <- purrr::map(boots$splits, fit_ipw)

  # comparison:
  expect_equal(
    res_tm %>%
      collect_extracts() %>%
      pull(.extracts) %>%
      purrr::map(extract_fit_engine) %>%
      purrr::map(coef),
    purrr::map(res_rc, extract_fit_engine) %>%
      purrr::map(coef)
  )
})
