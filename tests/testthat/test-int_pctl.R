
test_that("percentile intervals - resamples only", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("rsample", minimum_version = "1.1.1.9000")
  library(rsample)
  library(parsnip)

  data(Sacramento, package = "modeldata")
  set.seed(13)
  sac_rs <- vfold_cv(Sacramento)
  lm_res <-
    linear_reg() %>%
    fit_resamples(
      log10(price) ~ beds + baths + sqft + type + latitude + longitude,
      resamples = sac_rs,
      control = control_resamples(save_pred = TRUE)
    )
  template <- dplyr::tibble(
    .metric = character(0),
    .estimator = character(0),
    .lower = numeric(0),
    .estimate = numeric(0),
    .upper = numeric(0),
    .config = character(0)
  )
  expect_snapshot_warning(int_res_1 <- int_pctl(lm_res, times = 200))
  expect_equal(template, int_res_1[0,])
  expect_equal(2, nrow(int_res_1))
  expect_snapshot_error(sca_rs)

})


test_that("percentile intervals - last fit", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("rsample", minimum_version = "1.1.1.9000")
  library(rsample)
  library(parsnip)
  library(yardstick)

  data(Sacramento, package = "modeldata")
  set.seed(1)
  sac_split <- initial_split(Sacramento)
  f <- log10(price) ~ beds + baths + sqft + type + latitude + longitude

  lm_res <-
    linear_reg() %>%
    last_fit(
      log10(price) ~ beds + baths + sqft + type + latitude + longitude,
      metrics = metric_set(mae),
      split = sac_split
    )
  template <- dplyr::tibble(
    .metric = character(0),
    .estimator = character(0),
    .lower = numeric(0),
    .estimate = numeric(0),
    .upper = numeric(0),
    .config = character(0)
  )
  set.seed(1)
  expect_snapshot_warning(int_res_1 <- int_pctl(lm_res, times = 200))
  expect_equal(template, int_res_1[0,])
  expect_equal(1, nrow(int_res_1))
  set.seed(1)
  expect_snapshot_warning(int_res_2 <- int_pctl(lm_res, times = 200))
  expect_equal(int_res_1, int_res_2)
})



test_that("percentile intervals - tuning", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("C50")
  skip_if_not_installed("rsample", minimum_version = "1.1.1.9000")
  library(rsample)
  library(parsnip)
  library(yardstick)

  data("two_class_dat", package = "modeldata")
  set.seed(1)
  cls_rs <- validation_split(two_class_dat)

  c5_res <-
    decision_tree(min_n = tune()) %>%
    set_engine("C5.0") %>%
    set_mode("classification") %>%
    tune_grid(
      Class ~.,
      resamples = cls_rs,
      grid = dplyr::tibble(min_n = c(5, 20, 40)),
      metrics = metric_set(sens),
      control = control_grid(save_pred = TRUE)
    )
  template <- dplyr::tibble(
    .metric = character(0),
    .estimator = character(0),
    .lower = numeric(0),
    .estimate = numeric(0),
    .upper = numeric(0),
    .config = character(0),
    min_n = numeric(0)
  )
  int_res_1 <- int_pctl(c5_res)
  expect_equal(template, int_res_1[0,])
  expect_equal(3, nrow(int_res_1))

  # ------------------------------------------------------------------------------

  set.seed(92)
  c5_bo_res <-
    decision_tree(min_n = tune()) %>%
    set_engine("C5.0") %>%
    set_mode("classification") %>%
    tune_bayes(
      Class ~.,
      resamples = cls_rs,
      initial = c5_res,
      iter = 1,
      metrics = metric_set(sens),
      control = control_bayes(save_pred = TRUE)
    )
  template <- dplyr::tibble(
    .metric = character(0),
    .estimator = character(0),
    .lower = numeric(0),
    .estimate = numeric(0),
    .upper = numeric(0),
    .config = character(0),
    .iter = integer(0),
    min_n = integer(0)
  )
  set.seed(1)
  int_res_2 <- int_pctl(c5_bo_res)
  expect_equal(template, int_res_2[0,])
  expect_equal(4, nrow(int_res_2))
  set.seed(1)
  int_res_3 <- int_pctl(c5_bo_res, event_level = "second")
  expect_true(all(int_res_3$.estimate > int_res_2$.estimate))

  # ------------------------------------------------------------------------------

  c5_mixed_res <-
    decision_tree(min_n = tune()) %>%
    set_engine("C5.0") %>%
    set_mode("classification") %>%
    tune_grid(
      Class ~.,
      resamples = cls_rs,
      grid = dplyr::tibble(min_n = c(20, 40)),
      metrics = metric_set(roc_auc, sens),
      control = control_grid(save_pred = TRUE)
    )
  template <- dplyr::tibble(
    .metric = character(0),
    .estimator = character(0),
    .lower = numeric(0),
    .estimate = numeric(0),
    .upper = numeric(0),
    .config = character(0),
    min_n = numeric(0)
  )
  set.seed(2093)
  int_res_4 <- int_pctl(c5_mixed_res)
  expect_equal(template, int_res_4[0,])
  expect_equal(4, nrow(int_res_4))
})

