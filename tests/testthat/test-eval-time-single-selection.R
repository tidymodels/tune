library(yardstick)

test_that("selecting single eval time - non-survival case", {
  met_reg <- metric_set(rmse)

  times_1 <- 1 / 3
  times_2 <- as.numeric(5:4) / 7

  # eval time is not applicable outside of survival models; return null

  expect_null(first_eval_time(met_reg, eval_time = NULL))
  expect_null(first_eval_time(met_reg, eval_time = times_1))
  expect_null(first_eval_time(met_reg, eval_time = times_2))

})

test_that("selecting single eval time - pure metric sets", {
  met_int <- metric_set(brier_survival_integrated)
  met_dyn <- metric_set(brier_survival)
  met_stc <- metric_set(concordance_survival)

  times_1 <- 1 / 3
  times_2 <- as.numeric(5:4) / 7

  # all static; return NULL and add warning if times are given

  expect_null(first_eval_time(met_stc, eval_time = NULL))
  expect_null(first_eval_time(met_stc, metric = "concordance_survival", eval_time = NULL))

  expect_silent(
    stc_one <- first_eval_time(met_stc, eval_time = times_1)
  )
  expect_null(stc_one)

  expect_silent(
    stc_multi <- first_eval_time(met_stc, eval_time = times_2)
  )
  expect_null(stc_multi)

  # all dynamic; return a single time and warn if there are more and error if
  # there are none

  expect_snapshot(
    first_eval_time(met_dyn, eval_time = NULL),
    error = TRUE
  )
  expect_snapshot(
    first_eval_time(met_dyn, metric = "brier_survival", eval_time = NULL),
    error = TRUE
  )

  expect_equal(
    first_eval_time(met_dyn, eval_time = times_1),
    times_1
  )

  expect_snapshot(
    dyn_multi <- first_eval_time(met_dyn, eval_time = times_2)
  )
  expect_equal(dyn_multi, times_2[1])

  # all integrated; return NULL and warn if there 1+ times

  expect_null(first_eval_time(met_int, eval_time = NULL))
  expect_null(
    first_eval_time(met_int, metric = "brier_survival_integrated", eval_time = NULL)
  )

  expect_silent(
    int_1 <- first_eval_time(met_int, eval_time = times_1)
  )
  expect_null(int_1)

  expect_silent(
    int_multi <- first_eval_time(met_int, eval_time = times_2)
  )
  expect_null(int_multi)

})

test_that("selecting single eval time - mixed metric sets - static first", {
  met_mix_stc <- metric_set(concordance_survival, brier_survival)
  met_mix_stc_all <- metric_set(concordance_survival, brier_survival, brier_survival_integrated)

  times_1 <- 1 / 3
  times_2 <- as.numeric(5:4) / 7

  # static is first but includes dynamic. Should return NULL and add warning
  # if times are given

  expect_null(
    first_eval_time(met_mix_stc, eval_time = NULL)
  )

  expect_silent(
    stc_1 <- first_eval_time(met_mix_stc, eval_time = times_1)
  )
  expect_null(stc_1)

  expect_silent(
    stc_multi <- first_eval_time(met_mix_stc, eval_time = times_2)
  )
  expect_null(stc_multi)

  # static is first but includes dynamic and integrated. Should return NULL and
  # add warning if times are given

  expect_null(
    first_eval_time(met_mix_stc_all, eval_time = NULL)
  )

  expect_silent(
    stc_1 <- first_eval_time(met_mix_stc_all, eval_time = times_1)
  )
  expect_null(stc_1)

  expect_silent(
    stc_multi <- first_eval_time(met_mix_stc_all, eval_time = times_2)
  )
  expect_null(stc_multi)
})

test_that("selecting single eval time - mixed metric sets - dynamic first", {
  met_mix_dyn <- metric_set(brier_survival, concordance_survival)
  met_mix_dyn_all <-
    metric_set(brier_survival,
               brier_survival_integrated,
               concordance_survival)

  times_1 <- 1 / 3
  times_2 <- as.numeric(5:4) / 7

  # dynamic is first but includes static. Should return single time and add warning
  # if 2+ times are given

  expect_snapshot(
    first_eval_time(met_mix_dyn, eval_time = NULL),
    error = TRUE
  )
  expect_equal(
    first_eval_time(met_mix_dyn, eval_time = times_1),
    times_1
  )
  expect_snapshot(
    dyn_multi <- first_eval_time(met_mix_dyn, eval_time = times_2)
  )
  expect_equal(dyn_multi, times_2[1])

  # dynamic is first but includes static and integrated. Should return single
  # time and add warning if 2+ times are given

  expect_snapshot(
    first_eval_time(met_mix_dyn_all, eval_time = NULL),
    error = TRUE
  )
  expect_equal(
    first_eval_time(met_mix_dyn_all, eval_time = times_1),
    times_1
  )
  expect_snapshot(
    dyn_multi <- first_eval_time(met_mix_dyn_all, eval_time = times_2)
  )
  expect_equal(dyn_multi, times_2[1])

})


test_that("selecting single eval time - mixed metric sets - integrated first", {
  met_mix_int <- metric_set(brier_survival_integrated, concordance_survival)
  met_mix_int_all <-
    metric_set(brier_survival_integrated,
               brier_survival,
               concordance_survival)

  times_1 <- 1 / 3
  times_2 <- as.numeric(5:4) / 7

  # integrated is first but includes static. Should return NULL and add warning
  # if 1+ times are given

  expect_null(first_eval_time(met_mix_int, eval_time = NULL))

  expect_silent(
    first_eval_time(met_mix_int, eval_time = times_1)
  )
  expect_silent(
    int_multi <- first_eval_time(met_mix_int, eval_time = times_2)
  )
  expect_null(int_multi)

  # integrated is first but includes static and dynamic. Should return NULL and
  # add warning if 1+ times are given

  expect_null(first_eval_time(met_mix_int_all, eval_time = NULL))

  expect_silent(
    first_eval_time(met_mix_int_all, eval_time = times_1)
  )
  expect_silent(
    int_multi <- first_eval_time(met_mix_int_all, eval_time = times_2)
  )
  expect_null(int_multi)
})


test_that("selecting an evaluation time", {
  # much of this is indirectly tested in show/select best

  surv_res <- readRDS(test_path("data", "surv_boost_tree_res.rds"))

  expect_snapshot(
    choose_eval_time(surv_res, "brier_survival")
  )
  expect_snapshot(
    choose_eval_time(surv_res, "concordance_survival")
  )
  expect_snapshot(
    choose_eval_time(surv_res, "concordance_survival", eval_time = 10)
  )

  data("example_ames_knn")
  expect_snapshot(choose_eval_time(ames_grid_search, "rmse", eval_time = 1))
})
