# "selecting single eval time" means how functions like `show_best()` will pick
# an evaluation time for a dynamic metric when none is given. Previously we
# would find what is in the data and select a time that was close to the median
# time. This was fine but inconsistent with other parts of tidymodels that do
# similar operations. For example, tune_bayes has to have a metric to optimize
# on so it uses the first metric in the metric set and, if needed, the first
# evaluation time given to the function.

test_that("selecting single eval time - non-survival case", {
  library(yardstick)

  met_reg <- metric_set(rmse)

  times_1 <- 1 / 3
  times_2 <- as.numeric(5:4) / 7

  # ----------------------------------------------------------------------------
  # eval time is not applicable outside of survival models; return null

  expect_null(first_eval_time(met_reg, eval_time = NULL))
  expect_null(first_eval_time(met_reg, eval_time = times_1))
  expect_null(first_eval_time(met_reg, eval_time = times_2))

})

test_that("selecting single eval time - pure metric sets", {
  library(yardstick)

  met_int <- metric_set(brier_survival_integrated)
  met_dyn <- metric_set(brier_survival)
  met_stc <- metric_set(concordance_survival)

  times_1 <- 1 / 3
  times_2 <- as.numeric(5:4) / 7

  # ----------------------------------------------------------------------------
  # all static; return NULL and add warning if times are given

  expect_null(first_eval_time(met_stc, eval_time = NULL))
  expect_null(first_eval_time(met_stc, "concordance_survival", eval_time = NULL))

  expect_snapshot_warning(
    stc_one <- first_eval_time(met_stc, eval_time = times_1)
  )
  expect_null(stc_one)

  expect_snapshot_warning(
    stc_multi <- first_eval_time(met_stc, eval_time = times_2)
  )
  expect_null(stc_multi)

  # ----------------------------------------------------------------------------
  # all dynamic; return a single time and warn if there are more and error if
  # there are none

  expect_snapshot(
    first_eval_time(met_dyn, eval_time = NULL),
    error = TRUE
  )
  expect_snapshot(
    first_eval_time(met_dyn, "brier_survival", eval_time = NULL),
    error = TRUE
  )

  expect_equal(
    first_eval_time(met_dyn, eval_time = times_1),
    times_1
  )

  expect_snapshot_warning(
    dyn_multi <- first_eval_time(met_dyn, eval_time = times_2)
  )
  expect_equal(dyn_multi, times_2[1])

  # ----------------------------------------------------------------------------
  # all integrated; return NULL and warn if there 1+ times

  expect_null(first_eval_time(met_int, eval_time = NULL))
  expect_null(
    first_eval_time(met_int, "brier_survival_integrated", eval_time = NULL)
  )

  expect_warning(
    int_1 <- first_eval_time(met_int, eval_time = times_1)
  )
  expect_null(int_1)

  expect_warning(
    int_multi <- first_eval_time(met_int, eval_time = times_2)
  )
  expect_null(int_multi)

})

test_that("selecting single eval time - mixed metric sets - static first", {
  library(yardstick)

  met_mix_stc <- metric_set(concordance_survival, brier_survival)
  met_mix_stc_all <- metric_set(concordance_survival, brier_survival, brier_survival_integrated)

  times_1 <- 1 / 3
  times_2 <- as.numeric(5:4) / 7

  # ----------------------------------------------------------------------------
  # static is first but includes dynamic. Should return NULL and add warning
  # if times are given

  expect_null(
    first_eval_time(met_mix_stc, eval_time = NULL)
  )

  expect_warning(
    stc_1 <- first_eval_time(met_mix_stc, eval_time = times_1)
  )
  expect_null(stc_1)

  expect_warning(
    stc_multi <- first_eval_time(met_mix_stc, eval_time = times_2)
  )
  expect_null(stc_multi)

  # ----------------------------------------------------------------------------
  # static is first but includes dynamic and integrated. Should return NULL and
  # add warning if times are given

  expect_null(
    first_eval_time(met_mix_stc_all, eval_time = NULL)
  )

  expect_warning(
    stc_1 <- first_eval_time(met_mix_stc_all, eval_time = times_1)
  )
  expect_null(stc_1)

  expect_warning(
    stc_multi <- first_eval_time(met_mix_stc_all, eval_time = times_2)
  )
  expect_null(stc_multi)
})

test_that("selecting single eval time - mixed metric sets - dynamic first", {
  library(yardstick)

  met_mix_dyn <- metric_set(brier_survival, concordance_survival)
  met_mix_dyn_all <-
    metric_set(brier_survival,
               brier_survival_integrated,
               concordance_survival)

  times_1 <- 1 / 3
  times_2 <- as.numeric(5:4) / 7

  # ----------------------------------------------------------------------------
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
  expect_warning(
    dyn_multi <- first_eval_time(met_mix_dyn, eval_time = times_2)
  )
  expect_equal(dyn_multi, times_2[1])

  # ----------------------------------------------------------------------------
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
  expect_warning(
    dyn_multi <- first_eval_time(met_mix_dyn_all, eval_time = times_2)
  )
  expect_equal(dyn_multi, times_2[1])

})


test_that("selecting single eval time - mixed metric sets - integrated first", {
  library(yardstick)

  met_mix_int <- metric_set(brier_survival_integrated, concordance_survival)
  met_mix_int_all <-
    metric_set(brier_survival_integrated,
               brier_survival,
               concordance_survival)

  times_1 <- 1 / 3
  times_2 <- as.numeric(5:4) / 7

  # ----------------------------------------------------------------------------
  # integrated is first but includes static. Should return NULL and add warning
  # if 1+ times are given

  expect_null(first_eval_time(met_mix_int, eval_time = NULL))

  expect_warning(
    first_eval_time(met_mix_int, eval_time = times_1)
  )
  expect_warning(
    int_multi <- first_eval_time(met_mix_int, eval_time = times_2)
  )
  expect_null(int_multi)

  # ----------------------------------------------------------------------------
  # integrated is first but includes static and dynamic. Should return NULL and
  # add warning if 1+ times are given

  expect_null(first_eval_time(met_mix_int_all, eval_time = NULL))

  expect_warning(
    first_eval_time(met_mix_int_all, eval_time = times_1)
  )
  expect_warning(
    int_multi <- first_eval_time(met_mix_int_all, eval_time = times_2)
  )
  expect_null(int_multi)
})


test_that("selecting the first metric", {
  library(yardstick)

  met_1 <- metric_set(rmse)
  tbl_1 <- as_tibble(met_1)[1,]
  met_2 <- metric_set(rmse, ccc)
  tbl_2 <- as_tibble(met_2)[1,]

  expect_equal(first_metric(met_1), tbl_1)
  expect_equal(first_metric(met_2), tbl_2)
})
