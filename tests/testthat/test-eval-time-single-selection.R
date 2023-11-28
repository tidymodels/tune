
test_that("selecting single eval time - non-survival case", {
  library(yardstick)

  met_reg <- metric_set(rmse)

  times_1 <- 1 / 3
  times_2 <- as.numeric(5:4) / 7

  # ----------------------------------------------------------------------------
  # eval time is not applicable outside of survival models; return null

  expect_null(select_eval_time(met_reg, eval_time = NULL, single = TRUE))
  expect_null(select_eval_time(met_reg, eval_time = times_1, single = TRUE))
  expect_null(select_eval_time(met_reg, eval_time = times_2, single = TRUE))

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

  expect_null(select_eval_time(met_stc, eval_time = NULL, single = TRUE))

  expect_snapshot_warning(
    stc_one <- select_eval_time(met_stc, eval_time = times_1, single = TRUE)
  )
  expect_null(stc_one)

  expect_snapshot_warning(
    stc_multi <- select_eval_time(met_stc, eval_time = times_2, single = TRUE)
  )
  expect_null(stc_multi)

  # ----------------------------------------------------------------------------
  # all dynamic; return a single time and warn if there are more or zero

  expect_snapshot(
    select_eval_time(met_dyn, eval_time = NULL, single = TRUE),
    error = TRUE
  )
  expect_equal(
    select_eval_time(met_dyn, eval_time = times_1, single = TRUE),
    times_1
  )
  expect_snapshot_warning(
    dyn_multi <- select_eval_time(met_dyn, eval_time = times_2, single = TRUE)
  )
  expect_equal(dyn_multi, times_2[1])

  # ----------------------------------------------------------------------------
  # all integrated; return NULL and error if there < 2

  expect_snapshot(
    select_eval_time(met_int, eval_time = NULL, single = TRUE),
    error = TRUE
  )
  expect_snapshot(
    select_eval_time(met_int, eval_time = times_1, single = TRUE),
    error = TRUE
  )

  expect_silent(
    int_1 <- select_eval_time(met_int, eval_time = times_2, single = TRUE)
  )
  expect_null(int_1)


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
    select_eval_time(met_mix_stc, eval_time = NULL, single = TRUE)
  )
  # TODO should not warn
  expect_warning(
    select_eval_time(met_mix_stc, eval_time = times_1, single = TRUE)
  )
  expect_warning(
    select_eval_time(met_mix_stc, eval_time = times_2, single = TRUE)
  )

  # ----------------------------------------------------------------------------
  # static is first but includes dynamic and integrated. Should return NULL and add warning
  # if times are given

  # TODO errors but should not since first is static; should not warn

  # expect_null(
  #   select_eval_time(met_mix_stc_all, eval_time = NULL, single = TRUE)
  # )
  # expect_warning(
  #   select_eval_time(met_mix_stc_all, eval_time = times_1, single = TRUE)
  # )
  # expect_warning(
  #   select_eval_time(met_mix_stc_all, eval_time = times_2, single = TRUE)
  # )


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
  # dynamic is first but includes static Should return NULL and add warning
  # if times are given

  expect_snapshot(
    select_eval_time(met_mix_dyn, eval_time = NULL, single = TRUE),
    error = TRUE
  )
  # TODO should not warn
  expect_equal(
    select_eval_time(met_mix_dyn, eval_time = times_1, single = TRUE),
    times_1
  )
  expect_warning(
    dyn_multi <- select_eval_time(met_mix_dyn, eval_time = times_2, single = TRUE)
  )
  expect_equal(dyn_multi, times_2[1])

  # ----------------------------------------------------------------------------
  # dynamic is first but includes static and integrated. Should return NULL and add warning
  # if times are given

  expect_snapshot(
    select_eval_time(met_mix_dyn_all, eval_time = NULL, single = TRUE),
    error = TRUE
  )
  # TODO errors but should not
  # expect_warning(
  #   select_eval_time(met_mix_dyn_all, eval_time = times_1, single = TRUE)
  # )
  expect_warning(
    dyn_multi <- select_eval_time(met_mix_dyn_all, eval_time = times_2, single = TRUE)
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
  # integrated is first but includes static. Should return NULL and add error
  # if <2 times are given

  expect_snapshot(
    select_eval_time(met_mix_int, eval_time = NULL, single = TRUE),
    error = TRUE
  )
  expect_snapshot(
    select_eval_time(met_mix_int, eval_time = times_1, single = TRUE),
    error = TRUE
  )
  expect_silent(
    int_multi <- select_eval_time(met_mix_int, eval_time = times_2, single = TRUE)
  )
  expect_null(int_multi)

  # ----------------------------------------------------------------------------
  # integrated is first but includes static and dynamic. Should return NULL and
  # add error if <2 times are given

  expect_snapshot(
    select_eval_time(met_mix_int_all, eval_time = NULL, single = TRUE),
    error = TRUE
  )
  expect_snapshot(
    select_eval_time(met_mix_int_all, eval_time = times_1, single = TRUE),
    error = TRUE
  )
  expect_silent(
    int_multi <- select_eval_time(met_mix_int_all, eval_time = times_2, single = TRUE)
  )
  expect_null(int_multi)

})
