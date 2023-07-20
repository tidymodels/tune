
test_that("survival analysis", {
  skip_on_cran()
  skip_if_not_installed("modeldata")
  skip_if_not_installed("censored")

  library(dplyr)
  library(censored)
  library(modeldata)
  library(rsample)
  library(yardstick)

  # ----------------------------------------------------------------------------

  data("mlc_churn")

  mlc_churn <-
    mlc_churn %>%
    mutate(
      churned = ifelse(churn == "yes", 1, 0),
      event_time = Surv(account_length, churned)
    ) %>%
    select(event_time, account_length, voice_mail_plan) %>%
    slice(1:500)

  set.seed(6941)
  churn_split <- initial_split(mlc_churn, prop = 4/5)
  churn_tr <- training(churn_split)
  churn_te <- testing(churn_split)
  churn_rs <- bootstraps(churn_tr, times = 2)

  # ----------------------------------------------------------------------------

  sr_spec <- survival_reg()
  sr_tune_spec <- survival_reg(dist = tune())
  eval_times <- c(10, 100, 150)
  event_metrics <- metric_set(brier_survival, brier_survival_integrated,
                              concordance_survival, roc_auc_survival)

  # ------------------------------------------------------------------------------

  sr_rs_res <-
    sr_spec %>%
    fit_resamples(
      event_time ~ .,
      resamples = churn_rs,
      metrics = event_metrics,
      eval_time = eval_times,
      control = control_resamples(save_pred = TRUE)
    )

  sr_tune_res <-
    sr_tune_spec %>%
    tune_grid(
      event_time ~ .,
      resamples = churn_rs,
      metrics = event_metrics,
      eval_time = eval_times,
      grid = tibble(dist = c("loglogistic", "lognormal")),
      control = control_grid(save_pred = TRUE)
    )

  # ----------------------------------------------------------------------------
  # metrics

  un_sum_met <- collect_metrics(sr_rs_res)
  sum_met <- collect_metrics(sr_rs_res, summarize = FALSE)

  ###

  expect_equal(
    un_sum_met %>% slice(0),
    tibble::tibble(
      .metric = character(0),
      .estimator = character(0),
      .eval_time = numeric(0),
      mean = numeric(0),
      n = integer(0),
      std_err = numeric(0),
      .config = character(0)
    )
  )
  expect_equal(
    nrow(un_sum_met),
    (2 * length(eval_times)) + 2
    # (num dyn metr * num times) + num_static met
  )
  expect_equal(
    unique(un_sum_met$.eval_time),
    c(10, 100, 150, NA)
  )
  expect_equal(
    sort(unique(un_sum_met$.metric)),
    c("brier_survival", "brier_survival_integrated", "concordance_survival",
      "roc_auc_survival")
  )

  ###

  expect_equal(
    sum_met %>% slice(0),
    tibble::tibble(
      id = character(0),
      .metric = character(0),
      .estimator = character(0),
      .eval_time = numeric(0),
      .estimate = numeric(0),
      .config = character(0)
    )
  )

  expect_equal(
    nrow(sum_met),
    (nrow(churn_rs) * 2 * length(eval_times)) + (nrow(churn_rs) * 2)
    # (num resamples * num dyn metr * num times) + (resamples and * num_static met)
  )

  # ----------------------------------------------------------------------------
  # Predictions

  un_sum_prd <- collect_predictions(sr_rs_res)
  sum_prd <- collect_predictions(sr_rs_res, summarize = FALSE)

  ###

  expect_equal(
    un_sum_prd %>% slice(0),
    structure(
      list(
        id = character(0),
        .pred = list(),
        .row = integer(0),
        .pred_time = numeric(0),
        event_time = structure(
          numeric(0),
          type = "right",
          dim = c(0L, 2L),
          dimnames = list(NULL, c("time", "status")),
          class = "Surv"
        ),
        .config = character(0)
      ),
      row.names = integer(0),
      class = c("tbl_df", "tbl", "data.frame")
    )
  )


})

