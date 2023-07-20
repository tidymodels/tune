
test_that("survival analysis - last fit", {
  skip_on_cran()
  skip_if_not_installed("modeldata")
  skip_if_not_installed("censored")

  suppressPackageStartupMessages(library(dplyr))
  suppressPackageStartupMessages(library(censored))
  suppressPackageStartupMessages(library(modeldata))
  suppressPackageStartupMessages(library(rsample))
  suppressPackageStartupMessages(library(yardstick))
  suppressPackageStartupMessages(library(recipes))
  suppressPackageStartupMessages(library(workflows))
  suppressPackageStartupMessages(library(dials))

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

  eval_times <- c(100, 10, 150)
  event_metrics <- metric_set(brier_survival, brier_survival_integrated,
                              concordance_survival, roc_auc_survival)
  sr_spec <- survival_reg(dist = "lognormal")
  sr_wflow <-
    workflow() %>%
    add_model(sr_spec) %>%
    add_formula(event_time ~ .)


  # ------------------------------------------------------------------------------

  set.seed(1)
  expect_snapshot(
    sr_last_res <-
      sr_wflow %>%
      last_fit(
        churn_split,
        metrics = event_metrics,
        eval_time = eval_times
      )
  )

  # ----------------------------------------------------------------------------
  # metrics

  un_sum_met <- collect_metrics(sr_last_res, summarize = FALSE)
  sum_met    <- collect_metrics(sr_last_res, summarize = TRUE)

  ###

  expect_equal(
    sum_met %>% slice(0),
    tibble::tibble(
      .metric = character(0),
      .estimator = character(0),
      .eval_time = numeric(0),
      .estimate = numeric(0),
      .config = character(0)
    )
  )
  expect_equal(
    nrow(sum_met),
    (2 * length(eval_times) ) + ( 2  )
    # (num dyn metr * num times) + (num_static met )
  )
  expect_equal(
    unique(sum_met$.eval_time),
    c(10, 100, 150, NA)
  )
  expect_equal(
    sort(unique(sum_met$.metric)),
    c("brier_survival", "brier_survival_integrated", "concordance_survival",
      "roc_auc_survival")
  )

  ###

  expect_equal(
    un_sum_met %>% slice(0),
    tibble::tibble(
      .metric = character(0),
      .estimator = character(0),
      .eval_time = numeric(0),
      .estimate = numeric(0),
      .config = character(0)
    )
  )

  expect_equal(
    nrow(un_sum_met),
    (2 * length(eval_times) ) + ( 2  )
    # (num dyn metr * num times) + (num_static met )
  )

  # ----------------------------------------------------------------------------
  # Predictions

  un_sum_prd <- collect_predictions(sr_last_res, summarize = FALSE)
  sum_prd    <- collect_predictions(sr_last_res, summarize = TRUE)

  ###

  # can't show ptypes with tibbles with Surv objects
  surv_str <-
    structure(
      numeric(0),
      type = "right",
      dim = c(0L, 2L),
      dimnames = list(NULL, c("time", "status")),
      class = "Surv"
    )

  expect_equal(
    un_sum_prd %>% slice(0),
    structure(
      list(
        id = character(0),
        .pred = list(),
        .row = integer(0),
        .pred_time = numeric(0),
        event_time = surv_str,
        .config = character(0)
      ),
      row.names = integer(0),
      class = c("tbl_df", "tbl", "data.frame")
    )
  )
  expect_equal(
    un_sum_prd$.pred[[1]] %>% slice(0),
    tibble::tibble(
      .eval_time = numeric(0),
      .pred_survival = numeric(0),
      .weight_censored = numeric(0)
    )
  )
  expect_true(inherits(un_sum_prd$.pred[[1]], "tbl_df"))

  ###

  expect_equal(
    sum_prd %>% slice(0),
    structure(
      list(
        .pred = list(),
        .row = integer(0),
        .pred_time = numeric(0),
        event_time = surv_str,
        .config = character(0)
      ),
      row.names = integer(0),
      class = c("tbl_df", "tbl", "data.frame")
    )
  )
  expect_equal(
    sum_prd$.pred[[1]] %>% slice(0),
    tibble::tibble(
      .eval_time = numeric(0),
      .pred_survival = numeric(0),
      .weight_censored = numeric(0)
    )
  )
  expect_true(inherits(sum_prd$.pred[[1]], "tbl_df"))

  ###

  expect_snapshot(sr_rs_aug <- augment(sr_last_res))
  expect_equal(
    names(sr_rs_aug),
    c("event_time", "account_length", "voice_mail_plan", ".pred", ".pred_time")
  )
  expect_equal(
    names(sr_rs_aug$.pred[[2]]),
    c(".eval_time", ".pred_survival", ".weight_censored")
  )


})

