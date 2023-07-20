
test_that("survival analysis - tuning via grid search", {
  skip_on_cran()
  skip_if_not_installed("modeldata")
  skip_if_not_installed("censored")

  suppressPackageStartupMessages(library(dplyr))
  suppressPackageStartupMessages(library(censored))
  suppressPackageStartupMessages(library(modeldata))
  suppressPackageStartupMessages(library(rsample))
  suppressPackageStartupMessages(library(yardstick))

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

  sr_tune_spec <- survival_reg(dist = tune())
  eval_times <- c(10, 100, 150)
  event_metrics <- metric_set(brier_survival, brier_survival_integrated,
                              concordance_survival, roc_auc_survival)
  sr_grid <- tibble(dist = c("loglogistic", "lognormal"))

  # ------------------------------------------------------------------------------

  sr_tune_res <-
    sr_tune_spec %>%
    tune_grid(
      event_time ~ .,
      resamples = churn_rs,
      metrics = event_metrics,
      eval_time = eval_times,
      grid = sr_grid,
      control = control_grid(save_pred = TRUE)
    )

  # ----------------------------------------------------------------------------
  # metrics

  un_sum_met <- collect_metrics(sr_tune_res, summarize = FALSE)
  sum_met    <- collect_metrics(sr_tune_res, summarize = TRUE)

  ###

  expect_equal(
    sum_met %>% slice(0),
    tibble::tibble(
      dist = character(0),
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
    nrow(sum_met),
    ( nrow(sr_grid) * 2 * length(eval_times) ) + ( 2  * nrow(sr_grid) )
    # (num grid * num dyn metr * num times) + (num_static met  * num grid)
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
      id = character(0),
      dist = character(0),
      .metric = character(0),
      .estimator = character(0),
      .eval_time = numeric(0),
      .estimate = numeric(0),
      .config = character(0)
    )
  )

  expect_equal(
    nrow(un_sum_met),
    (nrow(churn_rs) *  nrow(sr_grid) * 2 * length(eval_times) ) +
      ( 2  * nrow(sr_grid) * nrow(churn_rs) )
    # (num rsamp * num grid * num dyn metr * num times) + (num_static met  * num grid * num rsamp)
  )

  # ----------------------------------------------------------------------------
  # Predictions

  un_sum_prd <- collect_predictions(sr_tune_res, summarize = FALSE)
  sum_prd    <- collect_predictions(sr_tune_res, summarize = TRUE)

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
        dist = character(0),
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
        dist = character(0),
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

  expect_snapshot(sr_rs_aug <- augment(sr_tune_res))
  # TODO this should include dist
  # expect_equal(
  #   names(sr_rs_aug),
  #   c("event_time", "account_length", "voice_mail_plan", ".pred", ".pred_time")
  # )
  # expect_snapshot(
  #   sr_rs_logn_aug <- augment(sr_tune_res, parameters = tibble(dist = "lognormal"))
  # )
  expect_equal(
    names(sr_rs_aug$.pred[[2]]),
    c(".eval_time", ".pred_survival", ".weight_censored")
  )


  # ----------------------------------------------------------------------------
  # show/select

  expect_snapshot(show_best(sr_tune_res))
  expect_snapshot(
    show_best(sr_tune_res, metric = "brier_survival", eval_time = -1),
    error = TRUE
  )
  expect_snapshot(
    show_best(sr_tune_res, metric = "brier_survival", eval_time = 10)
  )
  expect_snapshot(show_best(sr_tune_res, metric = "brier_survival_integrated"))
  # TODO no warning
  # expect_snapshot(
  #   show_best(sr_tune_res, metric = "brier_survival_integrated", eval_time = 10),
  #   error = TRUE
  # )

  ###

  expect_snapshot(select_best(sr_tune_res))
  expect_snapshot(
    select_best(sr_tune_res, metric = "brier_survival", eval_time = -1),
    error = TRUE
  )
  expect_snapshot(
    select_best(sr_tune_res, metric = "brier_survival", eval_time = 10)
  )
  expect_snapshot(select_best(sr_tune_res, metric = "brier_survival_integrated"))
  # TODO no warning
  # expect_snapshot(
  #   select_best(sr_tune_res, metric = "brier_survival_integrated", eval_time = 10),
  #   error = TRUE
  # )

  # ----------------------------------------------------------------------------
  # autoplot

  expect_snapshot(tune_plot_1 <- autoplot(sr_tune_res))
  expect_equal(
    sort(unique(tune_plot_1$data$.metric)),
    c("brier_survival @100", "brier_survival_integrated", "concordance_survival",
      "roc_auc_survival @100")
  )
  expect_equal(tune_plot_1$facet$vars(), ".metric")


  tune_plot_2 <- autoplot(sr_tune_res, eval_time = c(10, 100))
  expect_equal(
    sort(unique(tune_plot_2$data$.metric)),
    c("brier_survival @ 10", "brier_survival @100", "brier_survival_integrated",
      "concordance_survival", "roc_auc_survival @ 10", "roc_auc_survival @100")
  )
  expect_equal(tune_plot_2$facet$vars(), ".metric")
})

