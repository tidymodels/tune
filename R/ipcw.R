# ------------------------------------------------------------------------------
# Helper functions for using inverse probability of censoring weights (IPCW) in
# censored regression models


# ------------------------------------------------------------------------------
# Low-level

.extract_time <- function(x) {
  x[, "time"]
}

.extract_status <- function(x) {
  res <-   x[, "status"]
  un_vals <- sort(unique(res))
  if (identical(un_vals, 1:2) | identical(un_vals, c(1.0, 2.0))) {
    res <- res - 1
  }
  res
}


collect_surv_col <- function(data, truth) {
  surv_col <-
    tidyselect::eval_select(
      truth,
      data = data,
      allow_predicates = FALSE,
      allow_rename = FALSE,
      allow_empty = FALSE
    )
  surv_col <- names(surv_col)
  if (length(surv_col) != 1 || !inherits(data[[surv_col]], "Surv")) {
    rlang::abort("'truth' must correspond to a single column of class 'Surv'")
  }
  dplyr::select(data, dplyr::all_of(surv_col)) %>%
    stats::setNames("surv")
}

# For avoiding extremely large, outlier weights
trunc_probs <- function(probs, trunc = 0.01) {
  probs_non_zero <- probs[!is.na(probs)]
  non_zero_min <- min(probs_non_zero[probs_non_zero > 0])
  if (non_zero_min < trunc) {
    trunc <- non_zero_min / 2
  }
  ifelse(probs <= trunc, trunc, probs)
}

add_wt_class <- function(x, cls) {
  class(x) <- c(class(x), cls)
  x
}

# ------------------------------------------------------------------------------
# Brier score helpers. Most of this is based off of Graf, E., Schmoor, C.,
# Sauerbrei, W. and Schumacher, M. (1999), Assessment and comparison of
# prognostic classification schemes for survival data. _Statist. Med._, 18:
# 2529-2545.

# We need to use the time of analysis to determine what time to use to evaluate
# the IPCWs.

graf_weight_time <- function(surv_obj, predict_time, eps = 10^-10) {
  # TODO check for right censoring?
  event_time <- .extract_time(surv_obj)
  status <- .extract_status(surv_obj)
  is_event_before_t <- event_time <= predict_time & status == 1
  is_censored <- event_time > predict_time

  # Three possible contributions to the statistic from Graf 1999

  # Censoring time before predict_time, no contribution (Graf category 3)
  weight_time <- rep(NA_real_, length(event_time))

  # A real event prior to predict time (Graf category 1)
  weight_time[is_event_before_t] <- event_time[is_event_before_t] - eps

  # Observed time greater than predict_time (Graf category 2)
  weight_time[is_censored] <- predict_time

  tibble::tibble(surv = surv_obj, weight_time = weight_time, .time = predict_time) %>%
    parsnip::add_rowindex()
}

# Overall wrapper to compute the weights and line everything up in a big tibble

#' Calculations for two types of IPCW
#'
#' @param data A data frame with a column containing a [survival::Surv()] object.
#' @param truth An unquoted variable name that has the  [survival::Surv()] object.
#' @param analysis_time A vector of non-negative times at which we should
#' compute the probability of censoring and the corresponding weights. For ROC
#' analyses, the weights are independent of time but this argument is retained
#' to keep a consistent API across functions.
#' @param model_fit A fitted parsnip model object with a mode of "censored regression".
#' @param trunc A potential lower bound for the probability of censoring to avoid
#' very large weight values.
#' @export
#' @keywords internal
.survival_weights_brier <- function(data, truth, analysis_time, model_fit, trunc = 0.05) {
  truth <- rlang::enquo(truth)
  surv_data <- collect_surv_col(data, truth)
  purrr::map_dfr(analysis_time, ~ graf_weight_time(surv_data$surv, .x))  %>%
    dplyr::mutate(
      cens_prob = predict(model_fit$censor_probs, time = weight_time, as_vector = TRUE),
      cens_prob = trunc_probs(cens_prob, trunc),
      censoring_weights = 1 / cens_prob,
      censoring_weights = add_wt_class(censoring_weights, "brier_survival_weights")
    ) %>%
    dplyr::select(.row, censoring_weights, .time) %>%
    # TODO remove later?
    dplyr::select(-.time) %>%
    tidyr::nest(censoring_weights = c(censoring_weights)) %>%
    dplyr::select(-.row)
}

# TODO unnest and add group_by()?

# ------------------------------------------------------------------------------
# ROC curve helpers. Section 4.3 of Blanche, P., Dartigues, J.-F. and
# Jacqmin-Gadda, H. (2013), Review and comparison of ROC curve estimators for a
# time-dependent outcome with marker-dependent censoring. Biom. J., 55: 687-704.
# is a good starting place to understand these calculations.

# Note that these weights are independent of the time of analysis. We keep the
# same API as the Brier function for API consistency.

#' @export
#' @keywords internal
#' @rdname dot-survival_weights_brier
.survival_weights_roc <- function(data, truth, analysis_time, model_fit,
                                 trunc = 0.05, eps = 10^-10) {

  truth <- rlang::enquo(truth)
  collect_surv_col(data, truth) %>%
    dplyr::mutate(
      weight_time = .extract_time(surv) - eps,
      cens_prob = predict(model_fit$censor_probs, time = weight_time, as_vector = TRUE),
      cens_prob = trunc_probs(cens_prob, trunc),
      censoring_weights = 1 / cens_prob,
      censoring_weights = add_wt_class(censoring_weights, "roc_survival_weights")
    ) %>%
    dplyr::select(censoring_weights)
}


# add a helper function to use in predict_model() (at the end) to compute the weights
