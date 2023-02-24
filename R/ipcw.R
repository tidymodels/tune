# ------------------------------------------------------------------------------
# Helper functions for using inverse probability of censoring weights (IPCW) in
# censored regression models


# ------------------------------------------------------------------------------
# Low-level

check_cens_type <- function(x, type = "right", fail = FALSE) {
  obj_type <- attr(x, "type")
  good_type <- all(obj_type %in% type)
  if (fail && !good_type) {
    c_list <- paste0(type, collapse = ", ")
    msg <- glue::glue("For this usage, the allowed censoring types are: {c_list}")
    rlang::abort(msg)
  }
  good_type
}


.is_censored_right <- function(x) {
  check_cens_type(x, fail = FALSE)
}

.check_censored_right <- function(x) {
  check_cens_type(x, fail = TRUE)
}

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

graf_weight_time <- function(surv_obj, eval_time, rows = NULL, eps = 10^-10) {
  event_time <- .extract_time(surv_obj)
  status <- .extract_status(surv_obj)
  is_event_before_t <- event_time <= eval_time & status == 1
  is_censored <- event_time > eval_time

  # Three possible contributions to the statistic from Graf 1999

  # Censoring time before eval_time, no contribution (Graf category 3)
  weight_time <- rep(NA_real_, length(event_time))

  # A real event prior to predict time (Graf category 1)
  weight_time[is_event_before_t] <- event_time[is_event_before_t] - eps

  # Observed time greater than eval_time (Graf category 2)
  weight_time[is_censored] <- eval_time

  res <- tibble::tibble(surv = surv_obj, weight_time = weight_time, eval_time)
  add_row_index(res, rows)
}

# ------------------------------------------------------------------------------

#' Calculations for two types of IPCW
#'
#' @param data A data frame with a column containing a [survival::Surv()] object.
#' @param truth An unquoted variable name that has the  [survival::Surv()] object.
#' @param predictors Not currently used. A potential future slot for models with
#' informative censoring based on columns in `data`.
#' @param rows An optional integer vector with length equal to the number of
#' rows in `data` that is used to index the original data. The default is to
#' use a fresh index on data (i.e. `1:nrow(data)`).
#' @param eval_time A vector of non-negative times at which we should
#' compute the probability of censoring and the corresponding weights. For ROC
#' analyses, the weights are independent of time but this argument is retained
#' to keep a consistent API across functions.
#' @param model_fit A fitted parsnip model object with a mode of "censored regression".
#' @param trunc A potential lower bound for the probability of censoring to avoid
#' very large weight values.
#' @export
#' @name survival_weights
#' @keywords internal
.survival_weights_graf <- function(data, truth,
                                   eval_time, model_fit,
                                   rows = NULL,
                                   predictors = NULL,
                                   trunc = 0.05, eps = 10^-10) {
  if (!is.null(predictors)) {
    rlang::warn("The 'predictors' argument to the survival weighting function is not currently used.")
  }
  eval_time <- filter_eval_time(eval_time)

  truth <- rlang::enquo(truth)
  surv_data <- collect_surv_col(data, truth)
  .check_censored_right(surv_data$surv)

  purrr::map_dfr(eval_time,
                 ~ graf_weight_time(surv_data$surv, .x, eps = eps, rows = rows))  %>%
    dplyr::mutate(
      .prob_cens = predict(model_fit$censor_probs, time = weight_time, as_vector = TRUE),
      .prob_cens = trunc_probs(.prob_cens, trunc),
      .weight_cens = 1 / .prob_cens,
      .weight_cens = add_wt_class(.weight_cens, "survival_weights_graf")
    )  %>%
    dplyr::select(.row, eval_time, .prob_cens, .weight_cens)
}

add_row_index <- function(dat, rows = NULL) {
  if (is.null(rows)) {
    dat <- parsnip::add_rowindex(dat)
  } else {
    m <- length(rows)
    n <- nrow(dat)
    if (m != n) {
      rlang::abort(
        glue::glue(
          "The length of 'rows' ({m}) should be equal to the number of rows in 'data' ({n})"
        )
      )
    }
    dat$.row <- rows
  }
  dat
}

# ------------------------------------------------------------------------------
# ROC curve helpers. Section 4.3 of Blanche, P., Dartigues, J.-F. and
# Jacqmin-Gadda, H. (2013), Review and comparison of ROC curve estimators for a
# time-dependent outcome with marker-dependent censoring. Biom. J., 55: 687-704.
# is a good starting place to understand these calculations.

# Note that these weights are independent of the time of analysis. We keep the
# same API as the Brier function for API consistency.

# TODO rename .survival_weights_roc

#' @export
#' @keywords internal
#' @rdname survival_weights
.survival_weights_roc <- function(data, truth,
                                  eval_time, model_fit,
                                  rows = NULL,
                                  predictors = NULL,
                                  trunc = 0.05, eps = 10^-10) {
  if (!is.null(predictors)) {
    rlang::warn("The 'predictors' argument to the survival weighting function is not currently used.")
  }
  eval_time <- filter_eval_time(eval_time)
  eval_time_df <- tibble::tibble(eval_time = eval_time)

  truth <- rlang::enquo(truth)
  surv_data <- collect_surv_col(data, truth)
  .check_censored_right(surv_data$surv)

  surv_data <-
    surv_data %>%
    dplyr::mutate(
      weight_time = .extract_time(surv) - eps,
      .prob_cens = predict(model_fit$censor_probs, time = weight_time, as_vector = TRUE),
      .prob_cens = trunc_probs(.prob_cens, trunc),
      .weight_cens = 1 / .prob_cens,
      .weight_cens = add_wt_class(.weight_cens, "survival_weights_roc")
    )

  surv_data <- add_row_index(surv_data, rows)

  surv_data <-
    surv_data %>%
    dplyr::select(.row, .prob_cens, .weight_cens)

  tidyr::crossing(surv_data, eval_time_df) %>%
    dplyr::select(.row, eval_time, .prob_cens, .weight_cens)
}


# add a helper function to use in predict_model() (at the end) to compute the weights
