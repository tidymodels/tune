#' @section Censored Regression Models:
#'
#' Three types of metrics can be used to assess the quality of censored
#' regression models:
#'
#' - static: the prediction is independent of time.
#' - dynamic: the prediction is a time-specific probability (e.g., survival
#'   probability) and is measured at one or more particular times.
#' - integrated: same as the dynamic metric but returns the integral of the
#'   different metrics from each time point.
#'
#' Which metrics are chosen by the user affects how many evaluation times
#' should be specified. For example:
#'
#' ```
#' # Needs no `eval_time` value
#' metric_set(concordance_survival)
#'
#' # Needs at least one `eval_time`
#' metric_set(brier_survival)
#' metric_set(brier_survival, concordance_survival)
#'
#' # Needs at least two eval_time` values
#' metric_set(brier_survival_integrated, concordance_survival)
#' metric_set(brier_survival_integrated, concordance_survival)
#' metric_set(brier_survival_integrated, concordance_survival, brier_survival)
#' ```
#'
#' Values of `eval_time` should be less than the largest observed event
#' time in the training data. For many non-parametric models, the results beyond
#' the largest time corresponding to an event are constant (or `NA`).
#'
