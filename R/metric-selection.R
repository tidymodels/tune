#' Tools for selecting metrics and evaluation times
#'
#' @param mtr_set A [yardstick::metric_set()].
#' @param metric A character value for which metric is being used.
#' @param eval_time An optional vector of times to compute dynamic and/or
#' integrated metrics.
#' @param wflow A [workflows::workflow()].
#' @param x An object with class `tune_results`.
#' @param call The call to be displayed in warnings or errors.
#' @details
#' These are developer-facing functions used to compute and validate choices
#' for performance metrics. For survival analysis models, there are similar
#' functions for the evaluation time(s) required for dynamic and/or integrated
#' metrics.
#'
#' `choose_metric()` is used with functions such as [show_best()] or
#' [select_best()] where a single valid metric is required to rank models. If
#' no value is given by the user, the first metric value is used (with a
#' warning).
#'
#' For evaluation times, one is only required when the metric type is dynamic
#' (e.g. [yardstick::brier_survival()] or [yardstick::roc_auc_survival()]). For
#' these metrics, we require a single numeric value that was originally given
#' to the function used to produce `x` (such as [tune_grid()]).
#'
#' If a time is required and none is given, the first value in the vector
#' originally given in the `eval_time` argument is used (with a warning).
#'
#' @keywords internal
#' @export
choose_metric <- function(x, metric, ..., call = rlang::caller_env()) {
  rlang::check_dots_empty()

  mtr_set <- .get_tune_metrics(x)
  mtr_info <- tibble::as_tibble(mtr_set)

  if (is.null(metric)) {
    metric <- mtr_info$metric[1]
    cli::cli_warn("No value of {.arg metric} was given; {.val {metric}}
                   will be used.",
                  call = call)
  } else {
    metric <- check_mult_metrics(metric, call = call)
    check_metric_in_tune_results(mtr_info, metric, call = call)
  }

  mtr_info[mtr_info$metric == metric,]
}

check_mult_metrics <- function(metric, ..., call = rlang::caller_env()) {
  rlang::check_dots_empty()

  num_metrics <- length(metric)
  metric <- metric[1]
  if (num_metrics > 1) {
    cli::cli_warn("{num_metrics} metric{?s} were given; {.val {metric}} will
                   be used.",
                  call = call)
  }
  metric
}

check_metric_in_tune_results <- function(mtr_info, metric, ..., call = rlang::caller_env()) {
  rlang::check_dots_empty()

  if (!any(mtr_info$metric == metric)) {
    cli::cli_abort("{.val {metric}} was not in the metric set. Please choose
                    from: {.val {mtr_info$metric}}.", call = call)
  }
  invisible(NULL)
}

contains_survival_metric <- function(mtr_info) {
  any(grepl("_survival", mtr_info$class))
}

#' @rdname choose_metric
#' @export
choose_eval_time <- function(x, metric, eval_time = NULL, ..., call = rlang::caller_env()) {
  rlang::check_dots_empty()

  mtr_set <- .get_tune_metrics(x)
  mtr_info <- tibble::as_tibble(mtr_set)

  if (!contains_survival_metric(mtr_info)) {
    if (!is.null(eval_time)) {
      cli::cli_warn("Evaluation times are only required when the model
                     mode is {.val censored regression} (and will be ignored).")
    }
    return(NULL)
  }

  # If we need an eval time, set it to the possible values so that
  # we can choose the first value
  if (is_dyn(mtr_set, metric) && is.null(eval_time)) {
    eval_time <- .get_tune_eval_times(x)
  }

  eval_time <- first_eval_time(mtr_set, metric = metric, eval_time = eval_time)

  check_eval_time_in_tune_results(x, eval_time, call = call)

  eval_time
}

is_dyn <- function(mtr_set, metric) {
  mtr_info <- tibble::as_tibble(mtr_set)
  mtr_cls <- mtr_info$class[mtr_info$metric == metric]
  mtr_cls  == "dynamic_survival_metric"
}

check_eval_time_in_tune_results <- function(x, eval_time, call = rlang::caller_env()) {
  given_times <- .get_tune_eval_times(x)
  if (!is.null(eval_time)) {
    if (!any(eval_time == given_times)) {
      print_time <- format(eval_time, digits = 3)
      cli::cli_abort("Evaluation time {print_time} is not in the results.",
                     call = call)
    }
  }
  invisible(NULL)
}

# ------------------------------------------------------------------------------

#' @rdname choose_metric
#' @export
first_metric <- function(mtr_set) {
  tibble::as_tibble(mtr_set)[1,]
}

#' @rdname choose_metric
#' @export
first_eval_time <- function(mtr_set, metric = NULL, eval_time = NULL) {
  num_times <- length(eval_time)

  if (is.null(metric)) {
    mtr_info <- first_metric(mtr_set)
    metric <- mtr_info$metric
  } else {
    mtr_info <- tibble::as_tibble(mtr_set)
    mtr_info <- mtr_info[mtr_info$metric == metric,]
  }

  # Not a survival metric
  if (!contains_survival_metric(mtr_info)) {
    return(NULL)
  }

  # Not a metric that requires an eval_time
  no_time_req <- c("static_survival_metric", "integrated_survival_metric")
  if (mtr_info$class %in% no_time_req) {
    if (num_times > 0) {
      cli::cli_warn("Evaluation times are only required when dynmanic or
                     integrated metrics are selected as the primary metric
                     (and will be ignored).")
    }
    return(NULL)
  }

  # checks for dynamic metrics
  if (num_times == 0) {
    cli::cli_abort("A single evaluation time is required to use this metric.")
  } else if ( num_times > 1 ) {
    eval_time <- eval_time[1]
    print_time <- format(eval_time, digits = 3)
    cli::cli_warn("{.val {num_times}} evaluation times were specified during
                   tuning; the first ({print_time}) will be used.")
  }

  eval_time
}

# ------------------------------------------------------------------------------

#' @rdname choose_metric
#' @export
.filter_perf_metrics <- function(x, metric, eval_time) {
  summary_res <- estimate_tune_results(x)
  summary_res <- summary_res[summary_res$.metric == metric, ]
  is_missing_mean <- is.na(summary_res$mean)
  summary_res <- summary_res[!is_missing_mean, ]

  if (!is.null(eval_time) && any(colnames(summary_res) == ".eval_time")) {
    summary_res <- summary_res[summary_res$.eval_time == eval_time, ]
  }
  if (nrow(summary_res) == 0) {
    cli::cli_abort("No results are available. Please use {.fun collect_metrics}
                    to see if there were any issues.")
  }

  summary_res
}

# ------------------------------------------------------------------------------

#' @rdname choose_metric
#' @export
check_metrics_arg <- function(mtr_set, wflow, call = rlang::caller_env()) {
  mode <- extract_spec_parsnip(wflow)$mode

  if (is.null(mtr_set)) {
    switch(mode,
           regression = {
             mtr_set <- yardstick::metric_set(rmse, rsq)
           },
           classification = {
             mtr_set <- yardstick::metric_set(roc_auc, accuracy)
           },
           'censored regression' = {
             mtr_set <- yardstick::metric_set(brier_survival)
           },
           # workflows cannot be set with an unknown mode
           cli::cli_abort("Model value {.val {mode}} can't be used.", call = call)
    )

    return(mtr_set)
  }

  is_numeric_metric_set <- inherits(mtr_set, "numeric_metric_set")
  is_class_prob_metric_set <- inherits(mtr_set, "class_prob_metric_set")
  is_surv_metric_set <- inherits(mtr_set, c("survival_metric_set"))

  if (!is_numeric_metric_set && !is_class_prob_metric_set && !is_surv_metric_set) {
    cli::cli_abort("The {.val metrics} argument should be the results of
                   {.fn yardstick::metric_set}.", call = call)
  }

  if (mode == "regression" && !is_numeric_metric_set) {
    cli::cli_abort("The parsnip model has {.code mode} value of {.val {mode}},
                   but the {.code metrics} is a metric set for a
                   different model mode.", call = call)
  }

  if (mode == "classification" && !is_class_prob_metric_set) {
    cli::cli_abort("The parsnip model has {.code mode} value of {.val {mode}},
                   but the {.code metrics} is a metric set for a
                   different model mode.", call = call)
  }

  if (mode == "censored regression" && !is_surv_metric_set) {
    cli::cli_abort("The parsnip model has {.code mode} value of {.val {mode}},
                   but the {.code metrics} is a metric set for a
                   different model mode.", call = call)
  }

  mtr_set
}

# ------------------------------------------------------------------------------

#' @rdname choose_metric
#' @export
check_eval_time_arg <- function(eval_time, mtr_set, call = rlang::caller_env()) {
  mtr_info <- tibble::as_tibble(mtr_set)

  # Not a survival metric
  if (!contains_survival_metric(mtr_info)) {
    if (!is.null(eval_time)) {
      cli::cli_warn("Evaluation times are only required when the model
                     mode is {.val censored regression} (and will be ignored).")
    }
    return(NULL)
  }

  cls <- mtr_info$class
  uni_cls <- sort(unique(cls))
  eval_time <- .filter_eval_time(eval_time)

  num_times <- length(eval_time)

  max_times_req <- req_eval_times(mtr_set)

  if (max_times_req > num_times) {
    cli::cli_abort("At least {max_times_req} evaluation time{?s} {?is/are}
                   required for the metric type(s) requested: {.val {uni_cls}}.
                   Only {num_times} unique time{?s} {?was/were} given.",
                   call = call)
  }

  if (max_times_req == 0 & num_times > 0) {
    cli::cli_warn("Evaluation times are only required when dynmanic or
                   integrated metrics are used (and will be ignored here).")
    eval_time <- NULL
  }

  eval_time
}

req_eval_times <- function(mtr_set) {
  mtr_info <- tibble::as_tibble(mtr_set)
  cls <- mtr_info$class

  # Default for non-survival and static metrics
  max_req_times <- 0

  if (any(cls == "dynamic_survival_metric")) {
    max_req_times <- max(max_req_times, 1)
  }

  if (any(cls == "integrated_survival_metric")) {
    max_req_times <- max(max_req_times, 2)
  }

  max_req_times
}

# in parsnip
# nocov start
.filter_eval_time <- function(eval_time, fail = TRUE) {
  if (!is.null(eval_time)) {
    eval_time <- as.numeric(eval_time)
  }
  eval_time_0 <- eval_time
  # will still propagate nulls:
  eval_time <- eval_time[!is.na(eval_time)]
  eval_time <- eval_time[eval_time >= 0 & is.finite(eval_time)]
  eval_time <- unique(eval_time)
  if (fail && identical(eval_time, numeric(0))) {
    cli::cli_abort(
      "There were no usable evaluation times (finite, non-missing, and >= 0).",
      call = NULL
    )
  }
  if (!identical(eval_time, eval_time_0)) {
    diffs <- setdiff(eval_time_0, eval_time)
    cli::cli_warn("There {?was/were} {length(diffs)} inappropriate evaluation
                  time point{?s} that {?was/were} removed.", call = NULL)

  }
  eval_time
}
# nocov end

# TODO will be removed shortly

middle_eval_time <- function(x) {
  x <- x[!is.na(x)]
  times <- unique(x)
  med_time <- median(x, na.rm = TRUE)
  ind <- which.min(abs(times - med_time))
  eval_time <- times[ind]
  eval_time
}
