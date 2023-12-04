#' Tools for selecting metrics and evaluation times
#'
#' @param mtr_set A [yardstick::metric_set()].
#' @param metric A character value for which metric is being used.
#' @param eval_time An optional vector of times to compute dynamic and/or
#' integrated metrics.
#' @description
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
#' @keywords internal
#' @export
choose_metric <- function(x, metric) {
  mtr_set <- .get_tune_metrics(x)
  mtr_info <- tibble::as_tibble(mtr_set)

  if (is.null(metric)) {
    metric <- mtr_info$metric[1]
    cli::cli_warn("No value of `metric` was given; '{metric}' will be used")
  } else {
    metric <- check_mult_metrics(metric)
    check_right_metric(mtr_info, metric)
  }

  mtr_info[mtr_info$metric == metric,]
}

check_mult_metrics <- function(metric) {
  num_metrics <- length(metric)
  metric <- metric[1]
  if (num_metrics > 1) {
    cli::cli_warn("{num_metrics} metric{?s} were given; '{metric}' will be used")
  }
  metric
}

check_right_metric <- function(mtr_info, metric) {
  if (!any(mtr_info$metric == metric)) {
    met_list <- paste0("'", mtr_info$metric, "'", collapse = ", ")
    cli::cli_abort("'{metric}' was not in the metric set. Please choose from: {met_list}")
  }
  invisible(NULL)
}

is_survival_metric <- function(mtr_info) {
  any(grepl("_survival_", mtr_info$class))
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
  if (!is_survival_metric(mtr_info)) {
    return(NULL)
  }

  # Not a metric that requires an eval_time
  no_time_req <- c("static_survival_metric", "integrated_survival_metric")
  if (mtr_info$class %in% no_time_req) {
    if (num_times > 0) {
      cli::cli_warn("Evaluation times are only required when dynmanic or integrated metrics are selected as the primary metric.")
    }
    return(NULL)
  }

  # checks for dynamic metrics
  if (num_times == 0) {
    cli::cli_abort("A single evaluation time is required to use this metric.")
  } else if ( num_times > 1 ) {
    eval_time <- eval_time[1]
    print_time <- format(eval_time, digits = 3)
    cli::cli_warn("{num_times} evaluation times were available; the first ({print_time}) will be used.")
  }

  eval_time
}
