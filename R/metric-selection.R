#' Tools for selecting metrics and evaluation times
#'
#' @param mtr_set A [yardstick::metric_set()].
#' @param metric A character value for which metric is being used.
#' @param eval_time An optional vector of times to compute dynamic and/or
#' integrated metrics.
#' @keywords internal
#' @export
first_metric <- function(mtr_set) {
  tibble::as_tibble(mtr_set)[1,]
}

#' @rdname first_metric
#' @keywords internal
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
  if (!any(grepl("_survival_", mtr_info$class))) {
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
