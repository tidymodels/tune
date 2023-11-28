# For iterative search and racing, what metric will be optimized?
#' @keywords internal
#' @export
first_metric <- function(mtr_set) {
  tibble::as_tibble(mtr_set)[1,]
}

# Did the user pass an improper metric (i.e. want rmse but not computed)?
#' @keywords internal
#' @export
check_chosen_metric <- function(metric, mtr_set) {
  mtr_info <- tibble::as_tibble(mtr_set)
  in_set <- any(mtr_info$metric == metric)
  if (!in_set) {
    cli::cli_abort("metric '{metric}' is not in the metric set.")
  }
  invisible(TRUE)
}

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

#' @keywords internal
#' @export
check_eval_time <- function(eval_time = NULL, all_times = NULL) {
  if (!is.null(eval_time)) {
    return(eval_time)
  }

  all_times <- sort(unique(all_times))
}
