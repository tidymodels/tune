#' Obtain the numerically best parameters
#'
#' `select_best()` finds the tuning parameter combination with the best
#'  performance values. `show_best()` displays the top sub-models and their
#'  performance estimates.
#'
#' @param x The results of `tune_grid()` or `tune_bayes()`.
#' @param metric A character value for the metric that will be used to sort
#'  the models. (See
#'  \url{https://tidymodels.github.io/yardstick/articles/metric-types.html} for
#'  more details). Not required if a single metric exists in `x`.
#' @param n_top An integer for the number of top results/rows to return.
#' @param maximize A logical value (TRUE/FALSE).
#' @return A tibble with columns for the parameters. `show_best()` also
#'  includes columns for performance metrics.
#' @export
show_best <- function(x, metric, n_top = 5, maximize = TRUE) {
  summary_res <- estimate_tune_results(x)
  metrics <- unique(summary_res$.metric)
  if (length(metrics) == 1) {
    metric <- metrics
  }

  if (rlang::is_missing(metric) | length(metric) > 1) {
    rlang::abort("Please specify a single character value for `metric`.")
  }
  if (!is.logical(maximize) | length(maximize) > 1) {
    rlang::abort("Please specify a single logical value for `maximize`.")
  }

  # trap some cases that we know about
  to_min <- c("rmse", "mae", "mase", "mape")
  if (maximize & metric %in% to_min) {
    msg <- paste0("Did you mean to maximize ", metric, "?")
    rlang::warn(msg)
  }

  # get estimates/summarise
  summary_res <- summary_res %>% dplyr::filter(.metric == metric)

  if (nrow(summary_res) == 0) {
    rlang::abort("No results are available. Please check the value of `metric`.")
  }

  if (maximize) {
    summary_res <- summary_res %>% dplyr::arrange(dplyr::desc(mean))
  } else {
    summary_res <- summary_res %>% dplyr::arrange(mean)
  }
  show_ind <- 1:min(nrow(summary_res), n_top)
  summary_res %>% dplyr::slice(show_ind)
}


#' @export
#' @rdname show_best
select_best <- function(x, metric, maximize = TRUE) {
  res <- show_best(x, metric = metric, maximize = maximize, n_top = 1)
  res <- res %>% dplyr::select(-mean, -n, -.metric, -.estimator, -std_err)
  if (any(names(res) == ".iter")) {
    res <- res %>% dplyr::select(-.iter)
  }
  res
}


