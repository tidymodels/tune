#' Investigate best tuning parameters
#'
#' [show_best()] displays the top sub-models and their performance estimates.
#'
#' [select_best()] finds the tuning parameter combination with the best
#' performance values.
#'
#' [select_by_one_std_err()] uses the "one-standard error rule" (Breiman _el
#'  at, 1984) that selects the most simple model that is within one standard
#'  error of the numerically optimal results.
#'
#' [select_by_pct_loss()] selects the most simple model whose loss of
#'  performance is within some acceptable limit.
#'
#' @param x The results of [tune_grid()] or [tune_bayes()].
#' @param metric A character value for the metric that will be used to sort
#'  the models. (See
#'  \url{https://yardstick.tidymodels.org/articles/metric-types.html} for
#'  more details). Not required if a single metric exists in `x`. If there are
#'  multiple metric and none are given, the first in the metric set is used (and
#'  a warning is issued).
#' @param n An integer for the number of top results/rows to return.
#' @param limit The limit of loss of performance that is acceptable (in percent
#' units). See details below.
#' @param ... For [select_by_one_std_err()] and [select_by_pct_loss()], this
#' argument is passed directly to [dplyr::arrange()] so that the user can sort
#' the models from *most simple to most complex*. See the examples below. At
#' least one term is required for these two functions.
#' @return A tibble with columns for the parameters. [show_best()] also
#'  includes columns for performance metrics.
#' @details
#' For percent loss, suppose the best model has an RMSE of 0.75 and a simpler
#' model has an RMSE of 1. The percent loss would be `(1.00 - 0.75)/1.00 * 100`,
#' or 25 percent. Note that loss will always be non-negative.
#' @references
#' Breiman, Leo; Friedman, J. H.; Olshen, R. A.; Stone, C. J. (1984).
#' _Classification and Regression Trees._ Monterey, CA: Wadsworth.
#' @examples
#' \donttest{
#' data("example_ames_knn")
#'
#' show_best(ames_iter_search, metric = "rmse")
#'
#' select_best(ames_iter_search, metric = "rsq")
#'
#' # To find the least complex model within one std error of the numerically
#' # optimal model, the number of nearest neighbors are sorted from the largest
#' # number of neighbors (the least complex class boundary) to the smallest
#' # (corresponding to the most complex model).
#'
#' select_by_one_std_err(ames_grid_search, metric = "rmse", desc(K))
#'
#' # Now find the least complex model that has no more than a 5% loss of RMSE:
#' select_by_pct_loss(ames_grid_search, metric = "rmse",
#'                    limit = 5, desc(K))
#' }
#' @export
show_best <- function(x, metric = NULL, n = 5, ...) {
  is_a_race <- inherits(x, "tune_race")
  if (is_a_race) {
    x <- dplyr::select(x, -.order)
  }
  metric <- choose_metric(metric, x)
  dots <- rlang::enquos(...)
  if (!is.null(dots$maximize)) {
    rlang::warn(paste("The `maximize` argument is no longer needed.",
                      "This value was ignored."))
  }
  maximize <- is_metric_maximize(x, metric)
  summary_res <- estimate_tune_results(x)
  if (is_a_race) {
    summary_res <- dplyr::filter(summary_res, n == nrow(x))
  }
  metrics <- unique(summary_res$.metric)
  if (length(metrics) == 1) {
    metric <- metrics
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
  show_ind <- 1:min(nrow(summary_res), n)
  summary_res %>% dplyr::slice(show_ind)
}

choose_metric <- function(metric, x) {
  if (is.null(metric)) {
    metric_vals <- .get_tune_metric_names(x)
    metric <- metric_vals[1]
    if (length(metric_vals) > 1) {
      msg <- paste0("No value of `metric` was given; metric '", metric, "' ",
                    "will be used.")
      rlang::warn(msg)
    }
  }
  metric
}


#' @export
#' @rdname show_best
select_best <- function(x, metric = NULL, ...) {
  metric <- choose_metric(metric, x)
  dots <- rlang::enquos(...)
  if (!is.null(dots$maximize)) {
    rlang::warn(paste("The `maximize` argument is no longer needed.",
                      "This value was ignored."))
  }
  res <- show_best(x, metric = metric, n = 1)
  res <- res %>% dplyr::select(-mean, -n, -.metric, -.estimator, -std_err)
  if (any(names(res) == ".iter")) {
    res <- res %>% dplyr::select(-.iter)
  }
  res
}

#' @export
#' @rdname show_best
select_by_pct_loss <- function(x, ..., metric = NULL, limit = 2) {
  is_a_race <- inherits(x, "tune_race")
  if (is_a_race) {
    x <- dplyr::select(x, -.order)
  }
  metric <- choose_metric(metric, x)
  dots <- rlang::enquos(...)
  if (!is.null(dots$maximize)) {
    rlang::warn(paste("The `maximize` argument is no longer needed.",
                      "This value was ignored."))
    dots[["maximize"]] <- NULL
  }

  if (length(dots) == 0) {
    rlang::abort("Please choose at least one tuning parameter to sort in `...`.")
  }
  maximize <- is_metric_maximize(x, metric)
  res <-
    collect_metrics(x) %>%
    dplyr::filter(.metric == !!metric & !is.na(mean))

  if (is_a_race) {
    res <- dplyr::filter(res, n == nrow(x))
  }

  if (nrow(res) == 0) {
    rlang::abort("No results are available. Please check the value of `metric`.")
  }
  if (maximize) {
    best_metric <- max(res$mean, na.rm = TRUE)
    res <-
      res %>%
      dplyr::mutate(.best = best_metric,
                    .loss = (best_metric - mean)/best_metric * 100)
  } else {
    best_metric <- min(res$mean, na.rm = TRUE)
    res <-
      res %>%
      dplyr::mutate(.best = best_metric,
                    .loss = (mean - best_metric)/best_metric * 100)
  }

  res <- try(dplyr::arrange(res, !!!dots), silent = TRUE)
  if (inherits(res, "try-error")) {
    var_nm <- rlang::eval_tidy(dots)
    var_nm <- purrr::map_chr(var_nm, ~ as.character(rlang::quo_get_expr(.x)))
    msg <- paste0("Could not sort results by '", var_nm, "'.")
    rlang::abort(msg)
  }

  # discard models more complex than the best then rank by loss
  best_index <- which(res$.loss == 0)
  res %>%
    dplyr::slice(1:best_index) %>%
    dplyr::filter(.loss < limit) %>%
    dplyr::arrange(desc(.loss)) %>%
    dplyr::slice(1)
}

#' @export
#' @rdname show_best
select_by_one_std_err <- function(x, ..., metric = NULL) {
  is_a_race <- inherits(x, "tune_race")
  if (is_a_race) {
    x <- dplyr::select(x, -.order)
  }
  metric <- choose_metric(metric, x)
  dots <- rlang::enquos(...)
  if (!is.null(dots$maximize)) {
    rlang::warn(paste("The `maximize` argument is no longer needed.",
                      "This value was ignored."))
    dots[["maximize"]] <- NULL
  }
  if (length(dots) == 0) {
    rlang::abort("Please choose at least one tuning parameter to sort in `...`.")
  }
  maximize <- is_metric_maximize(x, metric)
  res <-
    collect_metrics(x) %>%
    dplyr::filter(.metric == !!metric & !is.na(mean))

  if (is_a_race) {
    res <- dplyr::filter(res, n == nrow(x))
  }

  if (nrow(res) == 0) {
    rlang::abort("No results are available. Please check the value of `metric`.")
  }

  if (maximize) {
    best_index <- which.max(res$mean)
    best <- res$mean[best_index]
    bound <- best - res$std_err[best_index]
    res <-
      res %>%
      dplyr::mutate(.best = best,
                    .bound = bound) %>%
      dplyr::filter(mean >= .bound)
  } else {
    best_index <- which.min(res$mean)
    best <- res$mean[best_index]
    bound <- best + res$std_err[best_index]
    res <-
      res %>%
      dplyr::mutate(.best = best,
                    .bound = bound) %>%
      dplyr::filter(mean <= .bound)
  }

  res <- try(dplyr::arrange(res, !!!dots), silent = TRUE)
  if (inherits(res, "try-error")) {
    var_nm <- rlang::eval_tidy(dots)
    var_nm <- purrr::map_chr(var_nm, ~ as.character(rlang::quo_get_expr(.x)))
    msg <- paste0("Could not sort results by '", var_nm, "'.")
    rlang::abort(msg)
  }
  res %>% dplyr::slice(1)
}

is_metric_maximize <- function(x, metric) {
  if (rlang::is_missing(metric) | length(metric) > 1) {
    rlang::abort("Please specify a single character value for `metric`.")
  }
  attr_x <- attr(x, "metrics") %>%
    attr("metrics")
  if (!metric %in% names(attr_x)) {
    rlang::abort("Please check the value of `metric`.")
  }
  directions <-
    attr(x, "metrics") %>%
    attr("metrics") %>%
    purrr::map(~ attr(.x, "direction"))

  directions[[metric]] == "maximize"
}
