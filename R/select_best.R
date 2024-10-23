#' Investigate best tuning parameters
#'
#' @description
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
#' @param ... For [select_by_one_std_err()] and [select_by_pct_loss()], this
#' argument is passed directly to [dplyr::arrange()] so that the user can sort
#' the models from *most simple to most complex*. That is, for a parameter `p`,
#' pass the unquoted expression `p` if smaller values of `p` indicate a simpler
#' model, or `desc(p)` if larger values indicate a simpler model. At
#' least one term is required for these two functions. See the examples below.
#' @param metric A character value for the metric that will be used to sort
#'  the models. (See
#'  \url{https://yardstick.tidymodels.org/articles/metric-types.html} for
#'  more details). Not required if a single metric exists in `x`. If there are
#'  multiple metric and none are given, the first in the metric set is used (and
#'  a warning is issued).
#' @param n An integer for the number of top results/rows to return.
#' @param limit The limit of loss of performance that is acceptable (in percent
#' units). See details below.
#' @param eval_time A single numeric time point where dynamic event time
#' metrics should be chosen (e.g., the time-dependent ROC curve, etc). The
#' values should be consistent with the values used to create `x`. The `NULL`
#' default will automatically use the first evaluation time used by `x`.
#' @param call The call to be shown in errors and warnings.
#' @return A tibble with columns for the parameters. [show_best()] also
#'  includes columns for performance metrics.
#' @details
#' For percent loss, suppose the best model has an RMSE of 0.75 and a simpler
#' model has an RMSE of 1. The percent loss would be `(1.00 - 0.75)/1.00 * 100`,
#' or 25 percent. Note that loss will always be non-negative.
#' @references
#' Breiman, Leo; Friedman, J. H.; Olshen, R. A.; Stone, C. J. (1984).
#' _Classification and Regression Trees._ Monterey, CA: Wadsworth.
#' @examplesIf tune:::should_run_examples()
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
#' select_by_pct_loss(
#'   ames_grid_search,
#'   metric = "rmse",
#'   limit = 5, desc(K)
#' )
#' @export
show_best <- function(x, ...) {
  UseMethod("show_best")
}

#' @export
#' @rdname show_best
show_best.default <- function(x, ...) {
  cli::cli_abort("No {.fn show_best} exists for this type of object.")
}

#' @export
#' @rdname show_best
show_best.tune_results <- function(x,
                                   ...,
                                   metric = NULL,
                                   eval_time = NULL,
                                   n = 5,
                                   call = rlang::current_env()) {
  rlang::check_dots_empty()

  metric_info <- choose_metric(x, metric, call = call)
  metric <- metric_info$metric

  eval_time <- choose_eval_time(x, metric, eval_time = eval_time, call = call)

  summary_res <- .filter_perf_metrics(x, metric, eval_time)

  if (metric_info$direction == "maximize") {
    summary_res <- summary_res %>% dplyr::arrange(dplyr::desc(mean))
  } else if (metric_info$direction == "minimize") {
    summary_res <- summary_res %>% dplyr::arrange(mean)
  } else if (metric_info$direction == "zero") {
    summary_res <- summary_res %>% dplyr::arrange(abs(mean))
  }
  show_ind <- 1:min(nrow(summary_res), n)
  summary_res %>%
    dplyr::slice(show_ind)
}


#' @export
#' @rdname show_best
select_best <- function(x, ...) {
  UseMethod("select_best")
}

#' @export
#' @rdname show_best
select_best.default <- function(x, ...) {
  cli::cli_abort("No {.fn select_best} exists for this type of object.")
}

#' @export
#' @rdname show_best
select_best.tune_results <- function(x, ..., metric = NULL, eval_time = NULL) {
  rlang::check_dots_empty()

  metric_info <- choose_metric(x, metric)
  metric <- metric_info$metric

  param_names <- .get_tune_parameter_names(x)

  res <- show_best(x, metric = metric, n = 1, eval_time = eval_time, call = rlang::current_env())
  res %>% dplyr::select(dplyr::all_of(param_names), .config)

}

#' @export
#' @rdname show_best
select_by_pct_loss <- function(x, ...) {
  UseMethod("select_by_pct_loss")
}

#' @export
#' @rdname show_best
select_by_pct_loss.default <- function(x, ...) {
  cli::cli_abort("No {.fn select_by_pct_loss} exists for this type of object.")
}

#' @export
#' @rdname show_best
select_by_pct_loss.tune_results <- function(x, ..., metric = NULL, eval_time = NULL, limit = 2) {
  metric_info <- choose_metric(x, metric)
  metric <- metric_info$metric

  param_names <- .get_tune_parameter_names(x)

  check_select_dots(...)

  eval_time <- choose_eval_time(x, metric, eval_time = eval_time)

  summary_res <- .filter_perf_metrics(x, metric, eval_time)

  if (metric_info$direction == "maximize") {
    best_metric <- max(summary_res$mean, na.rm = TRUE)
  } else if (metric_info$direction == "minimize") {
    best_metric <- min(summary_res$mean, na.rm = TRUE)
  } else if (metric_info$direction == "zero") {
    which_min <- which.min(abs(summary_res$mean))
    best_metric <- summary_res$mean[which_min]
  }

  summary_res <-
    summary_res %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      .best = best_metric,
      # can calculate loss without knowledge of direction since
      # we know that losses must be larger than that for the best
      .loss = abs((abs(mean) - abs(.best)) / .best) * 100
    ) %>%
    dplyr::ungroup()


  dots <- rlang::enquos(...)
  summary_res <- try(dplyr::arrange(summary_res, !!!dots), silent = TRUE)
  if (inherits(summary_res, "try-error")) {
    var_nm <- rlang::eval_tidy(dots)
    var_nm <- purrr::map_chr(var_nm, ~ rlang::quo_name(.x))
    var_nm <- var_nm[!var_nm %in% colnames(collect_metrics(x))]
    cli::cli_abort("Could not sort results by {.var {var_nm}}.")
  }

  # discard models more complex than the best and
  # remove models with greater increase in loss than the limit
  best_index <- which(summary_res$.loss == 0)
  summary_res %>%
    dplyr::slice(1:best_index) %>%
    dplyr::filter(.loss < limit) %>%
    dplyr::slice(1) %>%
    dplyr::select(dplyr::all_of(param_names), .config)
}

#' @export
#' @rdname show_best
select_by_one_std_err <- function(x, ...) {
  UseMethod("select_by_one_std_err")
}

#' @export
#' @rdname show_best
select_by_one_std_err.default <- function(x, ...) {
  cli::cli_abort("No {.fn select_by_one_std_err} exists for this type of object.")
}

#' @export
#' @rdname show_best
select_by_one_std_err.tune_results <- function(x, ..., metric = NULL, eval_time = NULL) {
  metric_info <- choose_metric(x, metric)
  metric <- metric_info$metric

  param_names <- .get_tune_parameter_names(x)

  check_select_dots(...)

  eval_time <- choose_eval_time(x, metric, eval_time = eval_time)

  summary_res <- .filter_perf_metrics(x, metric, eval_time)

  if (metric_info$direction == "maximize") {
    best_index <- which.max(summary_res$mean)
    best <- summary_res$mean[best_index]
    bound <- best - summary_res$std_err[best_index]
    summary_res <-
      summary_res %>%
      dplyr::mutate(
        .best = best,
        .bound = bound
      ) %>%
      dplyr::filter(mean >= .bound)
  } else if (metric_info$direction == "minimize") {
    best_index <- which.min(summary_res$mean)
    best <- summary_res$mean[best_index]
    bound <- best + summary_res$std_err[best_index]
    summary_res <-
      summary_res %>%
      dplyr::mutate(
        .best = best,
        .bound = bound
      ) %>%
      dplyr::filter(mean <= .bound)
  } else if (metric_info$direction == "zero") {
    best_index <- which.min(abs(summary_res$mean))
    best <- summary_res$mean[best_index]
    bound_lower <- -abs(best) - summary_res$std_err[best_index]
    bound_upper <- abs(best) + summary_res$std_err[best_index]
    summary_res <-
      summary_res %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        .best = best,
        .bound = list(c(lower = bound_lower, upper = bound_upper))
      ) %>%
      dplyr::filter(mean >= .bound[[1]] & mean <= .bound[[2]]) %>%
      dplyr::ungroup()
  }

  dots <- rlang::enquos(...)
  summary_res <- try(dplyr::arrange(summary_res, !!!dots), silent = TRUE)
  if (inherits(summary_res, "try-error")) {
    var_nm <- rlang::eval_tidy(dots)
    var_nm <- purrr::map_chr(var_nm, ~ rlang::quo_name(.x))
    var_nm <- var_nm[!var_nm %in% colnames(collect_metrics(x))]
    cli::cli_abort("Could not sort results by {.var {var_nm}}.")
  }
  summary_res %>%
    dplyr::slice(1) %>%
    dplyr::select(dplyr::all_of(param_names), .config)
}

check_select_dots <- function(..., call = rlang::caller_env()) {
  dots <- rlang::enquos(...)
  if (length(dots) == 0) {
    cli::cli_abort("Please choose at least one tuning parameter to sort in {.code ...}.",
                   call = call)
  }
  invisible(NULL)
}
