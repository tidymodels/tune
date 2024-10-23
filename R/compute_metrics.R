#' Calculate and format metrics from tuning functions
#'
#' @description
#'
#' This function computes metrics from tuning results. The arguments and
#' output formats are closely related to those from [collect_metrics()], but
#' this function additionally takes a `metrics` argument with a
#' [metric set][yardstick::metric_set()] for new metrics to compute. This
#' allows for computing new performance metrics without requiring users to
#' re-evaluate models against resamples.
#'
#' Note that the [control option][control_grid()] `save_pred = TRUE` must
#' have been supplied when generating `x`.
#'
#' @param x The results of a tuning function like [tune_grid()] or
#' [fit_resamples()], generated with the control option `save_pred = TRUE`.
#' @param metrics A [metric set][yardstick::metric_set()] of new metrics
#' to compute. See the "Details" section below for more information.
#' @param summarize A single logical value indicating whether metrics should
#' be summarized over resamples (`TRUE`) or return the values for each
#' individual resample. See [collect_metrics()] for more details on how
#' metrics are summarized.
#' @inheritParams control_grid
#' @inheritParams collect_metrics
#'
#' @return A tibble. See [collect_metrics()] for more details on the return value.
#'
#' @details
#'
#' Each metric in the set supplied to the `metrics` argument must have a metric
#' type (usually `"numeric"`, `"class"`, or `"prob"`) that matches some metric
#' evaluated when generating `x`. e.g. For example, if `x` was generated with
#' only hard `"class"` metrics, this function can't compute metrics that take in
#' class probabilities (`"prob"`.) By default, the tuning functions used to
#' generate `x` compute metrics of all needed types.
#'
#' @examplesIf tune:::should_run_examples()
#' # load needed packages:
#' library(parsnip)
#' library(rsample)
#' library(yardstick)
#'
#' # evaluate a linear regression against resamples.
#' # note that we pass `save_pred = TRUE`:
#' res <-
#'   fit_resamples(
#'     linear_reg(),
#'     mpg ~ cyl + hp,
#'     bootstraps(mtcars, 5),
#'     control = control_grid(save_pred = TRUE)
#'   )
#'
#' # to return the metrics supplied to `fit_resamples()`:
#' collect_metrics(res)
#'
#' # to compute new metrics:
#' compute_metrics(res, metric_set(mae))
#'
#' # if `metrics` is the same as that passed to `fit_resamples()`,
#' # then `collect_metrics()` and `compute_metrics()` give the same
#' # output, though `compute_metrics()` is quite a bit slower:
#' all.equal(
#'   collect_metrics(res),
#'   compute_metrics(res, metric_set(rmse, rsq))
#' )
#' @name compute_metrics
#' @export
compute_metrics <- function(x, metrics, summarize, event_level, ...) {
  UseMethod("compute_metrics")
}

#' @export
#' @rdname compute_metrics
compute_metrics.default <- function(x,
                                    metrics,
                                    summarize = TRUE,
                                    event_level = "first",
                                    ...) {
  cli::cli_abort("No {.fn compute_metrics} method exists for
                 {.obj_type_friendly {x}}.")
}

#' @export
#' @rdname compute_metrics
compute_metrics.tune_results <- function(x,
                                         metrics,
                                         ...,
                                         summarize = TRUE,
                                         event_level = "first") {
  rlang::check_dots_empty()
  check_bool(summarize)
  if (!".predictions" %in% names(x)) {
    cli::cli_abort(
      "{.arg x} must have been generated with the control argument
      {.code save_pred = TRUE}."
    )
  }

  if (!inherits(metrics, "metric_set")) {
    cli::cli_abort("{.arg metrics} must be a metric set.")
  }

  new_metrics_info <- metrics_info(metrics)
  old_metrics_info <- metrics_info(attr(x, "metrics"))

  # if only a class metric is supplied to a tuning function, then
  # `save_pred = TRUE` will only return the class predictions from
  # each model. same story goes for prob metrics.
  if (!all(new_metrics_info$type %in% old_metrics_info$type)) {
    cli::cli_abort(c(
      "The supplied `metrics` argument has metrics of type \\
       {.val {new_metrics_info$type}}, while the metrics used to generate \\
       predictions only used {.val {old_metrics_info$type}} metrics.",
      "i" = "To save predictions for {new_metrics_info$type} metrics, \\
             generate {.arg x} with metrics of that type."
    ))
  }

  param_names <- .get_tune_parameter_names(x)
  outcome_name <- .get_tune_outcome_names(x)

  preds <- x$.predictions
  preds <- purrr::map2(preds, x$id, mutate_id)
  preds <- purrr::list_rbind(preds)
  preds <- dplyr::group_by(preds, id, .config)

  # call `.estimate_metrics` with additional groupings
  mtrcs <-
    .estimate_metrics(
      preds,
      metric = metrics,
      param_names = c(param_names, "id", ".config"),
      outcome_name = outcome_name,
      event_level = event_level,
      metrics_info = new_metrics_info
    )

  # re-order so `.config` comes last
  mtrcs <- mtrcs[c(setdiff(names(mtrcs), ".config"), ".config")]

  # nest by resample id
  nest_cols <- "id"

  if ("Iter1" %in% mtrcs$.config) {
    mtrcs$.iter <- .config_to_.iter(mtrcs$.config)

    nest_cols <- c(nest_cols, ".iter")
  }

  mtrcs <- nest(mtrcs, .by = all_of(nest_cols), .key = ".metrics")
  match_locations <- vec_locate_matches(x[nest_cols], mtrcs[nest_cols])
  x$.metrics <- vec_slice(mtrcs$.metrics, match_locations$haystack)

  attr(x, "metrics") <- metrics

  collect_metrics(x = x, summarize = summarize, ...)
}

mutate_id <- function(tbl, id) {
  tbl$id <- id
  tbl
}
