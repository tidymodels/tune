#' Augment data with holdout predictions
#'
#' For `tune` objects that use resampling, these `augment()` methods will add
#' one or more columns for the hold-out predictions (i.e. from the assessment
#' set(s)).
#'
#' @param x An object resulting from one of the `tune_*()` functions,
#'  `fit_resamples()`, or `last_fit()`. The control specifications for these
#'  objects should have used the option `save_pred = TRUE`.
#' @param parameters A data frame with a single row that indicates what
#' tuning parameters should be used to generate the predictions (for `tune_*()`
#' objects only). If `NULL`, `select_best(x)` will be used.
#' @param ... Not currently used.
#' @return A data frame with one or more additional columns for model
#' predictions.
#'
#' @details
#' For some resampling methods where rows may be replicated in multiple
#' assessment sets, the prediction columns will be averages of the holdout
#' results. Also, for these methods, it is possible that all rows of the
#' original data do not have holdout predictions (like a single bootstrap
#' resample). In this case, all rows are return and a warning is issued.
#'
#' For objects created by `last_fit()`, the test set data and predictions are
#' returned.
#'
#' Unlike other `augment()` methods, the predicted values for regression models
#' are in a column called `.pred` instead of `.fitted` (to be consistent with
#' other tidymodels conventions).
#'
#' For regression problems, an additional `.resid` column is added to the
#' results.
#'
#' @export
augment.tune_results <- function(x, parameters = NULL, ...) {
  dots <- rlang::list2(...)
  if (length(dots) > 0) {
    rlang::abort(
      paste("The only two arguments for `augment.tune_results()` are",
            "'x' and 'parameters'. Others were passed:",
            paste0("'", names(dots), "'", collapse = ", "))
    )
  }

  # check/determine best settings
  if (is.null(parameters)) {
    obj_fun <- .get_tune_metric_names(x)[1]
    parameters <- select_best(x, metric = obj_fun)
  } else {
    if (!is.data.frame(parameters) || nrow(parameters) > 1) {
      rlang::abort("'parameters' should be a single row data frame")
    }
  }

  pred <- collect_predictions(x, summarize = TRUE, parameters = parameters)
  y_nm <- .get_tune_outcome_names(x)
  merge_pred(x$splits[[1]]$data, pred, y_nm)
}

#' @rdname augment.tune_results
#' @export
augment.resample_results <- function(x, ...) {
  dots <- rlang::list2(...)
  if (length(dots) > 0) {
    rlang::abort(
      paste("The only argument for `augment.fit_resamples()` is",
            "'x'. Others were passed:",
            paste0("'", names(dots), "'", collapse = ", "))
    )
  }

  pred <- collect_predictions(x, summarize = TRUE)
  y_nm <- .get_tune_outcome_names(x)
  merge_pred(x$splits[[1]]$data, pred, y_nm)
}



#' @rdname augment.tune_results
#' @export
augment.last_fit <- function(x, ...) {
  dots <- rlang::list2(...)
  if (length(dots) > 0) {
    rlang::abort(
      paste("The only argument for `augment.last_fit()` is",
            "'x'. Others were passed:",
            paste0("'", names(dots), "'", collapse = ", "))
    )
  }

  pred <- collect_predictions(x, summarize = TRUE)
  pred$.row <- 1:nrow(pred)
  y_nm <- .get_tune_outcome_names(x)
  merge_pred(rsample::assessment(x$splits[[1]]), pred, y_nm)
}

merge_pred <- function(dat, pred, y) {
  pred_cols <- grep("^\\.pred", names(pred), value = TRUE)
  pred <- pred[order(pred$.row),]
  pred <- pred[, c(".row", pred_cols)]
  if (nrow(pred) != nrow(dat)) {
    rlang::warn(
      paste("The orginal data had", nrow(dat), "rows but there were",
            nrow(pred), "hold-out predictions.")
    )
  }
  dat$.row <- 1:nrow(dat)
  dat <- dplyr::left_join(dat, pred, by = ".row")
  dat$.row <- NULL
  if (all(pred_cols == ".pred")) {
    dat$.resid <- dat[[y]] - dat$.pred
  }
  dat
}
