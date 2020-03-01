#' Control aspects of the grid search process
#'
#' @inheritParams control_bayes
#' @param allow_par A logical to allow parallel processing (if a parallel
#'   backend is registered).
#'
#' @details
#'
#' For `extract`, this function can be used to output the model object, the
#' recipe (if used), or some components of either or both. When evaluated, the
#' function's sole argument has a fitted workflow If the formula method is used,
#' the recipe element will be `NULL`.
#'
#' The results of the `extract` function are added to a list column in the
#' output called `.extracts`. Each element of this list is a tibble with tuning
#' parameter column and a list column (also called `.extracts`) that contains
#' the results of the function. If no extraction function is used, there is no
#' `.extracts` column in the resulting object. See [tune_bayes()] for more
#' specific details.
#'
#' Note that for [collect_predictions()], it is possible that each row of the
#'  original data point might be represented multiple times per tuning
#'  parameter. For example, if the bootstrap or repeated cross-validation are
#'  used, there will be multiple rows since the sample data point has been
#'  evaluated multiple times. This may cause issues when merging the predictions
#'  with the original data.
#'
#' [control_resamples()] is an alias for [control_grid()] and is meant to be
#' used with [fit_resamples()].
#' @export
control_grid <- function(verbose = FALSE, allow_par = TRUE,
                         extract = NULL, save_pred = FALSE,
                         pkgs = NULL) {
  # add options for  seeds per resample

  val_class_and_single(verbose, "logical", "control_grid()")
  val_class_and_single(allow_par, "logical", "control_grid()")
  val_class_and_single(save_pred, "logical", "control_grid()")
  val_class_or_null(pkgs, "character", "control_grid()")
  val_class_or_null(extract, "function", "control_grid()")


  list(verbose = verbose, allow_par = allow_par, extract = extract,
       save_pred = save_pred, pkgs = pkgs)
}

#' @rdname control_grid
#' @export
control_resamples <- control_grid

# ------------------------------------------------------------------------------

#' Control aspects of the Bayesian search process
#'
#' @param verbose A logical for logging results as they are generated. Despite
#'   this argument, warnings and errors are always shown. If using a dark IDE
#'   theme, some logging messages might be hard to see. If this is the case,
#'   try setting the `tidymodels.dark` option with
#'   `options(tidymodels.dark = TRUE)` to print lighter colors.
#' @param no_improve The integer cutoff for the number of iterations without
#'   better results.
#' @param uncertain The number of iterations with no improvement before an
#'  uncertainty sample is created where a sample with high predicted variance is
#'  chosen (i.e., in a region that has not yet been explored). The iteration
#'  counter is reset after each uncertainty sample. For example, if `uncertain =
#'  10`, this condition is triggered every 10 samples with no improvement.
#' @param seed An integer for controlling the random number stream.
#' @param time_limit A number for the minimum number of _minutes_ (elapsed) that
#'   the function should execute. The elapsed time is evaluated at internal
#'   checkpoints and, if over time, the results at that time are returned (with
#'   a warning). This means that the `time_limit` is not an exact limit, but a
#'   minimum time limit.
#' @param extract An optional function with at least one argument (or `NULL`)
#'   that can be used to retain arbitrary objects from the model fit object,
#'   recipe, or other elements of the workflow.
#' @param save_pred A logical for whether the out-of-sample predictions should
#'   be saved for each model _evaluated_.
#' @param pkgs An optional character string of R package names that should be
#'   loaded (by namespace) during parallel processing.
#' @details
#'
#' For `extract`, this function can be used to output the model object, the
#' recipe (if used), or some components of either or both. When evaluated, the
#' function's sole argument has a fitted workflow If the formula method is used,
#' the recipe element will be `NULL`.
#'
#' The results of the `extract` function are added to a list column in the
#' output called `.extracts`. Each element of this list is a tibble with tuning
#' parameter column and a list column (also called `.extracts`) that contains
#' the results of the function. If no extraction function is used, there is no
#' `.extracts` column in the resulting object. See [tune_bayes()] for more
#' specific details.
#'
#' Note that for [collect_predictions()], it is possible that each row of the
#'  original data point might be represented multiple times per tuning
#'  parameter. For example, if the bootstrap or repeated cross-validation are
#'  used, there will be multiple rows since the sample data point has been
#'  evaluated multiple times. This may cause issues when merging the predictions
#'  with the original data.
#' @export
control_bayes <-
  function(verbose = FALSE,
           no_improve = 10L,
           uncertain = Inf,
           seed = sample.int(10^5, 1),
           extract = NULL,
           save_pred = FALSE,
           time_limit = NA,
           pkgs = NULL) {
    # add options for seeds per resample

    val_class_and_single(verbose, "logical", "control_bayes()")
    val_class_and_single(save_pred, "logical", "control_bayes()")
    val_class_and_single(no_improve, c("numeric", "integer"), "control_bayes()")
    val_class_and_single(uncertain, c("numeric", "integer"), "control_bayes()")
    val_class_and_single(seed, c("numeric", "integer"), "control_bayes()")
    val_class_or_null(extract, "function", "control_bayes()")
    val_class_and_single(time_limit, c("logical", "numeric"), "control_bayes()")
    val_class_or_null(pkgs, "character", "control_bayes()")

    if (!is.infinite(uncertain) && uncertain > no_improve) {
      cli::cli_alert_warning(
        "Uncertainty sample scheduled after {uncertain} poor iterations but the search will stop after {no_improve}."
      )
    }

    list(
      verbose = verbose,
      no_improve = no_improve,
      uncertain = uncertain,
      seed = seed,
      extract = extract,
      save_pred = save_pred,
      time_limit = time_limit,
      pkgs = pkgs
    )
  }
