#' Control the grid search process
#'
#' @param verbose A logical for logging results as they are generated. Despite
#' this argument, warnings and errors are always shown.
#' @param allow_par A logical to allow parallel processing (if a parallel
#' backend is registered).
#' @param extract An optional function with at least one argument (or `NULL`)
#' that can be used to retain arbitrary objects from the model fit object,
#' recipe, or other elements of the workflow.
#' @param save_pred A logical for whether the out-of-sample predictions should
#' be saved for each model _evaluated_.
#'
#'@details
#'
#' For `extract`, this function can be used to output the model object, the
#'  recipe (if used), or some components of either or both. When evaluated, the
#'  function's sole argument has a named list with elements `recipe` and
#'  `model`. If the formula method is used, the recipe element will be `NULL`.
#'
#' The results of the `extract` function are added to a list column in the
#'  output called `.extracts`. Each element of this list is a tibble with tuning
#'  parameter column and a list column (also called `.extracts`) that contains
#'  the results of the function. If no extraction function is used, there is no
#'  `.extracts` column in the resulting object.
#' @export
grid_control <- function(verbose = FALSE, allow_par = TRUE,
                         extract = NULL, save_pred = FALSE) {
  # add options for `save_predictions`, and other stuff.
  # seeds per resample
  list(verbose = verbose, allow_par = allow_par, extract = extract,
       save_pred = save_pred)
}



# ------------------------------------------------------------------------------

#' Control the Bayesian search process
#'
#' @param verbose A logical for logging results as they are generated. Despite
#' this argument, warnings and errors are always shown.
#' @param no_improve The integer cutoff for the number of iterations without better
#' results.
#' @param uncertain The number of iterations with no improvement before an
#'  uncertainty sample is created where a sample with high predicted variance is
#'  chosen. The iteration counter is reset after each uncertainty sample. For
#'  example, if `uncertain = 10`, this condition is triggered every 10 samples
#'  with no improvement.
#' @param seed An integer for controlling the random number stream.
#' @param extract An optional function to collection any information from the
#' model or other objects. See `grid_control()` for details. Note that if
#' initial results were already generated using `tune_grid()`, care must be
#' taken if the Bayesian search has a different extraction function.
#' @param save_pred A logical to save the out-of-sample predictions from
#' each resample and each parameter combination. See `grid_control()` for details.
#' @param time_limit A number for the minimum number of _minutes_ (elapsed)
#'  that the function should execute. The elapsed time is evaluated at internal
#'  checkpoints and, if over time, the results at that time are returned (with a
#'  warning). This means that the `time_limit` is not an exact limit, but a
#'  minimum time limit.
#' @export
Bayes_control <-
  function(verbose = FALSE,
           no_improve = 10,
           uncertain = Inf,
           seed = sample.int(10^5, 1),
           extract = NULL,
           save_pred = FALSE,
           time_limit = NA) {
    # add options for `allow_parallel`, and other stuff.
    # seeds per resample

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
      time_limit = time_limit
    )
  }
