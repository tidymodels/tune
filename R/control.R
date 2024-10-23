#' Control aspects of the grid search process
#'
#' @inheritParams control_bayes
#'
#' @inheritSection collect_predictions Hyperparameters and extracted objects
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
                         pkgs = NULL, save_workflow = FALSE,
                         event_level = "first",
                         parallel_over = NULL,
                         backend_options = NULL) {

  # Any added arguments should also be added in superset control functions
  # in other packages

  # add options for seeds per resample
  check_bool(verbose)
  check_bool(allow_par)
  check_bool(save_pred)
  check_bool(save_workflow)
  check_string(event_level)
  check_character(pkgs, allow_null = TRUE)
  check_function(extract, allow_null = TRUE)

  val_parallel_over(parallel_over, "control_grid()")


  res <- list(
    verbose = verbose,
    allow_par = allow_par,
    extract = extract,
    save_pred = save_pred,
    pkgs = pkgs,
    save_workflow = save_workflow,
    event_level = event_level,
    parallel_over = parallel_over,
    backend_options = backend_options
  )

  class(res) <- c("control_grid", "control_resamples")
  res
}

#' @export
print.control_grid <- function(x, ...) {
  cat("grid/resamples control object\n")
  invisible(x)
}

#' @rdname control_grid
#' @export
control_resamples <- control_grid

#' Control aspects of the last fit process
#'
#' @inheritParams control_grid
#'
#' @details
#'
#' [control_last_fit()] is a wrapper around [control_resamples()] and is meant
#'   to be used with [last_fit()].
#'
#' @export
control_last_fit <- function(
    verbose = FALSE,
    event_level = "first",
    allow_par = FALSE
) {
  # Any added arguments should also be added in superset control functions
  # in other packages

  extr <- function(x) x
  control <-
    control_resamples(
      verbose = verbose,
      allow_par = allow_par,
      event_level = event_level,
      extract = extr,
      save_pred = TRUE,
      save_workflow = FALSE
    )
  class(control) <- c("control_last_fit", class(control))
  control
}

#' @export
print.control_last_fit <- function(x, ...) {
  cat("last fit control object\n")
  invisible(x)
}

# ------------------------------------------------------------------------------

#' Control aspects of the Bayesian search process
#'
#' @param verbose A logical for logging results (other than warnings and errors,
#'   which are always shown) as they are generated during training in a single
#'   R process. When using most parallel backends, this argument typically will
#'   not result in any logging. If using a dark IDE theme, some logging messages
#'   might be hard to see; try setting the `tidymodels.dark` option with
#'   `options(tidymodels.dark = TRUE)` to print lighter colors.
#' @param verbose_iter A logical for logging results of the Bayesian search
#'   process. Defaults to FALSE. If using a dark IDE theme, some logging
#'   messages might be hard to see; try setting the `tidymodels.dark` option
#'   with `options(tidymodels.dark = TRUE)` to print lighter colors.
#' @param no_improve The integer cutoff for the number of iterations without
#'   better results.
#' @param uncertain The number of iterations with no improvement before an
#'  uncertainty sample is created where a sample with high predicted variance is
#'  chosen (i.e., in a region that has not yet been explored). The iteration
#'  counter is reset after each uncertainty sample. For example, if `uncertain =
#'  10`, this condition is triggered every 10 samples with no improvement.
#' @param seed An integer for controlling the random number stream. Tuning
#' functions are sensitive to both the state of RNG set outside of tuning
#' functions with `set.seed()` as well as the value set here. The value of the
#' former determines RNG for the higher-level tuning process, like grid
#' generation and setting the value of this argument if left as default. The
#' value of this argument determines RNG state in workers for each iteration
#' of model fitting, determined by the value of `parallel_over`.
#' @param time_limit A number for the minimum number of _minutes_ (elapsed) that
#'   the function should execute. The elapsed time is evaluated at internal
#'   checkpoints and, if over time, the results at that time are returned (with
#'   a warning). This means that the `time_limit` is not an exact limit, but a
#'   minimum time limit.
#'
#'   Note that timing begins immediately on execution. Thus, if the
#'   `initial` argument to [tune_bayes()] is supplied as a number, the elapsed
#'   time will include the time needed to generate initialization results.
#' @param extract An optional function with at least one argument (or `NULL`)
#'   that can be used to retain arbitrary objects from the model fit object,
#'   recipe, or other elements of the workflow.
#' @param save_pred A logical for whether the out-of-sample predictions should
#'   be saved for each model _evaluated_.
#' @param pkgs An optional character string of R package names that should be
#'   loaded (by namespace) during parallel processing.
#' @param save_workflow A logical for whether the workflow should be appended
#'  to the output as an attribute.
#' @param save_gp_scoring A logical to save the intermediate Gaussian process
#'   models for each iteration of the search. These are saved to
#'  `tempdir()` with names `gp_candidates_{i}.RData` where `i` is the iteration.
#'  These results are deleted when the R session ends. This option is only
#'  useful for teaching purposes.
#' @param event_level A single string containing either `"first"` or `"second"`.
#'   This argument is passed on to yardstick metric functions when any type
#'   of class prediction is made, and specifies which level of the outcome
#'   is considered the "event".
#' @param parallel_over A single string containing either `"resamples"` or
#'   `"everything"` describing how to use parallel processing. Alternatively,
#'   `NULL` is allowed, which chooses between `"resamples"` and `"everything"`
#'   automatically.
#'
#'   If `"resamples"`, then tuning will be performed in parallel over resamples
#'   alone. Within each resample, the preprocessor (i.e. recipe or formula) is
#'   processed once, and is then reused across all models that need to be fit.
#'
#'   If `"everything"`, then tuning will be performed in parallel at two levels.
#'   An outer parallel loop will iterate over resamples. Additionally, an
#'   inner parallel loop will iterate over all unique combinations of
#'   preprocessor and model tuning parameters for that specific resample. This
#'   will result in the preprocessor being re-processed multiple times, but
#'   can be faster if that processing is extremely fast.
#'
#'   If `NULL`, chooses `"resamples"` if there are more than one resample,
#'   otherwise chooses `"everything"` to attempt to maximize core utilization.
#'
#'   Note that switching between `parallel_over` strategies is not guaranteed
#'   to use the same random number generation schemes. However, re-tuning a
#'   model using the same `parallel_over` strategy is guaranteed to be
#'   reproducible between runs.
#' @param backend_options An object of class `"tune_backend_options"` as created
#'   by `tune::new_backend_options()`, used to pass arguments to specific tuning
#'   backend. Defaults to `NULL` for default backend options.
#' @param allow_par A logical to allow parallel processing (if a parallel
#'   backend is registered).
#'
#' @inheritSection collect_predictions Hyperparameters and extracted objects
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
#' @export
control_bayes <-
  function(verbose = FALSE,
           verbose_iter = FALSE,
           no_improve = 10L,
           uncertain = Inf,
           seed = sample.int(10^5, 1),
           extract = NULL,
           save_pred = FALSE,
           time_limit = NA,
           pkgs = NULL,
           save_workflow = FALSE,
           save_gp_scoring = FALSE,
           event_level = "first",
           parallel_over = NULL,
           backend_options = NULL,
           allow_par = TRUE) {
    # Any added arguments should also be added in superset control functions
    # in other packages

    # add options for seeds per resample
    check_bool(verbose)
    check_bool(verbose_iter)
    check_bool(allow_par)
    check_bool(save_pred)
    check_bool(save_workflow)
    check_bool(save_gp_scoring)
    check_character(pkgs, allow_null = TRUE)
    check_function(extract, allow_null = TRUE)
    check_number_whole(no_improve, min = 0, allow_infinite = TRUE)
    check_number_whole(uncertain, min = 0, allow_infinite = TRUE)
    check_number_whole(seed)

    check_time_limit_arg(time_limit)

    val_parallel_over(parallel_over, "control_bayes()")


    if (!is.infinite(uncertain) && uncertain > no_improve) {
      cli::cli_warn(
        "Uncertainty sample scheduled after {uncertain} poor iterations but the
         search will stop after {no_improve}."
      )
    }

    res <-
      list(
        verbose = verbose,
        verbose_iter = verbose_iter,
        allow_par = allow_par,
        no_improve = no_improve,
        uncertain = uncertain,
        seed = seed,
        extract = extract,
        save_pred = save_pred,
        time_limit = time_limit,
        pkgs = pkgs,
        save_workflow = save_workflow,
        save_gp_scoring = save_gp_scoring,
        event_level = event_level,
        parallel_over = parallel_over,
        backend_options = backend_options
      )

    class(res) <- "control_bayes"
    res
  }

#' @export
print.control_bayes <- function(x, ...) {
  cat("bayes control object\n")
  invisible(x)
}

# ------------------------------------------------------------------------------

val_parallel_over <- function(parallel_over, where) {
  check_string(parallel_over, allow_null = TRUE)
  if (!is.null(parallel_over)) {
    rlang::arg_match0(parallel_over, c("resamples", "everything"), "parallel_over")
  }

  invisible(NULL)
}

#' @export
#' @keywords internal
#' @rdname control_grid
new_backend_options <- function(..., class = character()) {
  out <- rlang::list2(...)

  if (any(rlang::names2(out) == "")) {
    cli::cli_abort("All backend options must be named.")
  }

  structure(out, class = c(class, "tune_backend_options"))
}
