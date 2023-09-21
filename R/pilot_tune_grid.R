#' Run a subset of tuning grid to pre-screen for issues
#'
#' [pilot_tune_grid()] finds the extreme values from a pre-defined set of tuning
#' parameters that correspond to a model or recipe and fits the model on only
#' one of the resample splits, for purposes of quick error checking.
#'
#' @param object A `parsnip` model specification or a [workflows::workflow()].
#' @param preprocessor A traditional model formula or a recipe created using
#'   [recipes::recipe()].
#' @param resamples An `rset()` object.
#' @param param_info A [dials::parameters()] object or `NULL`. If none is given,
#' a parameters set is derived from other arguments. Passing this argument can
#' be useful when parameter ranges need to be customized.
#' @param grid A data frame of tuning combinations or a positive integer. The
#'  data frame should have columns for each parameter being tuned and rows for
#'  tuning parameter candidates. An integer denotes the number of candidate
#'  parameter sets to be created automatically.
#' @param metrics A [yardstick::metric_set()] or `NULL`.
#' @param control An object used to modify the tuning process.
#' @param eval_time A numeric vector of time points where dynamic event time
#' metrics should be computed (e.g. the time-dependent ROC curve, etc). The
#' values must be non-negative and should probably be no greater than the
#' largest event time in the training set (See Details below).
#' @param ... Not currently used.
#' @return An updated version of `resamples` with extra list columns for `.metrics` and
#' `.notes` (optional columns are `.predictions` and `.extracts`). `.notes`
#' contains warnings and errors that occur during execution.
#' @seealso [tune_grid()], [pilot_fit_resamples()]
#' @details
#'
#' See [tune_grid()] for details of defaults and computation.
#'
#' @export
pilot_tune_grid <- function(object, ...) {
  UseMethod("pilot_tune_grid")
}

#' @export
pilot_tune_grid.default <- function(object, ...) {
  msg <- paste0(
    "The first argument to [pilot_tune_grid()] should be either ",
    "a model or workflow."
  )
}

#' @export
#' @rdname pilot_tune_grid
pilot_tune_grid.model_spec <- function(object, preprocessor, resamples, ...,
                                 param_info = NULL, grid = 10, metrics = NULL,
                                 control = control_grid(), eval_time = NULL) {
  if (rlang::is_missing(preprocessor) || !is_preprocessor(preprocessor)) {
    rlang::abort(paste(
      "To tune a model spec, you must preprocess",
      "with a formula or recipe"
    ))
  }

  control <- parsnip::condense_control(control, control_grid())

  empty_ellipses(...)

  wflow <- add_model(workflow(), object)

  if (is_recipe(preprocessor)) {
    wflow <- add_recipe(wflow, preprocessor)
  } else if (rlang::is_formula(preprocessor)) {
    wflow <- add_formula(wflow, preprocessor)
  }

  tune_grid(
    wflow,
    resamples = resamples,
    param_info = param_info,
    grid = grid,
    metrics = metrics,
    control = control,
    eval_time = eval_time
  )
}

#' @export
#' @rdname pilot_tune_grid
pilot_tune_grid.workflow <- function(object, resamples, ..., param_info = NULL,
                               grid = 10, metrics = NULL,
                               control = control_grid(), eval_time = NULL) {
  empty_ellipses(...)

  control <- parsnip::condense_control(control, control_grid())

  # Disallow `NULL` grids in `tune_grid()`, as this is the special signal
  # used when no tuning is required
  if (is.null(grid)) {
    rlang::abort(grid_msg)
  }

  res <-
    pilot_tune_grid_workflow(
      object,
      resamples = resamples,
      grid = grid,
      metrics = metrics,
      pset = param_info,
      control = control,
      eval_time = eval_time
    )
  .stash_last_result(res)
  res
}

# ------------------------------------------------------------------------------

pilot_tune_grid_workflow <- function(workflow,
                               resamples,
                               grid = 10,
                               metrics = NULL,
                               pset = NULL,
                               control = control_grid(),
                               eval_time = NULL,
                               rng = TRUE) {
  check_rset(resamples)


  metrics <- check_metrics(metrics, workflow)
  check_eval_time(eval_time, metrics)

  pset <- check_parameters(
    workflow,
    pset = pset,
    data = resamples$splits[[1]]$data,
    grid_names = names(grid)
  )

  check_workflow(workflow, pset = pset)
  check_backend_options(control$backend_options)

  grid <- check_grid(
    grid = grid,
    workflow = workflow,
    pset = pset
  )

  # Cut grid down to extreme values only for piloting
  grid <- grid |>
    arrange(across(matches("."))) |>
    slice(1, nrow(grid))


  # Save rset attributes, then fall back to a bare tibble
  rset_info <- pull_rset_attributes(resamples)
  resamples <- new_bare_tibble(resamples)

  # Use only the first resampling split
  resamples <- resamples |> slice(1)

  resamples <- tune_grid_loop(
    resamples = resamples,
    grid = grid,
    workflow = workflow,
    metrics = metrics,
    control = control,
    eval_time = eval_time,
    rng = rng
  )

  if (is_cataclysmic(resamples)) {
    rlang::warn("All models failed. Run `show_notes(.Last.tune.result)` for more information.")
  }

  outcomes <- reduce_all_outcome_names(resamples)
  resamples[[".all_outcome_names"]] <- NULL

  workflow <- set_workflow(workflow, control)

  new_tune_results(
    x = resamples,
    parameters = pset,
    metrics = metrics,
    outcomes = outcomes,
    rset_info = rset_info,
    workflow = workflow
  )
}
