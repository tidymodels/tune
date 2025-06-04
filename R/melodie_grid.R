#' Model tuning via grid search
#'
#' @inheritParams tune_grid
#' @examplesIf tune:::should_run_examples(suggests = "kernlab") & rlang::is_installed("splines2")
#' library(recipes)
#' library(rsample)
#' library(parsnip)
#' library(workflows)
#' library(ggplot2)
#'
#' # ---------------------------------------------------------------------------
#'
#' set.seed(6735)
#' folds <- vfold_cv(mtcars, v = 5)
#'
#' # ---------------------------------------------------------------------------
#'
#' # tuning recipe parameters:
#'
#' spline_rec <-
#'   recipe(mpg ~ ., data = mtcars) |>
#'   step_spline_natural(disp, deg_free = tune("disp")) |>
#'   step_spline_natural(wt, deg_free = tune("wt"))
#'
#' lin_mod <-
#'   linear_reg() |>
#'   set_engine("lm")
#'
#' # manually create a grid
#' spline_grid <- expand.grid(disp = 2:5, wt = 2:5)
#'
#' # Warnings will occur from making spline terms on the holdout data that are
#' # extrapolations.
#' spline_res <-
#'   melodie_grid(lin_mod, spline_rec, resamples = folds, grid = spline_grid)
#' spline_res
#'
#'
#' show_best(spline_res, metric = "rmse")
#'
#' # ---------------------------------------------------------------------------
#'
#' # tune model parameters only (example requires the `kernlab` package)
#'
#' car_rec <-
#'   recipe(mpg ~ ., data = mtcars) |>
#'   step_normalize(all_predictors())
#'
#' svm_mod <-
#'   svm_rbf(cost = tune(), rbf_sigma = tune()) |>
#'   set_engine("kernlab") |>
#'   set_mode("regression")
#'
#' # Use a space-filling design with 7 points
#' set.seed(3254)
#' svm_res <- melodie_grid(svm_mod, car_rec, resamples = folds, grid = 7)
#' svm_res
#' @export
melodie_grid <- function(object, ...) {
  UseMethod("melodie_grid")
}

#' @export
melodie_grid.default <- function(object, ...) {
  cli::cli_abort(
    "The first argument to {.fn melodie_grid} should be either a model or workflow,
    not {.obj_type_friendly {object}}."
  )
}

#' @export
#' @rdname melodie_grid
melodie_grid.model_spec <- function(
  object,
  preprocessor,
  resamples,
  ...,
  param_info = NULL,
  grid = 10,
  metrics = NULL,
  eval_time = NULL,
  control = control_grid()
) {
  if (rlang::is_missing(preprocessor) || !is_preprocessor(preprocessor)) {
    cli::cli_abort(tune_pp_msg)
  }

  control <- parsnip::condense_control(control, control_grid())

  empty_ellipses(...)

  wflow <- add_model(workflow(), object)

  if (is_recipe(preprocessor)) {
    wflow <- add_recipe(wflow, preprocessor)
  } else if (rlang::is_formula(preprocessor)) {
    wflow <- add_formula(wflow, preprocessor)
  }

  melodie_grid(
    wflow,
    resamples = resamples,
    param_info = param_info,
    grid = grid,
    metrics = metrics,
    eval_time = eval_time,
    control = control
  )
}

#' @export
#' @rdname melodie_grid
melodie_grid.workflow <- function(
  object,
  resamples,
  ...,
  param_info = NULL,
  grid = 10,
  metrics = NULL,
  eval_time = NULL,
  control = control_grid()
) {
  empty_ellipses(...)

  control <- parsnip::condense_control(control, control_grid())

  # Disallow `NULL` grids in `melodie_grid()`, as this is the special signal
  # used when no tuning is required
  if (is.null(grid)) {
    cli::cli_abort(grid_msg)
  }

  res <- melodie_grid_workflow(
    object,
    resamples = resamples,
    grid = grid,
    metrics = metrics,
    eval_time = eval_time,
    pset = param_info,
    control = control
  )
  .stash_last_result(res)
  res
}

# ------------------------------------------------------------------------------

melodie_grid_workflow <- function(
  workflow,
  resamples,
  grid = 10,
  metrics = NULL,
  eval_time = NULL,
  pset = NULL,
  control = control_grid(),
  rng = TRUE,
  call = caller_env()
) {
  initialize_catalog_melodie()

  check_rset(resamples)

  metrics <- check_metrics_arg(metrics, workflow, call = call)
  eval_time <- check_eval_time_arg(eval_time, metrics, call = call)

  pset <- check_parameters(
    workflow,
    pset = pset,
    data = resamples$splits[[1]]$data,
    grid_names = names(grid)
  )

  check_workflow(workflow, pset = pset, call = call)
  check_backend_options(control$backend_options)

  grid <- check_grid(
    grid = grid,
    workflow = workflow,
    pset = pset
  )

  resamples <- tune_grid_loop_new(
    resamples = resamples,
    grid = grid,
    workflow = workflow,
    param_info = pset,
    metrics = metrics,
    eval_time = eval_time,
    control = control
  )

  if (is_cataclysmic(resamples)) {
    cli::cli_warn(
      "All models failed. Run {.code show_notes(.Last.tune.result)} for more
       information."
    )
  }

  resamples
}
