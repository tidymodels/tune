#' Fit multiple models via resampling
#'
#' [fit_resamples()] computes a set of performance metrics across one or more
#' resamples. It does not perform any tuning (see [tune_grid()] and
#' [tune_bayes()] for that), and is instead used for fitting a single
#' model+recipe or model+formula combination across many resamples.
#'
#' @inheritParams last_fit
#' @inheritParams tune_grid
#'
#' @param resamples An `rset` resampling object created from an `rsample`
#' function, such as [rsample::vfold_cv()].
#'
#' @param control A [control_resamples()] object used to fine tune the resampling
#'   process.
#'
#' @inheritSection tune_grid Performance Metrics
#' @inheritSection tune_grid Obtaining Predictions
#' @inheritSection tune_grid Extracting Information
#' @template case-weights
#' @template censored-regression
#' @seealso [control_resamples()], [collect_predictions()], [collect_metrics()]
#' @examplesIf tune:::should_run_examples() & rlang::is_installed("splines2")
#' library(recipes)
#' library(rsample)
#' library(parsnip)
#' library(workflows)
#'
#' set.seed(6735)
#' folds <- vfold_cv(mtcars, v = 5)
#'
#' spline_rec <- recipe(mpg ~ ., data = mtcars) %>%
#'   step_spline_natural(disp) %>%
#'   step_spline_natural(wt)
#'
#' lin_mod <- linear_reg() %>%
#'   set_engine("lm")
#'
#' control <- control_resamples(save_pred = TRUE)
#'
#' spline_res <- fit_resamples(lin_mod, spline_rec, folds, control = control)
#'
#' spline_res
#'
#' show_best(spline_res, metric = "rmse")
#'
#' # You can also wrap up a preprocessor and a model into a workflow, and
#' # supply that to `fit_resamples()` instead. Here, a workflows "variables"
#' # preprocessor is used, which lets you supply terms using dplyr selectors.
#' # The variables are used as-is, no preprocessing is done to them.
#' wf <- workflow() %>%
#'   add_variables(outcomes = mpg, predictors = everything()) %>%
#'   add_model(lin_mod)
#'
#' wf_res <- fit_resamples(wf, folds)
#' @export
fit_resamples <- function(object, ...) {
  UseMethod("fit_resamples")
}

#' @export
fit_resamples.default <- function(object, ...) {
  cli::cli_abort(
    "The first argument to {.fn fit_resamples} should be either a model or workflow,
    not {.obj_type_friendly {object}}."
  )
}

#' @export
#' @rdname fit_resamples
fit_resamples.model_spec <- function(object,
                                     preprocessor,
                                     resamples,
                                     ...,
                                     metrics = NULL,
                                     eval_time = NULL,
                                     control = control_resamples()) {
  if (rlang::is_missing(preprocessor) || !is_preprocessor(preprocessor)) {
    cli::cli_abort(tune_pp_msg)
  }

  control <- parsnip::condense_control(control, control_resamples())

  empty_ellipses(...)

  wflow <- add_model(workflow(), object)

  if (is_recipe(preprocessor)) {
    wflow <- add_recipe(wflow, preprocessor)
  } else if (rlang::is_formula(preprocessor)) {
    wflow <- add_formula(wflow, preprocessor)
  }

  fit_resamples(
    wflow,
    resamples = resamples,
    metrics = metrics,
    eval_time = eval_time,
    control = control
  )
}


#' @rdname fit_resamples
#' @export
fit_resamples.workflow <- function(object,
                                   resamples,
                                   ...,
                                   metrics = NULL,
                                   eval_time = NULL,
                                   control = control_resamples()) {
  empty_ellipses(...)

  control <- parsnip::condense_control(control, control_resamples())

  res <-
    resample_workflow(
      workflow = object,
      resamples = resamples,
      metrics = metrics,
      eval_time = eval_time,
      control = control,
      rng = TRUE
    )
  .stash_last_result(res)
  res
}

# ------------------------------------------------------------------------------

resample_workflow <- function(workflow, resamples, metrics, eval_time = NULL,
                              control, rng, call = caller_env()) {
  check_no_tuning(workflow)

  # `NULL` is the signal that we have no grid to tune with
  grid <- NULL
  pset <- NULL

  out <- tune_grid_workflow(
    workflow = workflow,
    resamples = resamples,
    grid = grid,
    metrics = metrics,
    eval_time = eval_time,
    pset = pset,
    control = control,
    rng = rng,
    call = call
  )

  attributes <- attributes(out)

  new_resample_results(
    x = out,
    parameters = attributes$parameters,
    metrics = attributes$metrics,
    eval_time = attributes$eval_time,
    eval_time_target = NULL,
    outcomes = attributes$outcomes,
    rset_info = attributes$rset_info,
    workflow = attributes$workflow
  )
}
