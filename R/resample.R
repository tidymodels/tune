#' Fit multiple models via resampling
#'
#' [fit_resamples()] computes a set of performance metrics across one or more
#' resamples. It does not perform any tuning (see [tune_grid()] and
#' [tune_bayes()] for that), and is instead used for fitting a single
#' model+recipe or model+formula combination across many resamples.
#'
#' @inheritParams last_fit
#'
#' @param resamples A resample `rset` created from an `rsample` function such
#'   as [rsample::vfold_cv()].
#'
#' @param control A [control_resamples()] object used to fine tune the resampling
#'   process.
#'
#' @inheritSection tune_grid Performance Metrics
#' @inheritSection tune_grid Obtaining Predictions
#' @inheritSection tune_grid Extracting Information
#' @seealso [control_resamples()], [collect_predictions()], [collect_metrics()]
#' @examples
#' \donttest{
#' library(recipes)
#' library(rsample)
#' library(parsnip)
#' library(workflows)
#'
#' set.seed(6735)
#' folds <- vfold_cv(mtcars, v = 5)
#'
#' spline_rec <- recipe(mpg ~ ., data = mtcars) %>%
#'   step_ns(disp) %>%
#'   step_ns(wt)
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
#' # preprocessor is used, which lets you supply terms using tidyselect.
#' # The variables are used as-is, no preprocessing is done to them.
#' wf <- workflow() %>%
#'   add_variables(outcomes = mpg, predictors = everything()) %>%
#'   add_model(lin_mod)
#'
#' wf_res <- fit_resamples(wf, folds)
#' }
#' @export
fit_resamples <- function(object, ...) {
  UseMethod("fit_resamples")
}

#' @export
fit_resamples.default <- function(object, ...) {
  msg <- paste0(
    "The first argument to [fit_resamples()] should be either ",
    "a model or workflow."
  )
  rlang::abort(msg)
}

#' @export
fit_resamples.recipe <- function(object,
                                 model,
                                 resamples,
                                 ...,
                                 metrics = NULL,
                                 control = control_resamples()) {

  lifecycle::deprecate_soft("0.1.0",
                            what = "fit_resamples.recipe()",
                            details = deprecate_msg(match.call(), "fit_resamples"))
  empty_ellipses(...)

  fit_resamples(model, preprocessor = object, resamples = resamples,
                metrics = metrics, control = control)
}

#' @export
fit_resamples.formula <- function(formula,
                                  model,
                                  resamples,
                                  ...,
                                  metrics = NULL,
                                  control = control_resamples()) {

  lifecycle::deprecate_soft("0.1.0",
                            what = "fit_resamples.formula()",
                            details = deprecate_msg(match.call(), "fit_resamples"))
  empty_ellipses(...)

  fit_resamples(model, preprocessor = formula, resamples = resamples,
                metrics = metrics, control = control)
}

#' @export
#' @rdname fit_resamples
fit_resamples.model_spec <- function(object,
                                     preprocessor,
                                     resamples,
                                     ...,
                                     metrics = NULL,
                                     control = control_resamples()) {

  if (rlang::is_missing(preprocessor) || !is_preprocessor(preprocessor)) {
    rlang::abort(paste("To tune a model spec, you must preprocess",
                       "with a formula or recipe"))
  }

  empty_ellipses(...)

  wflow <- add_model(workflow(), object)

  if (is_recipe(preprocessor)) {
    wflow <- add_recipe(wflow, preprocessor)
  } else if (rlang::is_formula(preprocessor)) {
    wflow <- add_formula(wflow, preprocessor)
  }

  resample_workflow(wflow, resamples, metrics, control)
}


#' @rdname fit_resamples
#' @export
fit_resamples.workflow <- function(object,
                                   resamples,
                                   ...,
                                   metrics = NULL,
                                   control = control_resamples()) {

  empty_ellipses(...)

  resample_workflow(object, resamples, metrics, control)
}

# ------------------------------------------------------------------------------

resample_workflow <- function(workflow, resamples, metrics, control) {
  check_rset(resamples)
  check_workflow(workflow)
  metrics <- check_metrics(metrics, workflow)

  resamples <- dplyr::mutate(resamples, .seed = sample.int(10^5, nrow(resamples)))

  # Save rset attributes, then fall back to a bare tibble
  rset_info <- pull_rset_attributes(resamples)
  resamples <- new_bare_tibble(resamples)

  resamples <- tune_grid_loop(
    resamples = resamples,
    grid = NULL,
    workflow = workflow,
    metrics = metrics,
    control = control
  )

  if (is_cataclysmic(resamples)) {
    rlang::warn(
      "All models failed in [fit_resamples()]. See the `.notes` column."
    )
  }

  outcomes <- reduce_all_outcome_names(resamples)
  resamples[[".all_outcome_names"]] <- NULL

  workflow_output <- set_workflow(workflow, control)

  resamples <- dplyr::select(resamples, -.seed)

  new_resample_results(
    x = resamples,
    parameters = parameters(workflow),
    metrics = metrics,
    outcomes = outcomes,
    rset_info = rset_info,
    workflow = workflow_output
  )
}
