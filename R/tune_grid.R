#' Model tuning via grid search
#'
#' [tune_grid()] computes a set of performance metrics (e.g. accuracy or RMSE)
#'  for a pre-defined set of tuning parameters that correspond to a model or
#'  recipe across one or more resamples of the data.
#'
#' @inheritParams last_fit
#' @inheritParams fit_resamples
#' @param param_info A [dials::parameters()] object or `NULL`. If none is given,
#' a parameters set is derived from other arguments. Passing this argument can
#' be useful when parameter ranges need to be customized.
#' @param grid A data frame of tuning combinations or a positive integer. The
#'  data frame should have columns for each parameter being tuned and rows for
#'  tuning parameter candidates. An integer denotes the number of candidate
#'  parameter sets to be created automatically.
#' @param control An object used to modify the tuning process, likely created
#' by [control_grid()].
#' @param eval_time A numeric vector of time points where dynamic event time
#' metrics should be computed (e.g. the time-dependent ROC curve, etc). The
#' values must be non-negative and should probably be no greater than the
#' largest event time in the training set (See Details below).
#' @param ... Not currently used.
#' @return An updated version of `resamples` with extra list columns for `.metrics` and
#' `.notes` (optional columns are `.predictions` and `.extracts`). `.notes`
#' contains warnings and errors that occur during execution.
#' @seealso [control_grid()], [tune()], [fit_resamples()],
#' [autoplot.tune_results()], [show_best()], [select_best()],
#' [collect_predictions()], [collect_metrics()]
#' @details
#'
#' Suppose there are _m_ tuning parameter combinations. [tune_grid()] may not
#' require all _m_ model/recipe fits across each resample. For example:
#'
#' \itemize{
#'   \item In cases where a single model fit can be used to make predictions
#'         for different parameter values in the grid, only one fit is used.
#'         For example, for some boosted trees, if 100 iterations of boosting
#'         are requested, the model object for 100 iterations can be used to
#'         make predictions on iterations less than 100 (if all other
#'         parameters are equal).
#'   \item When the model is being tuned in conjunction with pre-processing
#'         and/or post-processing parameters, the minimum number of fits are
#'         used. For example, if the number of PCA components in a recipe step
#'         are being tuned over three values (along with model tuning
#'         parameters), only three recipes are trained. The alternative
#'         would be to re-train the same recipe multiple times for each model
#'         tuning parameter.
#' }
#'
#' tune supports parallel processing with the \pkg{future} package. To execute
#' the resampling iterations in parallel, specify a [plan][future::plan] with
#' future first. The `allow_par` argument can be used to avoid parallelism.
#'
#' For the most part, warnings generated during training are shown as they occur
#' and are associated with a specific resample when
#' `control_grid(verbose = TRUE)`. They are (usually) not aggregated until the
#' end of processing.
#'
#' @section Parameter Grids:
#'
#' If no tuning grid is provided, a grid (via [dials::grid_space_filling()]) is
#' created with 10 candidate parameter combinations.
#'
#' When provided, the grid should have column names for each parameter and
#'  these should be named by the parameter name or `id`. For example, if a
#'  parameter is marked for optimization using `penalty = tune()`, there should
#'  be a column named `penalty`. If the optional identifier is used, such as
#'  `penalty = tune(id = 'lambda')`, then the corresponding column name should
#'  be `lambda`.
#'
#' In some cases, the tuning parameter values depend on the dimensions of the
#'  data. For example, `mtry` in random forest models depends on the number of
#'  predictors. In this case, the default tuning parameter object requires an
#'  upper range. [dials::finalize()] can be used to derive the data-dependent
#'  parameters. Otherwise, a parameter set can be created (via
#'  [dials::parameters()]) and the `dials` `update()` function can be used to
#'  change the values. This updated parameter set can be passed to the function
#'  via the `param_info` argument.
#'
#' @section Performance Metrics:
#'
#' To use your own performance metrics, the [yardstick::metric_set()] function
#'  can be used to pick what should be measured for each model. If multiple
#'  metrics are desired, they can be bundled. For example, to estimate the area
#'  under the ROC curve as well as the sensitivity and specificity (under the
#'  typical probability cutoff of 0.50), the `metrics` argument could be given:
#'
#' \preformatted{
#'   metrics = metric_set(roc_auc, sens, spec)
#' }
#'
#' Each metric is calculated for each candidate model.
#'
#' If no metric set is provided, one is created:
#' \itemize{
#'   \item For regression models, the root mean squared error and coefficient
#'         of determination are computed.
#'   \item For classification, the area under the ROC curve and overall accuracy
#'         are computed.
#' }
#'
#' Note that the metrics also determine what type of predictions are estimated
#' during tuning. For example, in a classification problem, if metrics are used
#' that are all associated with hard class predictions, the classification
#' probabilities are not created.
#'
#' The out-of-sample estimates of these metrics are contained in a list column
#' called `.metrics`. This tibble contains a row for each metric and columns
#' for the value, the estimator type, and so on.
#'
#' [collect_metrics()] can be used for these objects to collapse the results
#' over the resampled (to obtain the final resampling estimates per tuning
#' parameter combination).
#'
#' @section Obtaining Predictions:
#'
#' When `control_grid(save_pred = TRUE)`, the output tibble contains a list
#' column called `.predictions` that has the out-of-sample predictions for each
#' parameter combination in the grid and each fold (which can be very large).
#'
#' The elements of the tibble are tibbles with columns for the tuning
#' parameters, the row number from the original data object (`.row`), the
#' outcome data (with the same name(s) of the original data), and any columns
#' created by the predictions. For example, for simple regression problems, this
#' function generates a column called `.pred` and so on. As noted above, the
#' prediction columns that are returned are determined by the type of metric(s)
#' requested.
#'
#' This list column can be `unnested` using [tidyr::unnest()] or using the
#'  convenience function [collect_predictions()].
#'
#' @section Extracting Information:
#'
#' The `extract` control option will result in an additional function to be
#'  returned called `.extracts`. This is a list column that has tibbles
#'  containing the results of the user's function for each tuning parameter
#'  combination. This can enable returning each model and/or recipe object that
#'  is created during resampling. Note that this could result in a large return
#'  object, depending on what is returned.
#'
#' The control function contains an option (`extract`) that can be used to
#'  retain any model or recipe that was created within the resamples. This
#'  argument should be a function with a single argument. The value of the
#'  argument that is given to the function in each resample is a workflow
#'  object (see [workflows::workflow()] for more information). Several
#'  helper functions can be used to easily pull out the preprocessing
#'  and/or model information from the workflow, such as
#'  [`extract_preprocessor()`][workflows::extract_preprocessor.workflow()] and
#'  [`extract_fit_parsnip()`][workflows::extract_fit_parsnip.workflow()].
#'
#' As an example, if there is interest in getting each parsnip model fit back,
#' one could use:
#' \preformatted{
#'   extract = function (x) extract_fit_parsnip(x)
#' }
#'
#' Note that the function given to the `extract` argument is evaluated on
#'  every model that is _fit_ (as opposed to every model that is _evaluated_).
#' As noted above, in some cases, model predictions can be derived for
#'  sub-models so that, in these cases, not every row in the tuning parameter
#'  grid has a separate R object associated with it.
#'
#' @template case-weights
#' @template censored-regression
#'
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
#'   recipe(mpg ~ ., data = mtcars) %>%
#'   step_spline_natural(disp, deg_free = tune("disp")) %>%
#'   step_spline_natural(wt, deg_free = tune("wt"))
#'
#' lin_mod <-
#'   linear_reg() %>%
#'   set_engine("lm")
#'
#' # manually create a grid
#' spline_grid <- expand.grid(disp = 2:5, wt = 2:5)
#'
#' # Warnings will occur from making spline terms on the holdout data that are
#' # extrapolations.
#' spline_res <-
#'   tune_grid(lin_mod, spline_rec, resamples = folds, grid = spline_grid)
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
#'   recipe(mpg ~ ., data = mtcars) %>%
#'   step_normalize(all_predictors())
#'
#' svm_mod <-
#'   svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
#'   set_engine("kernlab") %>%
#'   set_mode("regression")
#'
#' # Use a space-filling design with 7 points
#' set.seed(3254)
#' svm_res <- tune_grid(svm_mod, car_rec, resamples = folds, grid = 7)
#' svm_res
#'
#' show_best(svm_res, metric = "rmse")
#'
#' autoplot(svm_res, metric = "rmse") +
#'   scale_x_log10()
#'
#' # ---------------------------------------------------------------------------
#'
#' # Using a variables preprocessor with a workflow
#'
#' # Rather than supplying a preprocessor (like a recipe) and a model directly
#' # to `tune_grid()`, you can also wrap them up in a workflow and pass
#' # that along instead (note that this doesn't do any preprocessing to
#' # the variables, it passes them along as-is).
#' wf <- workflow() %>%
#'   add_variables(outcomes = mpg, predictors = everything()) %>%
#'   add_model(svm_mod)
#'
#' set.seed(3254)
#' svm_res_wf <- tune_grid(wf, resamples = folds, grid = 7)
#' @export
tune_grid <- function(object, ...) {
  UseMethod("tune_grid")
}

#' @export
tune_grid.default <- function(object, ...) {
  cli::cli_abort(
    "The first argument to {.fn tune_grid} should be either a model or workflow,
    not {.obj_type_friendly {object}}."
  )
}

#' @export
#' @rdname tune_grid
tune_grid.model_spec <- function(object, preprocessor, resamples, ...,
                                 param_info = NULL, grid = 10, metrics = NULL,
                                 eval_time = NULL, control = control_grid()) {
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

  tune_grid(
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
#' @rdname tune_grid
tune_grid.workflow <- function(object, resamples, ..., param_info = NULL,
                               grid = 10, metrics = NULL,
                               eval_time = NULL, control = control_grid()) {
  empty_ellipses(...)

  control <- parsnip::condense_control(control, control_grid())

  # Disallow `NULL` grids in `tune_grid()`, as this is the special signal
  # used when no tuning is required
  if (is.null(grid)) {
    cli::cli_abort(grid_msg)
  }

  res <-
    tune_grid_workflow(
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

tune_grid_workflow <- function(workflow,
                               resamples,
                               grid = 10,
                               metrics = NULL,
                               eval_time = NULL,
                               pset = NULL,
                               control = control_grid(),
                               rng = TRUE,
                               call = caller_env()) {
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

  # Save rset attributes, then fall back to a bare tibble
  rset_info <- pull_rset_attributes(resamples)
  split_args <- rsample::.get_split_args(resamples)
  resamples <- new_bare_tibble(resamples)

  resamples <- tune_grid_loop(
    resamples = resamples,
    grid = grid,
    workflow = workflow,
    metrics = metrics,
    eval_time = eval_time,
    control = control,
    rng = rng,
    split_args = split_args
  )

  if (is_cataclysmic(resamples)) {
    cli::cli_warn(
      "All models failed. Run {.code show_notes(.Last.tune.result)} for more
       information."
    )
  }

  outcomes <- reduce_all_outcome_names(resamples)
  resamples[[".all_outcome_names"]] <- NULL

  workflow <- set_workflow(workflow, control)

  new_tune_results(
    x = resamples,
    parameters = pset,
    metrics = metrics,
    eval_time = eval_time,
    eval_time_target = NULL,
    outcomes = outcomes,
    rset_info = rset_info,
    workflow = workflow
  )
}

# ------------------------------------------------------------------------------

#' @export
#' @keywords internal
#' @rdname empty_ellipses
pull_rset_attributes <- function(x) {
  excl_att <- c("names", "row.names")
  att <- attributes(x)
  att_nms <- names(att)
  att_nms <- setdiff(att_nms, excl_att)
  att$class <- setdiff(class(x), class(tibble::new_tibble(list())))
  att$class <- att$class[att$class != "rset"]

  lab <- try(pretty(x), silent = TRUE)
  if (inherits(lab, "try-error")) {
    lab <- NA_character_
  }
  list(att = att[att_nms], label = lab)
}

# ------------------------------------------------------------------------------

set_workflow <- function(workflow, control) {
  if (control$save_workflow) {
    if (!is.null(workflow$pre$actions$recipe)) {
      w_size <- utils::object.size(workflow$pre$actions$recipe)
      # make 5MB cutoff
      if (w_size / 1024^2 > 5) {
        msg <-
          paste0(
            "The workflow being saved contains a recipe, which is ",
            format(w_size, units = "Mb", digits = 2),
            " in memory. If this was not intentional, please set the control ",
            "setting `save_workflow = FALSE`."
          )
        cols <- get_tune_colors()
        msg <- strwrap(msg, prefix = paste0(cols$symbol$info(cli::symbol$info), " "))
        msg <- cols$message$info(paste0(msg, collapse = "\n"))
        cli::cli_bullets(msg)
      }
    }
    workflow
  } else {
    NULL
  }
}
