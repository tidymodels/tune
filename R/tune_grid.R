#' Model tuning via grid search
#'
#' [tune_grid()] computes a set of performance metrics (e.g. accuracy or RMSE)
#'  for a pre-defined set of tuning parameters that correspond to a model or
#'  recipe across one or more resamples of the data.
#'
#' @param object A model workflow, R formula or recipe object.
#' @param formula A traditional model formula.
#' @param model A `parsnip` model specification (or `NULL` when `object` is a
#' workflow).
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
#'         parameters), only three recipes are are trained. The alternative
#'         would be to re-train the same recipe multiple times for each model
#'         tuning parameter.
#' }
#'
#' The `foreach` package is used here. To execute the resampling iterations in
#' parallel, register a parallel backend function. See the documentation for
#' [foreach::foreach()] for examples.
#'
#' For the most part, warnings generated during training are shown as they occur
#' and are associated with a specific resample when `control(verbose = TRUE)`.
#' They are (usually) not aggregated until the end of processing.
#'
#' @section Parameter Grids:
#'
#' If no tuning grid is provided, a semi-random grid (via
#' [dials::grid_latin_hypercube()]) is created with 10 candidate parameter
#' combinations.
#'
#' When provided, the grid should have column names for each parameter and
#'  these should be named by the parameter name or `id`. For example, if a
#'  parameter is marked for optimization using `penalty = tune()`, there should
#'  be a column names `tune`. If the optional identifier is used, such as
#'  `penalty = tune(id = 'lambda')`, then the corresponding column name should
#'  be `lambda`.
#'
#' In some cases, the tuning parameter values depend on the dimensions of the
#'  data. For example, `mtry` in random forest models depends on the number of
#'  predictors. In this case, the default tuning parameter object requires an
#'  upper range. [dials::finalize()] can be used to derive the data-dependent
#'  parameters. Otherwise, a parameter set can be created (via
#'  [dials::parameters()] and the `dials` `update()` function can be used to
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
#' When `control(save_preds = TRUE)`, the output tibble contains a list column
#'  called `.predictions` that has the out-of-sample predictions for each
#'  parameter combination in the grid and each fold (which can be very large).
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
#'  object (see [workflows::workflow()] for more information). There are two
#'  helper functions that can be used to easily pull out the recipe (if any)
#'  and/or the model: [extract_recipe()] and [extract_model()].
#'
#' As an example, if there is interest in getting each model back, one could use:
#' \preformatted{
#'   extract = function (x) extract_model(x)
#' }
#'
#' Note that the function given to the `extract` argument is evaluated on
#'  every model that is _fit_ (as opposed to every model that is _evaluated_).
#' As noted above, in some cases, model predictions can be derived for
#'  sub-models so that, in these cases, not every row in the tuning parameter
#'  grid has a separate R object associated with it.
#' @examples
#' \donttest{
#' library(recipes)
#' library(rsample)
#' library(parsnip)
#'
#' # ------------------------------------------------------------------------------
#'
#' set.seed(6735)
#' folds <- vfold_cv(mtcars, v = 5)
#'
#' # ------------------------------------------------------------------------------
#'
#' # tuning recipe parameters:
#'
#' spline_rec <-
#'   recipe(mpg ~ ., data = mtcars) %>%
#'   step_ns(disp, deg_free = tune("disp")) %>%
#'   step_ns(wt, deg_free = tune("wt"))
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
#'   tune_grid(spline_rec, model = lin_mod, resamples = folds, grid = spline_grid)
#' spline_res
#'
#'
#' show_best(spline_res, metric = "rmse")
#'
#' # ------------------------------------------------------------------------------
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
#' svm_res <- tune_grid(car_rec, model = svm_mod, resamples = folds, grid = 7)
#' svm_res
#'
#' show_best(svm_res, metric = "rmse")
#'
#' autoplot(svm_res, metric = "rmse") +
#'   scale_x_log10()
#' }
#' @export
tune_grid <- function(object, ...) {
  UseMethod("tune_grid")
}

#' @export
#' @rdname tune_grid
tune_grid.default <- function(object, ...) {
  stop("The first argument should be either a formula, recipe, or workflow.",
       call. = FALSE)
}

#' @export
#' @rdname tune_grid
tune_grid.recipe <- function(object, model, resamples, ..., param_info = NULL,
                             grid = 10, metrics = NULL, control = control_grid()) {
  if (is_missing(model) || !inherits(model, "model_spec")) {
    stop("`model` should be a parsnip model specification object.", call. = FALSE)
  }

  empty_ellipses(...)

  wflow <-
    workflow() %>%
    add_recipe(object) %>%
    add_model(model)

  tune_grid_workflow(
    wflow,
    resamples = resamples,
    grid = grid,
    metrics = metrics,
    pset = param_info,
    control = control
  )
}

#' @export
#' @rdname tune_grid
tune_grid.formula <- function(formula, model, resamples, ..., param_info = NULL,
                              grid = 10, metrics = NULL, control = control_grid()) {
  if (is_missing(model) || !inherits(model, "model_spec")) {
    stop("`model` should be a parsnip model specification object.", call. = FALSE)
  }

  empty_ellipses(...)

  wflow <-
    workflow() %>%
    add_formula(formula) %>%
    add_model(model)

  tune_grid_workflow(
    wflow,
    resamples = resamples,
    grid = grid,
    metrics = metrics,
    pset = param_info,
    control = control
  )
}

#' @export
#' @rdname tune_grid
tune_grid.workflow <- function(object, resamples, ..., param_info = NULL,
                               grid = 10, metrics = NULL, control = control_grid()) {

  empty_ellipses(...)

  tune_grid_workflow(
    object,
    resamples = resamples,
    grid = grid,
    metrics = metrics,
    pset = param_info,
    control = control
  )
}

# ------------------------------------------------------------------------------

tune_grid_workflow <-
  function(object, resamples, grid = 10, metrics = NULL, pset = NULL,
           control = control_grid()) {
    check_rset(resamples)
    metrics <- check_metrics(metrics, object)
    pset <- check_parameters(object, pset = pset, data = resamples$splits[[1]]$data)
    check_workflow(object, pset = pset)
    grid <- check_grid(grid, object, pset)

    code_path <- quarterback(object)

    resamples <- rlang::eval_tidy(code_path)

    all_bad <- is_cataclysmic(resamples)
    if (all_bad) {
      warning("All models failed in tune_grid(). See the `.notes` column.",
              call. = FALSE)
    }

    class(resamples) <- unique(c("tune_results", class(resamples)))
    resamples
  }

# ------------------------------------------------------------------------------

quarterback <- function(x) {
  y <- dials::parameters(x)
  sources <- unique(y$source)
  has_form <- has_preprocessor_formula(x)
  tune_rec <- any(sources == "recipe") & !has_form
  tune_model <- any(sources == "model_spec")

  args <- list(
    resamples = expr(resamples),
    grid = expr(grid),
    workflow = expr(object),
    metrics = expr(metrics),
    control = expr(control)
  )

  dplyr::case_when(
     tune_rec & !tune_model ~ rlang::call2("tune_rec", !!!args),
     tune_rec &  tune_model ~ rlang::call2("tune_rec_and_mod", !!!args),
     has_form &  tune_model ~ rlang::call2("tune_mod_with_formula", !!!args),
    !tune_rec &  tune_model ~ rlang::call2("tune_mod_with_recipe", !!!args),
     has_form & !tune_model ~ rlang::call2("tune_nothing_with_formula", !!!args),
     TRUE ~ rlang::call2("tune_nothing_with_recipe", !!!args)
  )
}

