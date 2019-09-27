#' Model tuning via grid search
#'
#' `tune_grid()` computes a set of perfomance metrics (e.g. accuracy or RMSE)
#'  for a pre-defined set of tuning parameters that correspond to a model or
#'  recipe across one or more resamples of the data.
#'
#' @param object A model workflow or recipe object.
#' @param formula A traditional model formula.
#' @param model A `parsnip` model specification (or `NULL` when `object` is a
#' workflow).
#' @param rs An `rset()` object. This argument __should be named__.
#' @param grid A data frame of tuning combinations or `NULL`. If used, this
#' argument __should be named__.
#' @param perf A `yardstick::metric_set()` or `NULL`. If used, this argument
#' __should be named__.
#' @param control An object used to modify the tuning process. If used, this
#' argument __should be named__.
#' @param ... Not currently used.
#' @return A tibble of results.
#'
#' @details
#'
#' Suppose there are _m_ tuning parameter combinations. `tune_grid()` may not
#' require all _m_ model/recipe fits across each resample. For example:
#'
#' \itemize{
#'   \item In cases where a single model fit can be used to make predictions
#'         for different parameter values in the grid, only one fit is used.
#'         For example, for some boosted trees, of 100 iterations of boosting
#'         are requested, the model object for 100 iterations can be used to
#'         make predictions on iterations less than 100 (if all other
#'         parameters are equal).
#'   \item When the model is being tuned in conjunction with pre-processing
#;         and/or post-processing parameters, the minimum number of fits are
#'.        used. For example, if the number of PCA components in a recipe step
#'         are being tuned over three values (along with model tuning
#'         parameters), only three recipes are are trained. The alternative
#'         would be to re-train the same recipe multiple times for each model
#'         tuning parameter.
#' }
#'
#' The `foreach` package is used here. To execute the resampling iterations in
#' parallel, register a parallel backend function. See the documentation for
#' `foreach::foreach()` for examples.
#'
#' For the most part, warnings generated during training are shown as they occur
#' and are associated with a specific resample when `control(verbose = TRUE)`.
#' They are (usually) not aggregated until the end of processing.
#'
#' @section Parameter Grids:
#'
#' If no tuning grid is provided, a semi-random grid (via
#' `dials::grid_latin_hypercube()`) is created with 10 candidate parameter
#' combinations.
#'
#' @section Performance Metrics:
#'
#' If no metric set is provided, one is created:
#' \itemize{
#'   \item For regression models, the root mean squared error and coefficient
#'         of determination are computed.
#'   \item For classification, the log-likelihood and overall accuracy are
#'         computed.
#' }
#'
#' Note that the metrics also determine what type of predictions are estimated
#' during tuning. For example, in a classification problem, of metrics are used
#' that are all associated with hard class predictions, the classification
#' probabilities are not created.
#'
#' The out-of-sample estimates of these metrics are contained in a list column
#' called `.metrics`. This tibble contains a row for each metric and columns
#' for the value, the estimator type, and so on.
#'
#' A `summarize()` method can be used for these objects to collapse the results
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
#'  parameters, the row number from the original data object (`.row`), the
#'  outcome data (with the same name(s) of the original data), and any columns
#'  created by the predictions. For example, for simple regression problems
#'  generates a column called `.pred` and so on. As noted above, the prediction
#'  columns that are returned are determined by the type of metric(s) requested.
#'
#' This list column can be `unnested` using `tidyr::unnest()` or using the
#'  convenience function `collect_predictions()`.
#'
#' @section Extracting information:
#'
#' The `extract` control option will result in an additional function to be
#'  returned called `.extracts`. This is a list column that has tibbles
#'  containing the results of the user's function for each tuning parameter
#'  combination. This can enable returning each model and/or recipe object that
#'  is created during resampling. Note that this could result in a large return
#'  object, depending on what is returned.
#'
#' Note that the function given to the `extract` argument is evaluated on
#'  every model that is _fit_ (as opposed to every model that is _evaluated_).
#' As noted above, in some cases, model predictions can be derived for
#'  sub-models so that, in these cases, not every row in the tuning parameter
#'  grid has a separate R object associated with it.
#'
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
tune_grid.recipe <- function(object, model, rs, grid = NULL,
                             perf = NULL, control = grid_control(), ...) {
  if (is_missing(model) || !inherits(model, "model_spec")) {
    stop("`model` should be a parsnip model specification object.", call. = FALSE)
  }

  wflow <-
    workflows::workflow() %>%
    workflows::add_recipe(object) %>%
    workflows::add_model(model)

  tune_grid_workflow(wflow, rs = rs, grid = grid, perf = perf, control = control)
}

#' @export
#' @rdname tune_grid
tune_grid.formula <- function(formula, model, rs, grid = NULL,
                             perf = NULL, control = grid_control(), ...) {
  if (is_missing(model) || !inherits(model, "model_spec")) {
    stop("`model` should be a parsnip model specification object.", call. = FALSE)
  }

  wflow <-
    workflows::workflow() %>%
    workflows::add_formula(formula) %>%
    workflows::add_model(model)

  tune_grid_workflow(wflow, rs = rs, grid = grid, perf = perf, control = control)
}

#' @export
#' @rdname tune_grid
tune_grid.workflow <- function(object, model = NULL, rs, grid = NULL,
                             perf = NULL, control = grid_control(), ...) {
  if (!is.null(model)) {
    stop("When using a workflow, `model` should be NULL.", call. = FALSE)
  }

  tune_grid_workflow(object, rs = rs, grid = grid, perf = perf, control = control)
}

# ------------------------------------------------------------------------------

tune_grid_workflow <- function(object, rs, grid = NULL, perf = NULL, control = grid_control()) {

  check_rset(rs)
  check_object(object)
  perf <- check_perf(perf, object)
  grid <- check_grid(grid, object)

  code_path <- quarterback(object)

  rs <- rlang::eval_tidy(code_path)

  all_bad <- is_cataclysmic(rs)
  if (all_bad) {
    warning(
      "All models failed in tune_grid().",
      call. = FALSE
      )
  }

  class(rs) <- c("tune_results", class(rs))
  rs
}

# ------------------------------------------------------------------------------

quarterback <- function(x) {
  y <- param_set(x)
  sources <- unique(y$source)
  has_form <- names(x$pre) == "formula_processor"
  tune_rec <- any(sources == "recipe") & !has_form
  tune_model <- any(sources == "model_spec")

  args <- list(splits = expr(rs), grid = expr(grid),
               wflow = expr(object), perf = expr(perf),
               ctrl = expr(control))
  dplyr::case_when(
     tune_rec & !tune_model ~ rlang::call2("tune_rec", !!!args),
     tune_rec &  tune_model ~ rlang::call2("tune_rec_and_mod", !!!args),
     has_form &  tune_model ~ rlang::call2("tune_mod_with_formula", !!!args),
    !tune_rec &  tune_model ~ rlang::call2("tune_mod_with_recipe", !!!args),
     TRUE ~ rlang::call2("tune_nothing")
  )
}

