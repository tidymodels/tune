#' Fit multiple models via resampling
#'
#' `fit_resamples()` computes a set of performance metrics across one or more
#' resamples. It does not perform any tuning (see [tune_grid()] and
#' [tune_bayes()] for that), and is instead used for fitting a single
#' model+recipe or model+formula combination across many resamples.
#'
#' @param object A workflow, formula, or recipe.
#'
#' @param model A `parsnip` model specification.
#'
#' @param resamples A resample `rset` created from an `rsample` function such
#'   as [rsample::vfold_cv()].
#'
#' @param metrics A [yardstick::metric_set()], or `NULL` to compute a standard
#'   set of metrics.
#'
#' @param control A `control_resamples()` object used to fine tune the resampling
#'   process.
#'
#' @param formula A formula specifying the terms of the model.
#'
#' @inheritSection tune_grid Performance Metrics
#' @inheritSection tune_grid Obtaining Predictions
#' @inheritSection tune_grid Extracting information
#'
#' @examples
#' library(recipes)
#' library(rsample)
#' library(parsnip)
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
#' spline_res <- fit_resamples(spline_rec, lin_mod, folds, control = control)
#'
#' spline_res
#'
#' show_best(spline_res, metric = "rmse", maximize = FALSE)
#'
#' @export
fit_resamples <- function(object, ...) {
  UseMethod("fit_resamples")
}

#' @export
fit_resamples.default <- function(object, ...) {
  msg <- paste0(
    "The first argument to `fit_resamples()` should be either a ",
    "formula, recipe, or workflow."
  )
  rlang::abort(msg)
}

#' @rdname fit_resamples
#' @export
fit_resamples.recipe <- function(object,
                                 model,
                                 resamples,
                                 ...,
                                 metrics = NULL,
                                 control = control_resamples()) {

  if (is_missing(model) || !inherits(model, "model_spec")) {
    rlang::abort("`model` should be a parsnip model specification object.")
  }

  workflow <- workflow()
  workflow <- add_recipe(workflow, object)
  workflow <- add_model(workflow, model)

  resample_workflow(workflow, resamples, metrics, control)
}

#' @rdname fit_resamples
#' @export
fit_resamples.formula <- function(formula,
                                  model,
                                  resamples,
                                  ...,
                                  metrics = NULL,
                                  control = control_resamples()) {

  if (is_missing(model) || !inherits(model, "model_spec")) {
    rlang::abort("`model` should be a parsnip model specification object.")
  }

  workflow <- workflow()
  workflow <- add_formula(workflow, formula)
  workflow <- add_model(workflow, model)

  resample_workflow(workflow, resamples, metrics, control)
}

#' @rdname fit_resamples
#' @export
fit_resamples.workflow <- function(object,
                                   resamples,
                                   ...,
                                   metrics = NULL,
                                   control = control_resamples()) {

  resample_workflow(object, resamples, metrics, control)
}

# ------------------------------------------------------------------------------

resample_workflow <- function(workflow, resamples, metrics, control) {
  check_rset(resamples)
  check_object(workflow)
  metrics <- check_metrics(metrics, workflow)

  has_formula <- has_workflow_formula(workflow)

  if (has_formula) {
    resamples <- resample_with_formula(resamples, workflow, metrics, control)
  } else {
    resamples <- resample_with_recipe(resamples, workflow, metrics, control)
  }

  if (is_cataclysmic(resamples)) {
    rlang::warn(
      "All models failed in `fit_resamples()`. See the `.notes` column."
    )
  }

  # TODO - what class?
  class(resamples) <- c("resample_results", class(resamples))

  resamples
}

has_workflow_formula <- function(x) {
  names(x$pre) == "formula_processor"
}

# ------------------------------------------------------------------------------

resample_with_recipe <- function(resamples, workflow, metrics, control) {
  B <- nrow(resamples)

  `%op%` <- get_operator(control$allow_par, workflow)

  lab_names <- names(labels(resamples$splits[[1]]))

  results <- foreach::foreach(rs_iter = 1:B, .packages = "tune", .errorhandling = "pass") %op%
    iter_resample_with_recipe(rs_iter, resamples, workflow, metrics, control)

  resamples <- pull_metrics(resamples, results, control)
  resamples <- pull_notes(resamples, results, control)
  resamples <- pull_extracts(resamples, results, control)
  resamples <- pull_predictions(resamples, results, control)

  resamples
}

# TODO - use fit.workflow and predict.workflow
iter_resample_with_recipe <- function(rs_iter, resamples, workflow, metrics, control) {
  load_pkgs(workflow)
  load_namespace(control$pkgs)
  fit_control <- parsnip::fit_control(verbosity = 0, catch = TRUE)

  split <- resamples$splits[[rs_iter]]
  metric_est <- NULL
  extracted <- NULL
  pred_vals <- NULL
  .notes <- NULL

  tmp_rec <- catch_and_log(
    train_recipe(split, workflow, NULL),
    control,
    split,
    "recipe",
    notes = .notes
  )

  if (inherits(tmp_rec, "try-error")) {
    out <- list(
      .metrics = metric_est,
      .extracts = extracted,
      .predictions = pred_vals,
      .notes = .notes
    )

    return(out)
  }

  tmp_fit <- catch_and_log(
    train_model_from_recipe(workflow, tmp_rec, NULL, control = fit_control),
    control,
    split,
    "model",
    notes = .notes
  )

  if (inherits(tmp_fit, "try-error")) {
    out <- list(
      .metrics = metric_est,
      .extracts = extracted,
      .predictions = pred_vals,
      .notes = .notes
    )

    return(out)
  }

  # Dummy tbl with no columns but the correct number of rows to `bind_cols()`
  # against in `append_extracts()`
  split_label_tbl <- labels(split)
  dummy_param_tbl <- tibble(.rows = nrow(split_label_tbl))

  extracted <- append_extracts(extracted, tmp_rec, tmp_fit$fit, dummy_param_tbl, split, control)

  tmp_pred <- catch_and_log(
    predict_model_from_recipe_no_grid(split, tmp_fit, tmp_rec, metrics),
    control,
    split,
    "model (predictions)",
    bad_only = TRUE,
    notes = .notes
  )

  if (inherits(tmp_pred, "try-error")) {
    out <- list(
      .metrics = metric_est,
      .extracts = extracted,
      .predictions = pred_vals,
      .notes = .notes
    )

    return(out)
  }

  metric_est <- append_metrics(metric_est, tmp_pred, workflow, metrics, split)
  pred_vals <- append_predictions(pred_vals, tmp_pred, split, control)

  list(.metrics = metric_est, .extracts = extracted, .predictions = pred_vals, .notes = .notes)
}

# ------------------------------------------------------------------------------

resample_with_formula <- function(resamples, workflow, metrics, control) {
  B <- nrow(resamples)

  `%op%` <- get_operator(control$allow_par, workflow)

  lab_names <- names(labels(resamples$splits[[1]]))

  results <- foreach::foreach(rs_iter = 1:B, .packages = "tune", .errorhandling = "pass") %op%
    iter_resample_with_formula(rs_iter, resamples, workflow, metrics, control)

  resamples <- pull_metrics(resamples, results, control)
  resamples <- pull_notes(resamples, results, control)
  resamples <- pull_extracts(resamples, results, control)
  resamples <- pull_predictions(resamples, results, control)

  resamples
}

# TODO - use fit.workflow and predict.workflow
iter_resample_with_formula <- function(rs_iter, resamples, workflow, metrics, control) {
  load_pkgs(workflow)
  load_namespace(control$pkgs)
  fit_control <- parsnip::fit_control(verbosity = 0, catch = TRUE)

  split <- resamples$splits[[rs_iter]]
  metric_est <- NULL
  extracted <- NULL
  pred_vals <- NULL
  .notes <- NULL

  tmp_df <- catch_and_log(
    exec_formula(split, workflow),
    control,
    split,
    "formula",
    notes = .notes
  )

  if (inherits(tmp_df, "try-error")) {
    out <- list(
      .metrics = metric_est,
      .extracts = extracted,
      .predictions = pred_vals,
      .notes = .notes
    )

    return(out)
  }

  tmp_terms <- tmp_df$terms
  tmp_df <- tmp_df[c("x", "y")]

  tmp_fit <- catch_and_log(
    train_model_from_df(workflow, tmp_df, NULL, control = fit_control),
    control,
    split,
    "model",
    notes = .notes
  )

  if (inherits(tmp_fit, "try-error")) {
    out <- list(
      .metrics = metric_est,
      .extracts = extracted,
      .predictions = pred_vals,
      .notes = .notes
    )

    return(out)
  }

  # Dummy tbl with no columns but the correct number of rows to `bind_cols()`
  # against in `append_extracts()`
  split_label_tbl <- labels(split)
  dummy_param_tbl <- tibble(.rows = nrow(split_label_tbl))

  extracted <- append_extracts(extracted, NULL, tmp_fit$fit, dummy_param_tbl, split, control)

  tmp_pred <- catch_and_log(
    predict_model_from_terms_no_grid(split, tmp_fit, tmp_terms, metrics),
    control,
    split,
    "model (predictions)",
    bad_only = TRUE,
    notes = .notes
  )

  if (inherits(tmp_pred, "try-error")) {
    out <- list(
      .metrics = metric_est,
      .extracts = extracted,
      .predictions = pred_vals,
      .notes = .notes
    )

    return(out)
  }

  metric_est <- append_metrics(metric_est, tmp_pred, workflow, metrics, split)
  pred_vals <- append_predictions(pred_vals, tmp_pred, split, control)

  list(.metrics = metric_est, .extracts = extracted, .predictions = pred_vals, .notes = .notes)
}

# ------------------------------------------------------------------------------
# `resamples()` prediction

predict_model_from_recipe_no_grid <- function(split, model, recipe, metrics) {
  y_names <- outcome_names(recipe)

  new_vals <- recipes::bake(
    recipe,
    rsample::assessment(split),
    all_predictors(),
    all_outcomes()
  )

  x_vals <- dplyr::select(new_vals, -one_of(y_names))

  orig_rows <- as.integer(split, data = "assessment")

  # Determine the type of prediction that is required
  type_info <- metrics_info(metrics)
  types <- unique(type_info$type)

  res <- NULL

  for (type_iter in types) {
    tmp_res <- predict(model, x_vals, type = type_iter) %>%
      mutate(.row = orig_rows)

    if (!is.null(res)) {
      res <- dplyr::full_join(res, tmp_res, by = ".row")
    } else {
      res <- tmp_res
    }

    rm(tmp_res)
  }

  # Add outcome data
  outcome_data <- dplyr::select(new_vals, dplyr::one_of(y_names))
  outcome_data <- dplyr::mutate(outcome_data, .row = orig_rows)

  res <- dplyr::full_join(res, outcome_data, by = ".row")

  tibble::as_tibble(res)
}

predict_model_from_terms_no_grid <- function(split, model, terms, metrics) {
  data <- exec_terms(split, terms)

  orig_rows <- as.integer(split, data = "assessment")

  # Determine the type of prediction that is required
  type_info <- metrics_info(metrics)
  types <- unique(type_info$type)

  res <- NULL

  for (type_iter in types) {
    tmp_res <- predict(model, data$x, type = type_iter) %>%
      mutate(.row = orig_rows)

    if (!is.null(res)) {
      res <- dplyr::full_join(res, tmp_res, by = ".row")
    } else {
      res <- tmp_res
    }

    rm(tmp_res)
  }

  # Add outcome data
  outcome_data <- dplyr::mutate(data$y, .row = orig_rows)

  res <- dplyr::full_join(res, outcome_data, by = ".row")

  tibble::as_tibble(res)
}
