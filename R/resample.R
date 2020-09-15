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

  if (has_preprocessor_formula(workflow)) {
    resamples <- resample_with_formula(resamples, workflow, metrics, control)
  } else if (has_preprocessor_recipe(workflow)) {
    resamples <- resample_with_recipe(resamples, workflow, metrics, control)
  } else if (has_preprocessor_variables(workflow)) {
    resamples <- resample_with_variables(resamples, workflow, metrics, control)
  } else {
    rlang::abort("Internal error: `workflow` should have been checked for a preprocessor by now.")
  }

  if (is_cataclysmic(resamples)) {
    rlang::warn(
      "All models failed in [fit_resamples()]. See the `.notes` column."
    )
  }

  outcomes <- reduce_all_outcome_names(resamples)
  resamples[[".all_outcome_names"]] <- NULL

  workflow_output <- set_workflow(workflow, control)

  resamples <- resamples %>% dplyr::select(-.seed)
  new_resample_results(
    x = resamples,
    parameters = parameters(workflow),
    metrics = metrics,
    outcomes = outcomes,
    rset_info = rset_info,
    workflow = workflow_output
  )
}

# ------------------------------------------------------------------------------

resample_with_recipe <- function(resamples, workflow, metrics, control) {
  B <- nrow(resamples)

  `%op%` <- get_operator(control$allow_par, workflow)

  lab_names <- names(labels(resamples$splits[[1]]))

  safely_iter_resample_with_recipe <- super_safely_iterate(iter_resample_with_recipe)

  results <-
    foreach::foreach(rs_iter = 1:B,
                     .packages = required_pkgs(workflow),
                     .errorhandling = "pass") %op% {
                       safely_iter_resample_with_recipe(
                         rs_iter = rs_iter,
                         resamples = resamples,
                         grid = NULL,
                         workflow = workflow,
                         metrics = metrics,
                         control = control
                       )
                     }

  resamples <- pull_metrics(resamples, results, control)
  resamples <- pull_notes(resamples, results, control)
  resamples <- pull_extracts(resamples, results, control)
  resamples <- pull_predictions(resamples, results, control)
  resamples <- pull_all_outcome_names(resamples, results)

  resamples
}

iter_resample_with_recipe <- function(rs_iter, resamples, workflow, metrics, control) {
  load_pkgs(workflow)
  load_namespace(control$pkgs)
  set.seed(resamples$.seed[[rs_iter]])

  control_parsnip <- parsnip::control_parsnip(verbosity = 0, catch = TRUE)
  control_workflow <- control_workflow(control_parsnip = control_parsnip)

  split <- resamples$splits[[rs_iter]]
  metric_est <- NULL
  extracted <- NULL
  pred_vals <- NULL
  all_outcome_names <- list()
  .notes <- NULL

  workflow <- catch_and_log(
    train_recipe(split, workflow, NULL),
    control,
    split,
    "recipe",
    notes = .notes
  )

  if (is_failure(workflow)) {
    out <- list(
      .metrics = metric_est,
      .extracts = extracted,
      .predictions = pred_vals,
      .all_outcome_names = all_outcome_names,
      .notes = .notes
    )

    return(out)
  }

  workflow <- catch_and_log_fit(
    train_model(workflow, NULL, control = control_workflow),
    control,
    split,
    "model",
    notes = .notes
  )

  # check for parsnip level and model level failure
  if (is_failure(workflow) || is_failure(workflow$fit$fit$fit)) {
    out <- list(
      .metrics = metric_est,
      .extracts = extracted,
      .predictions = pred_vals,
      .all_outcome_names = all_outcome_names,
      .notes = .notes
    )

    return(out)
  }

  # Extract names from the mold
  all_outcome_names <- append_outcome_names(all_outcome_names, workflow)

  # Dummy tbl with no columns but the correct number of rows to `bind_cols()`
  # against in `append_extracts()`
  split_label_tbl <- labels(split)
  dummy_param_tbl <- tibble(.rows = nrow(split_label_tbl))

  extracted <- append_extracts(extracted, workflow, dummy_param_tbl, split, control)

  predictions <- catch_and_log(
    predict_model_no_grid(split, workflow, metrics),
    control,
    split,
    "model (predictions)",
    bad_only = TRUE,
    notes = .notes
  )

  if (is_failure(predictions)) {
    out <- list(
      .metrics = metric_est,
      .extracts = extracted,
      .predictions = pred_vals,
      .all_outcome_names = all_outcome_names,
      .notes = .notes
    )

    return(out)
  }

  metric_est <- append_metrics(metric_est, predictions, workflow, metrics, split)
  pred_vals <- append_predictions(pred_vals, predictions, split, control)

  list(
    .metrics = metric_est,
    .extracts = extracted,
    .predictions = pred_vals,
    .all_outcome_names = all_outcome_names,
    .notes = .notes
  )
}

# ------------------------------------------------------------------------------

resample_with_formula <- function(resamples, workflow, metrics, control) {
  B <- nrow(resamples)

  `%op%` <- get_operator(control$allow_par, workflow)

  lab_names <- names(labels(resamples$splits[[1]]))

  safely_iter_resample_with_formula <- super_safely_iterate(iter_resample_with_formula)

  results <-
    foreach::foreach(rs_iter = 1:B,
                     .packages = required_pkgs(workflow),
                     .errorhandling = "pass") %op% {
                       safely_iter_resample_with_formula(
                         rs_iter = rs_iter,
                         resamples = resamples,
                         grid = NULL,
                         workflow = workflow,
                         metrics = metrics,
                         control = control
                       )
                     }

  resamples <- pull_metrics(resamples, results, control)
  resamples <- pull_notes(resamples, results, control)
  resamples <- pull_extracts(resamples, results, control)
  resamples <- pull_predictions(resamples, results, control)
  resamples <- pull_all_outcome_names(resamples, results)

  resamples
}

iter_resample_with_formula <- function(rs_iter, resamples, workflow, metrics, control) {
  load_pkgs(workflow)
  load_namespace(control$pkgs)
  set.seed(resamples$.seed[[rs_iter]])

  control_parsnip <- parsnip::control_parsnip(verbosity = 0, catch = TRUE)
  control_workflow <- control_workflow(control_parsnip = control_parsnip)

  split <- resamples$splits[[rs_iter]]
  metric_est <- NULL
  extracted <- NULL
  pred_vals <- NULL
  all_outcome_names <- list()
  .notes <- NULL

  workflow <- catch_and_log(
    train_formula(split, workflow),
    control,
    split,
    "formula",
    notes = .notes
  )

  if (is_failure(workflow)) {
    out <- list(
      .metrics = metric_est,
      .extracts = extracted,
      .predictions = pred_vals,
      .all_outcome_names = all_outcome_names,
      .notes = .notes
    )

    return(out)
  }

  workflow <- catch_and_log_fit(
    train_model(workflow, NULL, control = control_workflow),
    control,
    split,
    "model",
    notes = .notes
  )

  # check for parsnip level and model level failure
  if (is_failure(workflow) || is_failure(workflow$fit$fit$fit)) {
    out <- list(
      .metrics = metric_est,
      .extracts = extracted,
      .predictions = pred_vals,
      .all_outcome_names = all_outcome_names,
      .notes = .notes
    )

    return(out)
  }

  # Extract names from the mold
  all_outcome_names <- append_outcome_names(all_outcome_names, workflow)

  # Dummy tbl with no columns but the correct number of rows to `bind_cols()`
  # against in `append_extracts()`
  split_label_tbl <- labels(split)
  dummy_param_tbl <- tibble(.rows = nrow(split_label_tbl))

  extracted <- append_extracts(extracted, workflow, dummy_param_tbl, split, control)

  predictions <- catch_and_log(
    predict_model_no_grid(split, workflow, metrics),
    control,
    split,
    "model (predictions)",
    bad_only = TRUE,
    notes = .notes
  )

  if (is_failure(predictions)) {
    out <- list(
      .metrics = metric_est,
      .extracts = extracted,
      .predictions = pred_vals,
      .all_outcome_names = all_outcome_names,
      .notes = .notes
    )

    return(out)
  }

  metric_est <- append_metrics(metric_est, predictions, workflow, metrics, split)
  pred_vals <- append_predictions(pred_vals, predictions, split, control)

  list(
    .metrics = metric_est,
    .extracts = extracted,
    .predictions = pred_vals,
    .all_outcome_names = all_outcome_names,
    .notes = .notes
  )
}

# ------------------------------------------------------------------------------

resample_with_variables <- function(resamples, workflow, metrics, control) {
  B <- nrow(resamples)

  `%op%` <- get_operator(control$allow_par, workflow)

  safely_iter_resample_with_variables <- super_safely_iterate(iter_resample_with_variables)

  results <- foreach::foreach(rs_iter = 1:B,
                              .packages = required_pkgs(workflow),
                              .errorhandling = "pass") %op% {
    safely_iter_resample_with_variables(
      rs_iter = rs_iter,
      resamples = resamples,
      grid = NULL,
      workflow = workflow,
      metrics = metrics,
      control = control
    )
  }

  resamples <- pull_metrics(resamples, results, control)
  resamples <- pull_notes(resamples, results, control)
  resamples <- pull_extracts(resamples, results, control)
  resamples <- pull_predictions(resamples, results, control)
  resamples <- pull_all_outcome_names(resamples, results)

  resamples
}

iter_resample_with_variables <- function(rs_iter, resamples, workflow, metrics, control) {
  load_pkgs(workflow)
  load_namespace(control$pkgs)
  set.seed(resamples$.seed[[rs_iter]])

  control_parsnip <- parsnip::control_parsnip(verbosity = 0, catch = TRUE)
  control_workflow <- control_workflow(control_parsnip = control_parsnip)

  split <- resamples$splits[[rs_iter]]
  metric_est <- NULL
  extracted <- NULL
  pred_vals <- NULL
  all_outcome_names <- list()
  .notes <- NULL

  workflow <- catch_and_log(
    train_variables(split, workflow),
    control,
    split,
    "variables",
    notes = .notes
  )

  if (is_failure(workflow)) {
    out <- list(
      .metrics = metric_est,
      .extracts = extracted,
      .predictions = pred_vals,
      .all_outcome_names = all_outcome_names,
      .notes = .notes
    )

    return(out)
  }

  workflow <- catch_and_log_fit(
    train_model(workflow, NULL, control = control_workflow),
    control,
    split,
    "model",
    notes = .notes
  )

  # check for parsnip level and model level failure
  if (is_failure(workflow) || is_failure(workflow$fit$fit$fit)) {
    out <- list(
      .metrics = metric_est,
      .extracts = extracted,
      .predictions = pred_vals,
      .all_outcome_names = all_outcome_names,
      .notes = .notes
    )

    return(out)
  }

  # Extract names from the mold
  all_outcome_names <- append_outcome_names(all_outcome_names, workflow)

  # Dummy tbl with no columns but the correct number of rows to `bind_cols()`
  # against in `append_extracts()`
  split_label_tbl <- labels(split)
  dummy_param_tbl <- tibble(.rows = nrow(split_label_tbl))

  extracted <- append_extracts(extracted, workflow, dummy_param_tbl, split, control)

  predictions <- catch_and_log(
    predict_model_no_grid(split, workflow, metrics),
    control,
    split,
    "model (predictions)",
    bad_only = TRUE,
    notes = .notes
  )

  if (is_failure(predictions)) {
    out <- list(
      .metrics = metric_est,
      .extracts = extracted,
      .predictions = pred_vals,
      .all_outcome_names = all_outcome_names,
      .notes = .notes
    )

    return(out)
  }

  metric_est <- append_metrics(metric_est, predictions, workflow, metrics, split)
  pred_vals <- append_predictions(pred_vals, predictions, split, control)

  list(
    .metrics = metric_est,
    .extracts = extracted,
    .predictions = pred_vals,
    .all_outcome_names = all_outcome_names,
    .notes = .notes
  )
}

# ------------------------------------------------------------------------------
# `resamples()` prediction

predict_model_no_grid <- function(split, workflow, metrics) {
  model <- workflows::pull_workflow_fit(workflow)

  forged <- forge_from_workflow(split, workflow)

  x_vals <- forged$predictors
  y_vals <- forged$outcomes

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
  y_vals <- dplyr::mutate(y_vals, .row = orig_rows)
  res <- dplyr::full_join(res, y_vals, by = ".row")

  tibble::as_tibble(res)
}

forge_from_workflow <- function(split, workflow) {
  new_data <- rsample::assessment(split)

  blueprint <- workflow$pre$mold$blueprint
  forged <- hardhat::forge(new_data, blueprint, outcomes = TRUE)

  forged
}
