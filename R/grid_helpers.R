trim_ipcw <- function(x) {
  x$.weight_time <- NULL
  x$.pred_censored <- NULL
  x
}

maybe_add_ipcw <- function(.data, model, types) {
  if (!any(types == "survival")) {
    return(.data)
  }
  res <- parsnip::.censoring_weights_graf(model, .data)
  res$.pred <- purrr::map(res$.pred, trim_ipcw)
  res
}

#' Get time for analysis of dynamic survival metrics
#' @param metrics A metric set.
#' @param eval_time A vector of evaluation times.
#' @export
#' @keywords internal
get_metric_time <- function(metrics, eval_time) {
  info <- tibble::as_tibble(metrics)
  if (any(info$class == "dynamic_survival_metric")) {
    eval_time <- eval_time[1]
  } else {
    eval_time <- NULL
  }
  eval_time
}

predict_wrapper <- function(model, new_data, type, eval_time, subgrid = NULL) {
  if (is.null(subgrid)) {
    fn <- "predict.model_fit"
  } else {
    fn <- "multi_predict"
  }

  cl <-
    rlang::call2(
      fn,
      .ns = "parsnip",
      object = rlang::expr(model),
      new_data = rlang::expr(new_data),
      type = type
    )

  # Add in censored regression evaluation times (if needed)
  has_type <- type %in% dyn_surv_types
  if (
    model$spec$mode == "censored regression" & !is.null(eval_time) & has_type
  ) {
    cl <- rlang::call_modify(cl, eval_time = eval_time)
  }

  # When there are sub-models:
  if (!is.null(subgrid)) {
    cl <- rlang::call_modify(cl, !!!subgrid)
  }
  res <- rlang::eval_tidy(cl)

  res
}

# ------------------------------------------------------------------------------

#' @export
#' @rdname tune-internal-functions
forge_from_workflow <- function(new_data, workflow) {
  blueprint <- workflow$pre$mold$blueprint
  forged <- hardhat::forge(new_data, blueprint, outcomes = TRUE)

  forged
}

get_metrics_by <- function(metric_set) {
  metrics <- attr(metric_set, "metrics")
  metrics_by <- purrr::map(metrics, attr, "by")
  unique(unlist(metrics_by, use.names = FALSE))
}

# metrics_by is the output of `get_metrics_by()`---it's assumed that wherever
# `has_metrics_by()` is needed, `get_metrics_by()` output will be needed too.
has_metrics_by <- function(metrics_by) {
  length(metrics_by) > 0
}

# ------------------------------------------------------------------------------

#' @export
#' @rdname tune-internal-functions
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `finalize_workflow_preprocessor()` is deprecated.
finalize_workflow_preprocessor <- function(workflow, grid_preprocessor) {
  lifecycle::deprecate_warn("2.1.0", "finalize_workflow_preprocessor()")

  # Already finalized, nothing to tune
  if (ncol(grid_preprocessor) == 0L) {
    return(workflow)
  }

  recipe <- extract_preprocessor(workflow)
  recipe <- merge(recipe, grid_preprocessor)$x[[1]]

  workflow <- .set_workflow_recipe(workflow, recipe)

  workflow
}

# ------------------------------------------------------------------------------

#' @export
#' @keywords internal
#' @rdname empty_ellipses
.has_preprocessor <- function(workflow) {
  .has_preprocessor_recipe(workflow) ||
    .has_preprocessor_formula(workflow) ||
    .has_preprocessor_variables(workflow)
}

#' @export
#' @keywords internal
#' @rdname empty_ellipses
.has_preprocessor_recipe <- function(workflow) {
  "recipe" %in% names(workflow$pre$actions)
}

#' @export
#' @keywords internal
#' @rdname empty_ellipses
.has_preprocessor_formula <- function(workflow) {
  "formula" %in% names(workflow$pre$actions)
}

#' @export
#' @keywords internal
#' @rdname empty_ellipses
.has_preprocessor_variables <- function(workflow) {
  "variables" %in% names(workflow$pre$actions)
}

has_postprocessor <- function(workflow) {
  "tailor" %in% names(workflow$post$actions)
}

has_case_weights <- function(workflow) {
  "case_weights" %in% names(workflow$pre$actions)
}

#' @export
#' @keywords internal
#' @rdname empty_ellipses
.has_spec <- function(workflow) {
  "model" %in% names(workflow$fit$actions)
}

#' @export
#' @keywords internal
#' @rdname empty_ellipses
.set_workflow_spec <- function(workflow, spec) {
  workflow$fit$actions$model$spec <- spec
  workflow
}

#' @export
#' @keywords internal
#' @rdname empty_ellipses
.set_workflow_recipe <- function(workflow, recipe) {
  workflow$pre$actions$recipe$recipe <- recipe
  workflow
}

set_workflow_tailor <- function(workflow, tailor) {
  workflow$post$actions$tailor$tailor <- tailor
  workflow
}
