#' Fit the best model to the training set
#'
#' [last_fit()] emulates the process where, after determining the best model,
#' a final model is fit on the entire training set and is evaluated on the
#' test set.
#'
#' @param object A workflow, formula, or recipe. No tuning parameters are allowed.
#'
#' @param model A `parsnip` model specification. No tuning parameters are allowed.
#'
#' @param split An `rsplit` object created from [rsample::initial_split()].
#'
#' @param metrics A [yardstick::metric_set()], or `NULL` to compute a standard
#'   set of metrics.
#'
#' @param formula A formula specifying the terms of the model.
#'
#' @param ... Currently unused.
#' @return A single row tibble that emulates the structure of `fit_resamples()`.
#' @examples
#' library(recipes)
#' library(rsample)
#' library(parsnip)
#'
#' set.seed(6735)
#' tr_te_split <- initial_split(mtcars)
#'
#' # After fitting a _variety of models_, you've settled on a linear model that
#' # uses spline functions for a single variable. The next step is to fit the
#' # model on the entire training set and verify performance using the test data.
#'
#' spline_rec <- recipe(mpg ~ ., data = mtcars) %>%
#'   step_ns(disp)
#'
#' lin_mod <- linear_reg() %>%
#'   set_engine("lm")
#'
#' spline_res <- last_fit(spline_rec, lin_mod, split = tr_te_split)
#' spline_res
#'
#' # test set results
#' spline_res$.metrics[[1]]
#' @export
last_fit <- function(object, ...) {
  UseMethod("last_fit")
}

#' @export
last_fit.default <- function(object, ...) {
  empty_ellipses(...)
  msg <- paste0(
    "The first argument to [last_fit()] should be either a ",
    "formula, recipe, or workflow."
  )
  rlang::abort(msg)
}

#' @rdname last_fit
#' @export
last_fit.recipe <- function(object, model, split, ..., metrics = NULL) {
  empty_ellipses(...)
  if (is_missing(model) || !inherits(model, "model_spec")) {
    rlang::abort("`model` should be a parsnip model specification object.")
  }

  workflow <- workflow()
  workflow <- add_recipe(workflow, object)
  workflow <- add_model(workflow, model)

  last_fit_workflow(workflow, split, metrics)
}

#' @rdname last_fit
#' @export
last_fit.formula <- function(formula, model, split, ..., metrics = NULL) {
  empty_ellipses(...)
  if (is_missing(model) || !inherits(model, "model_spec")) {
    rlang::abort("`model` should be a parsnip model specification object.")
  }

  workflow <- workflow()
  workflow <- add_formula(workflow, formula)
  workflow <- add_model(workflow, model)

  last_fit_workflow(workflow, split, metrics)
}

#' @rdname last_fit
#' @export
last_fit.workflow <- function(object, split, ..., metrics = NULL) {
  empty_ellipses(...)
  last_fit_workflow(object, split, metrics)
}

split_to_rset <- function(x) {
  prop <- length(x$in_id)/nrow(x$data)
  res <- rsample::mc_cv(x$data, times = 1, prop = prop)
  res$splits[[1]] <- x
  res
}

last_fit_workflow <- function(object, split, metrics) {
  extr <- function(x)
    x
  ctrl <- control_resamples(save_pred = TRUE, extract = extr)
  res <-
    fit_resamples(
      object,
      resamples = split_to_rset(split),
      metrics = metrics,
      control = ctrl
    )
  res$id[[1]] <- "train/test split"
  res$.workflow <- res$.extracts[[1]][[1]]
  res$.extracts <- NULL
  class(res) <- c("last_fit", class(res))
  class(res) <- unique(class(res))
  res
}

empty_ellipses <- function(...) {
  dots <- rlang::enquos(...)
  if (length(dots) > 0) {
    msg <- "The `...` are not used in this function but one or more objects were passed: "
    msg <- paste0(msg, paste0("'", names(dots), "'", collapse = ", "))
    rlang::warn(msg)
  }
  invisible(NULL)
}

