#' Fit the final best model to the training set and evaluate the test set
#'
#' [last_fit()] emulates the process where, after determining the best model,
#' the final fit on the entire training set is needed and is then evaluated on
#' the test set.
#'
#' @param object A `parsnip` model specification or a [workflows::workflow()].
#'   No tuning parameters are allowed.
#'
#' @param preprocessor A traditional model formula or a recipe created using
#'   [recipes::recipe()].
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
#'
#' @details
#' This function is intended to be used after fitting a _variety of models_
#'  and the final tuning parameters (if any) have been finalized. The next step
#'  would be to fit using the entire training set and verify performance using
#'  the test data.
#' @return A single row tibble that emulates the structure of `fit_resamples()`.
#' However, a list column called `.workflow` is also attached with the fitted
#' model (and recipe, if any) that used the training set.
#' @examples
#' \donttest{
#' library(recipes)
#' library(rsample)
#' library(parsnip)
#'
#' set.seed(6735)
#' tr_te_split <- initial_split(mtcars)
#'
#' spline_rec <- recipe(mpg ~ ., data = mtcars) %>%
#'   step_ns(disp)
#'
#' lin_mod <- linear_reg() %>%
#'   set_engine("lm")
#'
#' spline_res <- last_fit(lin_mod, spline_rec, split = tr_te_split)
#' spline_res
#'
#' # test set results
#' spline_res$.metrics[[1]]
#'
#' # or use a workflow
#'
#' library(workflows)
#' spline_wfl <-
#'  workflow() %>%
#'  add_recipe(spline_rec) %>%
#'  add_model(lin_mod)
#'
#' last_fit(spline_wfl, split = tr_te_split)
#' }
#' @export
last_fit <- function(object, ...) {
  UseMethod("last_fit")
}

#' @export
last_fit.default <- function(object, ...) {
  empty_ellipses(...)
  msg <- paste0(
    "The first argument to [last_fit()] should be either ",
    "a model or workflow."
  )
  rlang::abort(msg)
}

#' @rdname last_fit
#' @export
last_fit.recipe <- function(object, model, split, ..., metrics = NULL) {
  lifecycle::deprecate_soft("0.0.2",
                            what = "last_fit.recipe()",
                            details = deprecate_msg(match.call(), "last_fit"))
  empty_ellipses(...)

  last_fit(model, preprocessor = object, split = split, metrics = metrics)

}

#' @rdname last_fit
#' @export
last_fit.formula <- function(formula, model, split, ..., metrics = NULL) {
  lifecycle::deprecate_soft("0.0.2",
                            what = "last_fit.formula()",
                            details = deprecate_msg(match.call(), "last_fit"))
  empty_ellipses(...)

  last_fit(model, preprocessor = formula, split = split, metrics = metrics)
}

#' @export
#' @rdname last_fit
last_fit.model_spec <- function(object, preprocessor, split, ..., metrics = NULL) {

  if (is_missing(preprocessor) || !(inherits(preprocessor, "recipe") || inherits(preprocessor, "formula"))) {
    rlang::abort("To fit the final best model, you must preprocess with a formula or recipe")
  }

  empty_ellipses(...)

  wflow <-
    workflow() %>%
    add_model(object)

  if (inherits(preprocessor, "recipe")) {
    wflow <-
      wflow %>%
      add_recipe(preprocessor)
  } else if (inherits(preprocessor, "formula")) {
    wflow <-
      wflow %>%
      add_formula(preprocessor)
  }

  last_fit_workflow(wflow, split, metrics)
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

