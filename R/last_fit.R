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
#' @param split An `rsplit` object created from [rsample::initial_split()].
#'
#' @param metrics A [yardstick::metric_set()], or `NULL` to compute a standard
#'   set of metrics.
#'
#' @param control A [control_last_fit()] object used to fine tune the last fit
#'   process.
#'
#' @param eval_time A numeric vector of time points where dynamic event time
#' metrics should be computed (e.g. the time-dependent ROC curve, etc). The
#' values should be non-negative and should probably be no greater then the
#' largest event time in the training set.
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
#' @examplesIf tune:::should_run_examples()
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
#'   workflow() %>%
#'   add_recipe(spline_rec) %>%
#'   add_model(lin_mod)
#'
#' last_fit(spline_wfl, split = tr_te_split)
#' @export
last_fit <- function(object, ...) {
  UseMethod("last_fit")
}

#' @export
last_fit.default <- function(object, ...) {
  msg <- paste0(
    "The first argument to [last_fit()] should be either ",
    "a model or workflow."
  )
  rlang::abort(msg)
}

#' @export
#' @rdname last_fit
last_fit.model_spec <- function(object, preprocessor, split, ..., metrics = NULL,
                                control = control_last_fit(), eval_time = NULL) {
  if (rlang::is_missing(preprocessor) || !is_preprocessor(preprocessor)) {
    rlang::abort(paste(
      "To tune a model spec, you must preprocess",
      "with a formula or recipe"
    ))
  }

  control <- parsnip::condense_control(control, control_last_fit())

  empty_ellipses(...)

  wflow <- add_model(workflow(), object)

  if (is_recipe(preprocessor)) {
    wflow <- add_recipe(wflow, preprocessor)
  } else if (rlang::is_formula(preprocessor)) {
    wflow <- add_formula(wflow, preprocessor)
  }

  last_fit_workflow(wflow, split, metrics, control, eval_time)
}


#' @rdname last_fit
#' @export
last_fit.workflow <- function(object, split, ..., metrics = NULL,
                              control = control_last_fit(), eval_time = NULL) {
  empty_ellipses(...)

  control <- parsnip::condense_control(control, control_last_fit())

  last_fit_workflow(object, split, metrics, control, eval_time)
}

last_fit_workflow <- function(object, split, metrics, control, eval_time = NULL) {
  check_no_tuning(object)
  splits <- list(split)
  resamples <- rsample::manual_rset(splits, ids = "train/test split")

  # Turn off seed generation to ensure `last_fit()` and workflows `fit()`
  # are reproducible
  rng <- FALSE

  res <- resample_workflow(
    workflow = object,
    resamples = resamples,
    metrics = metrics,
    control = control,
    eval_time = NULL,
    rng = rng
  )

  res$.workflow <- res$.extracts[[1]][[1]]
  res$.extracts <- NULL
  class(res) <- c("last_fit", class(res))
  class(res) <- unique(class(res))

  .stash_last_result(res)
  res
}
