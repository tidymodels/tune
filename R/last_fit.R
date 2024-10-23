#' Fit the final best model to the training set and evaluate the test set
#'
#' [last_fit()] emulates the process where, after determining the best model,
#' the final fit on the entire training set is needed and is then evaluated on
#' the test set.
#'
#' @param object A `parsnip` model specification or an unfitted
#'   [workflow()][workflows::workflow]. No tuning parameters are allowed; if arguments
#'   have been marked with [tune()][hardhat::tune], their values must be
#'   [finalized][finalize_model].
#'
#' @param preprocessor A traditional model formula or a recipe created using
#'   [recipes::recipe()].
#'
#' @param split An `rsplit` object created from [rsample::initial_split()] or
#' [rsample::initial_validation_split()].
#'
#' @param metrics A [yardstick::metric_set()], or `NULL` to compute a standard
#'   set of metrics.
#'
#' @param control A [control_last_fit()] object used to fine tune the last fit
#'   process.
#'
#' @inheritParams tune_grid
#'
#' @param add_validation_set For 3-way splits into training, validation, and test
#' set via [rsample::initial_validation_split()], should the validation set be
#' included in the data set used to train the model. If not, only the training
#' set is used.
#'
#' @param ... Currently unused.
#'
#' @details
#' This function is intended to be used after fitting a _variety of models_
#'  and the final tuning parameters (if any) have been finalized. The next step
#'  would be to fit using the entire training set and verify performance using
#'  the test data.
#'
#' @template case-weights
#' @template censored-regression
#'
#' @section See also:
#'
#' [last_fit()] is closely related to [fit_best()]. They both
#' give you access to a workflow fitted on the training data but are situated
#' somewhat differently in the modeling workflow. [fit_best()] picks up
#' after a tuning function like [tune_grid()] to take you from tuning results
#' to fitted workflow, ready for you to predict and assess further. [last_fit()]
#' assumes you have made your choice of hyperparameters and finalized your
#' workflow to then take you from finalized workflow to fitted workflow and
#' further to performance assessment on the test data. While [fit_best()] gives
#' a fitted workflow, [last_fit()] gives you the performance results. If you
#' want the fitted workflow, you can extract it from the result of [last_fit()]
#' via [extract_workflow()][extract_workflow.tune_results].
#'
#'
#' @return A single row tibble that emulates the structure of `fit_resamples()`.
#' However, a list column called `.workflow` is also attached with the fitted
#' model (and recipe, if any) that used the training set. Helper functions
#' for formatting tuning results like [collect_metrics()] and
#' [collect_predictions()] can be used with `last_fit()` output.
#' @examplesIf tune:::should_run_examples() & rlang::is_installed("splines2")
#' library(recipes)
#' library(rsample)
#' library(parsnip)
#'
#' set.seed(6735)
#' tr_te_split <- initial_split(mtcars)
#'
#' spline_rec <- recipe(mpg ~ ., data = mtcars) %>%
#'   step_spline_natural(disp)
#'
#' lin_mod <- linear_reg() %>%
#'   set_engine("lm")
#'
#' spline_res <- last_fit(lin_mod, spline_rec, split = tr_te_split)
#' spline_res
#'
#' # test set metrics
#' collect_metrics(spline_res)
#'
#' # test set predictions
#' collect_predictions(spline_res)
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
  cli::cli_abort(
    "The first argument to {.fn last_fit} should be either a model or workflow,
    not {.obj_type_friendly {object}}."
  )
}

#' @export
last_fit.model_fit <- function(object, ...) {
  cli::cli_abort(c(
    "{.help [{.fun last_fit}](tune::last_fit)} is not well-defined for \\
     fitted model objects.",
    "i" = "{.help [{.fun last_fit}](tune::last_fit)} takes \\
           a {.help [model specification](parsnip::model_spec)} or \\
           {.help [unfitted workflow](workflows::workflow)} as its first \\
           argument."
  ))
}

tune_pp_msg <- "To tune a model specification, you must preprocess with a
                formula, recipe, or {.pkg dplyr} selectors, not
                {.obj_type_friendly {preprocessor}}."

#' @export
#' @rdname last_fit
last_fit.model_spec <- function(object, preprocessor, split, ..., metrics = NULL,
                                eval_time = NULL, control = control_last_fit(),
                                add_validation_set = FALSE) {
  if (rlang::is_missing(preprocessor) || !is_preprocessor(preprocessor)) {
    cli::cli_abort(tune_pp_msg)
  }

  control <- parsnip::condense_control(control, control_last_fit())

  empty_ellipses(...)

  wflow <- add_model(workflow(), object)

  if (is_recipe(preprocessor)) {
    wflow <- add_recipe(wflow, preprocessor)
  } else if (rlang::is_formula(preprocessor)) {
    wflow <- add_formula(wflow, preprocessor)
  }

  last_fit_workflow(
    wflow,
    split = split,
    metrics = metrics,
    eval_time = eval_time,
    control = control,
    add_validation_set = add_validation_set
  )
}


#' @rdname last_fit
#' @export
last_fit.workflow <- function(object, split, ..., metrics = NULL,
                              eval_time = NULL, control = control_last_fit(),
                              add_validation_set = FALSE) {
  empty_ellipses(...)

  control <- parsnip::condense_control(control, control_last_fit())

  last_fit_workflow(
    object,
    split = split,
    metrics = metrics,
    eval_time = eval_time,
    control = control,
    add_validation_set = add_validation_set
  )
}


last_fit_workflow <- function(object,
                              split,
                              metrics,
                              eval_time = NULL,
                              control,
                              add_validation_set = FALSE,
                              ...,
                              call = rlang::caller_env()) {
  rlang::check_dots_empty()
  check_no_tuning(object)

  if (workflows::is_trained_workflow(object)) {
    cli::cli_abort(
      "`last_fit()` is not well-defined for a fitted workflow.",
      call = call
    )
  }

  if (inherits(split, "initial_validation_split")) {
    split <- prepare_validation_split(split, add_validation_set)
  }
  splits <- list(split)
  resamples <- rsample::manual_rset(splits, ids = "train/test split")

  # Turn off seed generation to ensure `last_fit()` and workflows `fit()`
  # are reproducible
  rng <- FALSE

  res <- resample_workflow(
    workflow = object,
    resamples = resamples,
    metrics = metrics,
    eval_time = eval_time,
    control = control,
    rng = rng,
    call = call
  )

  res$.workflow <- res$.extracts[[1]][[1]]
  res$.extracts <- NULL
  class(res) <- c("last_fit", class(res))
  class(res) <- unique(class(res))

  .stash_last_result(res)
  res
}


prepare_validation_split <- function(split, add_validation_set){
  if (add_validation_set) {
    # equivalent to (unexported) rsample:::rsplit() without checks
    split <- structure(
      list(
        data = split$data,
        in_id = c(split$train_id, split$val_id),
        out_id = NA
      ),
      class = "rsplit"
    )
  } else {
    id_train_test <- seq_len(nrow(split$data))[-sort(split$val_id)]
    id_train <- match(split$train_id, id_train_test)

    split <- structure(
      list(
        data = split$data[-sort(split$val_id), , drop = FALSE],
        in_id = id_train,
        out_id = NA
      ),
      class = "rsplit"
    )
  }

  split
}
