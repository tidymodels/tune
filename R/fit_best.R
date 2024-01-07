#' Fit a model to the numerically optimal configuration
#'
#' `fit_best()` takes the results from model tuning and fits it to the training
#' set using tuning parameters associated with the best performance.
#'
#' @param x The results of class `tune_results` (coming from functions such as
#' [tune_grid()], [tune_bayes()], etc). The control option
#' [`save_workflow = TRUE`][tune::control_grid] should have been used.
#' @param metric A character string (or `NULL`) for which metric to optimize. If
#' `NULL`, the first metric is used.
#' @param eval_time A numeric vector of time points where dynamic event time
#' metrics should be chosen (e.g., the time-dependent ROC curve, etc). The
#' values should be consistent with the values used to create `x`. If the
#' analysis used a dynamic metric, the `NULL` default will automatically use the
#' first evaluation time used by `x`.
#' @param parameters An optional 1-row tibble of tuning parameter settings, with
#' a column for each tuning parameter. This tibble should have columns for each
#' tuning parameter identifier (e.g. `"my_param"` if `tune("my_param")` was used).
#' If `NULL`, this argument will be set to
#' [`select_best(metric)`][tune::select_best.tune_results].
#' @param verbose A logical for printing logging.
#' @param add_validation_set When the resamples embedded in `x` are a split into
#' training set and validation set, should the validation set be included in the
#' data set used to train the model? If not, only the training set is used. If
#' `NULL`, the validation set is not used for resamples originating from
#' [rsample::validation_set()] while it is used for resamples originating
#' from [rsample::validation_split()].
#' @param ... Not currently used.
#' @details
#' This function is a shortcut for the manual steps of:
#'
#' \preformatted{
#'   best_param <- select_best(tune_results, metric) # or other `select_*()`
#'   wflow <- finalize_workflow(wflow, best_param)  # or just `finalize_model()`
#'   wflow_fit <- fit(wflow, data_set)
#' }
#'
#' @inheritSection last_fit See also
#'
#' @examplesIf tune:::should_run_examples()
#' library(recipes)
#' library(rsample)
#' library(parsnip)
#' library(dplyr)
#'
#' data(meats, package = "modeldata")
#' meats <- meats %>% select(-water, -fat)
#'
#' set.seed(1)
#' meat_split <- initial_split(meats)
#' meat_train <- training(meat_split)
#' meat_test  <- testing(meat_split)
#'
#' set.seed(2)
#' meat_rs <- vfold_cv(meat_train, v = 10)
#'
#' pca_rec <-
#'   recipe(protein ~ ., data = meat_train) %>%
#'   step_normalize(all_numeric_predictors()) %>%
#'   step_pca(all_numeric_predictors(), num_comp = tune())
#'
#' knn_mod <- nearest_neighbor(neighbors = tune()) %>% set_mode("regression")
#'
#' ctrl <- control_grid(save_workflow = TRUE)
#'
#' set.seed(128)
#' knn_pca_res <-
#'   tune_grid(knn_mod, pca_rec, resamples = meat_rs, grid = 10, control = ctrl)
#'
#' knn_fit <- fit_best(knn_pca_res, verbose = TRUE)
#' predict(knn_fit, meat_test)
#' @return A fitted workflow.
#' @export
fit_best <- function(x, ...) {
  UseMethod("fit_best")
}

#' @export
#' @rdname fit_best
fit_best.default <- function(x, ...) {
  cls <- class(x)
  cli::cli_abort(
    "There is no `fit_best()` method for an object with \\
     {cli::qty(cls)} class{?es} {.var {cls}}."
  )
}

#' @export
#' @rdname fit_best
fit_best.tune_results <- function(x,
                                  metric = NULL,
                                  eval_time = NULL,
                                  parameters = NULL,
                                  verbose = FALSE,
                                  add_validation_set = NULL,
                                  ...) {
  if (length(list(...))) {
    cli::cli_abort(c("x" = "The `...` are not used by this function."))
  }
  wflow <- .get_tune_workflow(x)
  if (is.null(wflow)) {
    cli::cli_abort(c("x" = "The control option `save_workflow = TRUE` should be used when tuning."))
  }

  if (is.null(parameters)) {
    if (is.null(metric)) {
      metric <- .get_tune_metric_names(x)[1]
    }
    met_set <- .get_tune_metrics(x)
    if (is.null(eval_time) & is_dyn(met_set, metric)) {
      # 1st element of NULL is still NULL
      eval_time <- .get_tune_eval_times(x)[1]
    }
    parameters <- select_best(x, metric = metric, eval_time = eval_time)
    if (verbose) {
      format_final_param(parameters, metric)
    }
  }

  # ----------------------------------------------------------------------------

  wflow <- finalize_workflow(wflow, parameters = parameters)

  # ----------------------------------------------------------------------------

  pset <- hardhat::extract_parameter_set_dials(wflow)
  p <- nrow(pset)
  if (p > 0) {
    cli::cli_abort(
      "The {cli::qty(pset$id)} parameter{?s} {.var {pset$id}} \\
       {?is/are} still marked for tuning."
    )
  }

  # ----------------------------------------------------------------------------

  if (inherits(x$splits[[1]], "val_split")) {
    if (is.null(add_validation_set)) {
      rset_info <- attr(x, "rset_info")
      originate_from_3way_split <- rset_info$att$origin_3way %||% FALSE
      if (originate_from_3way_split) {
        add_validation_set <- FALSE
      } else {
        add_validation_set <- TRUE
      }
    }
    if (add_validation_set) {
      dat <- x$splits[[1]]$data
    } else {
      dat <- rsample::training(x$splits[[1]])
    }
  } else {
    if (!is.null(add_validation_set)) {
      rlang::warn(
        "The option `add_validation_set` is being ignored because the resampling object does not include a validation set."
      )
    }
    dat <- x$splits[[1]]$data
  }
  if (verbose) {
    cli::cli_inform(c("i" = "Fitting using {nrow(dat)} data points..."))
  }
  res <- parsnip::fit(wflow, dat)
  if (verbose) {
    cli::cli_inform(c("v" = "Done.", " "))
  }
  res
}

format_final_param <- function(x, metric) {
  x <- dplyr::select(x, -dplyr::any_of(".config"))
  if (ncol(x) == 0) {
    return(invisible(x))
  }
  lst <- as.list(x)
  chr_lst <- format(lst)
  nms_lst <- format(paste0(names(chr_lst), ": "))
  print_lst <- paste0("  ", nms_lst, chr_lst, collapse = "\n")
  prm <- ifelse(length(lst) == 1, "parameter was:\n", "parameters were:\n")
  cat("Using", metric, "as the metric, the optimal", prm)
  cat(print_lst, sep = "")
  cat("\n\n")
  invisible(x)
}

