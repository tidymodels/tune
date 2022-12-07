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
#' @param parameters An optional 1-row tibble of tuning parameter settings, with
#' a column for each tuning parameter. This tibble should have columns for each
#' tuning parameter identifier (e.g. `"my_param"` if `tune("my_param")` was used).
#' If `NULL`, this argument will be set to
#' [`select_best(metric)`][tune::select_best.tune_results].
#' @param verbose A logical for printing logging.
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
#' The data used for the fit are taken from the `splits` column. If the split
#' column was from a validation split, the combined training and validation sets
#' are used.
#'
#' In comparison to [last_fit()], that function requires a finalized model, fits
#' the model on the data set by [rsample::initial_split()], and computes
#' metrics from the test set.
#' @examples
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
#' meat_test <- testing(meat_split)
#'
#' set.seed(2)
#' meat_rs <- vfold_cv(meat_train, v = 10)
#'
#' pca_rec <-
#'   recipe(protein ~ ., data = meat_train) %>%
#'   step_pca(all_predictors(), num_comp = tune())
#'
#' knn_mod <- nearest_neighbor(neighbors = tune()) %>% set_mode("regression")
#'
#' ctrl <- control_grid(save_workflow = TRUE)
#'
#' knn_pca_res <-
#'   tune_grid(knn_mod, pca_rec, resamples = meat_rs, grid = 10, control = ctrl)
#'
#' knn_fit <- fit_best(knn_pca_res, verbose = TRUE)
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
                                  parameters = NULL,
                                  verbose = FALSE,
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
    parameters <- select_best(x, metric = metric)
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

  dat <- x$splits[[1]]$data
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

