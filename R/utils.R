#' Internal functions for developers
#'
#' These are not intended for use by the general public.
#' @param x An object.
#' @param ... Other options
#' @keywords internal
#' @export
empty_ellipses <- function(...) {
  dots <- rlang::enquos(...)
  if (length(dots) > 0) {
    nms <- names(dots)
    no_name <- nms == ""
    if (!any(no_name)) {
      cli::cli_warn(
        "The {.code ...} are not used in this function but {length(dots)}
         object{?s} {?was/were} passed: {.val {names(dots)}}"
      )
    } else if (all(no_name)) {
      cli::cli_warn(
        "The {.code ...} are not used in this function but {length(dots)}
         unnamed object{?s} {?was/were} passed."
      )
    } else {
      cli::cli_warn(
        "The {.code ...} are not used in this function but {length(dots)}
         object{?s} {?was/were} passed."
      )
    }
  }
  invisible(NULL)
}


#' @export
#' @keywords internal
#' @rdname empty_ellipses
is_recipe <- function(x) {
  inherits(x, "recipe")
}

#' @export
#' @keywords internal
#' @rdname empty_ellipses
is_preprocessor <- function(x) {
  is_recipe(x) || rlang::is_formula(x)
}

#' @export
#' @keywords internal
#' @rdname empty_ellipses
is_workflow <- function(x) {
  inherits(x, "workflow")
}

# adapted from ps:::is_cran_check()
is_cran_check <- function() {
  if (identical(Sys.getenv("NOT_CRAN"), "true")) {
    FALSE
  } else {
    Sys.getenv("_R_CHECK_PACKAGE_NAME_", "") != ""
  }
}

# suggests: a character vector of package names, giving packages
#           listed in Suggests that are needed for the example.
# for use a la `@examplesIf tune:::should_run_examples()`
should_run_examples <- function(suggests = NULL) {
  has_needed_installs <- TRUE

  if (!is.null(suggests)) {
    has_needed_installs <- rlang::is_installed(suggests)
  }

  has_needed_installs && !is_cran_check()
}

# new_tibble() currently doesn't strip attributes
# https://github.com/tidyverse/tibble/pull/769
new_bare_tibble <- function(x, ..., class = character()) {
  x <- vctrs::new_data_frame(x)
  tibble::new_tibble(x, nrow = nrow(x), ..., class = class)
}

# a helper that takes in a .config vector and returns the corresponding `.iter`.
# entries from initial results, e.g. `pre2_mod1_post0`, are assigned
# `.iter = 0`.
.config_to_.iter <- function(.config) {
  .iter <- .config
  nonzero <- grepl("^[iI]ter", .iter)
  .iter <- ifelse(nonzero, gsub("^[iI]ter", "", .iter), "0")
  .iter <- as.numeric(.iter)
  .iter
}

`%||%` <- function(x, y) {
  if (rlang::is_null(x)) y else x
}

## -----------------------------------------------------------------------------

#' Various accessor functions
#'
#' These functions return different attributes from objects with class
#' `tune_result`.
#'
#' @param x An object of class `tune_result`.
#' @return
#' \itemize{
#'   \item `.get_tune_parameters()` returns a `dials` `parameter` object or a tibble.
#'   \item `.get_tune_parameter_names()`, `.get_tune_metric_names()`, and
#'    `.get_tune_outcome_names()` return a character string.
#'   \item `.get_tune_metrics()` returns a metric set or NULL.
#'   \item `.get_tune_workflow()` returns the workflow used to fit the
#'   resamples (if `save_workflow` was set to `TRUE` during fitting) or NULL.
#' }
#' @keywords internal
#' @export
#' @rdname tune_accessor
.get_tune_parameters <- function(x) {
  x <- attributes(x)
  if (any(names(x) == "parameters")) {
    res <- x$parameters
  } else {
    res <- tibble::new_tibble(list())
  }
  res
}

#' @export
#' @rdname tune_accessor
.get_tune_parameter_names <- function(x) {
  x <- attributes(x)
  if (any(names(x) == "parameters")) {
    res <- x$parameters$id
  } else {
    res <- character(0)
  }
  res
}


#' @export
#' @rdname tune_accessor
# This will return any other columns that should be added to the group_by()
# when computing the final (averaged) resampling estimate
.get_extra_col_names <- function(x) {
  res <- character(0)
  mtrcs <- x$.metrics[[1]]
  if (any(names(mtrcs) == ".eval_time")) {
    res <- c(res, ".eval_time")
  }
  if (any(names(mtrcs) == ".by")) {
    res <- c(res, ".by")
  }
  res
}


#' @export
#' @rdname tune_accessor
.get_tune_metrics <- function(x) {
  x <- attributes(x)
  if (any(names(x) == "metrics")) {
    res <- x$metrics
  } else {
    res <- NULL
  }
  res
}

#' @export
#' @rdname tune_accessor
.get_tune_metric_names <- function(x) {
  x <- attributes(x)
  if (any(names(x) == "metrics")) {
    res <- names(attributes(x$metrics)$metrics)
  } else {
    res <- character(0)
  }
  res
}


#' @export
#' @rdname tune_accessor
.get_tune_eval_times <- function(x) {
  x <- attributes(x)
  if (any(names(x) == "eval_time")) {
    res <- x$eval_time
  } else {
    res <- NULL
  }
  res
}

#' @export
#' @rdname tune_accessor
.get_tune_eval_time_target <- function(x) {
  x <- attributes(x)
  if (any(names(x) == "eval_time_target")) {
    res <- x$eval_time_target
  } else {
    res <- NULL
  }
  res
}

#' @export
#' @rdname tune_accessor
.get_tune_outcome_names <- function(x) {
  x <- attributes(x)
  if (any(names(x) == "outcomes")) {
    res <- x$outcomes
  } else {
    res <- character(0)
  }
  res
}

#' @export
#' @rdname tune_accessor
.get_tune_workflow <- function(x) {
  x <- attributes(x)
  if (any(names(x) == "workflow")) {
    res <- x$workflow
  } else {
    res <- NULL
  }
  res
}

#' @export
#' @rdname tune_accessor
.get_fingerprint.tune_results <- function(x, ...) {
  att <- attributes(x)$rset_info$att
  if (any(names(att) == "fingerprint")) {
    res <- att$fingerprint
  } else {
    res <- NA_character_
  }
  res
}

# Get a textual summary of the type of resampling
#' @export
pretty.tune_results <- function(x, ...) {
  attr(x, "rset_info")$label
}

#' Fold weights utility functions
#'
#' These are internal functions for handling variable fold weights in
#' hyperparameter tuning.
#'
#' @param x A tune_results object.
#' @param weights Numeric vector of weights.
#' @param id_names Character vector of ID column names.
#' @param metrics_data The metrics data frame.
#' @param w Numeric vector of weights.
#' @param n_folds Integer number of folds.
#'
#' @return Various return values depending on the function.
#' @keywords internal
#' @name fold_weights_utils
#' @aliases .create_weight_mapping .weighted_sd .effective_sample_size .validate_fold_weights
#' @export
#' @rdname fold_weights_utils
.get_fold_weights <- function(x) {
  rset_info <- attr(x, "rset_info")
  if (is.null(rset_info)) {
    return(NULL)
  }

  # Access weights from rset_info attributes using correct path
  weights <- rset_info$att[[".fold_weights"]]

  weights
}

#' @export
#' @rdname fold_weights_utils
.create_weight_mapping <- function(weights, id_names, metrics_data) {
  # Get unique combinations of ID columns from the metrics data
  unique_ids <- dplyr::distinct(metrics_data, !!!rlang::syms(id_names))

  if (nrow(unique_ids) != length(weights)) {
    cli::cli_warn(
      c(
        "Number of weights ({length(weights)}) does not match number of resamples ({nrow(unique_ids)}).",
        "Weights will be ignored."
      )
    )
    return(NULL)
  }

  # Add weights to the unique ID combinations
  unique_ids$.fold_weight <- weights
  unique_ids
}

#' @export
#' @rdname fold_weights_utils
.weighted_sd <- function(x, w) {
  if (all(is.na(x))) {
    return(NA_real_)
  }

  # Remove NA values and corresponding weights
  valid <- !is.na(x)
  x_valid <- x[valid]
  w_valid <- w[valid]

  if (length(x_valid) <= 1) {
    return(NA_real_)
  }

  # Calculate weighted variance
  weighted_var <-
    tibble::as_tibble_col(x) |>
    stats::cov.wt(wt = w, cor = FALSE)

  weighted_var <- weighted_var$cov[1, 1]

  sqrt(weighted_var)
}

#' @export
#' @rdname fold_weights_utils
.effective_sample_size <- function(w) {
  # Remove NA weights
  w <- w[!is.na(w)]

  if (length(w) == 0) {
    return(0)
  }

  # Calculate effective sample size: (sum of weights)^2 / sum of squared weights
  sum_w <- sum(w)
  sum_w_sq <- sum(w^2)

  if (sum_w_sq == 0) {
    return(0)
  }

  sum_w^2 / sum_w_sq
}

#' @export
#' @rdname fold_weights_utils
.validate_fold_weights <- function(weights, n_folds) {
  if (is.null(weights)) {
    return(NULL)
  }

  if (!is.numeric(weights)) {
    cli::cli_abort("{.arg weights} must be numeric.")
  }

  if (length(weights) != n_folds) {
    cli::cli_abort(
      "Length of {.arg weights} ({length(weights)}) must equal number of folds ({n_folds})."
    )
  }

  if (any(weights < 0)) {
    cli::cli_abort("{.arg weights} must be non-negative.")
  }

  if (all(weights == 0)) {
    cli::cli_abort("At least one weight must be positive.")
  }

  # Return normalized weights
  weights / sum(weights)
}

#' Add fold weights to an rset object
#'
#' This function allows you to specify custom weights for cross-validation
#' folds. Weights are automatically normalized to sum to 1.
#'
#' @param rset An rset object from \pkg{rsample}.
#' @param weights A numeric vector of weights, one per fold. Weights will be
#' normalized.
#' @return The rset object with weights added as an attribute.
#' @details
#' Fold weights are useful when assessment sets (i.e., held out data) have
#' different sizes or when you want to upweight certain folds in the evaluation.
#' The weights are stored as an attribute and used automatically during
#' metric aggregation.
#' @seealso [calculate_fold_weights()], [get_fold_weights()]
#' @examples
#' library(rsample)
#' folds <- vfold_cv(mtcars, v = 3)
#' # Give equal weight to all folds
#' weighted_folds <- add_fold_weights(folds, c(1, 1, 1))
#' # Emphasize the first fold
#' weighted_folds <- add_fold_weights(folds, c(0.5, 0.25, 0.25))
#' @export
add_fold_weights <- function(rset, weights) {
  if (!inherits(rset, "rset")) {
    cli::cli_abort("{.arg rset} must be an rset object.")
  }

  # Validate weights
  weights <- .validate_fold_weights(weights, nrow(rset))

  # Add weights as an attribute
  attr(rset, ".fold_weights") <- weights

  rset
}

#' Calculate fold weights from fold sizes
#'
#' This convenience function calculates weights proportional to the number of
#' observations in each fold's analysis set. Larger folds get higher weights.
#' This ensures that folds with more data have proportionally more influence
#' on the final aggregated metrics.
#'
#' @param rset An rset object from \pkg{rsample}.
#' @return A numeric vector of weights proportional to fold sizes, normalized
#'   to sum to 1.
#' @details
#' This is particularly useful for time-based folds (e.g., expanding window CV)
#' or stratified sampling  where folds might have slightly different sizes, in
#' which folds are imbalanced.
#' @seealso [add_fold_weights()], [get_fold_weights()]
#' @examples
#' library(rsample)
#' folds <- vfold_cv(mtcars, v = 3)
#' weights <- calculate_fold_weights(folds)
#' weighted_folds <- add_fold_weights(folds, weights)
#' @export
calculate_fold_weights <- function(rset) {
  if (!inherits(rset, "rset")) {
    cli::cli_abort("{.arg rset} must be an rset object.")
  }

  # Calculate the size of each analysis set
  fold_sizes <- purrr::map_int(rset$splits, ~ nrow(rsample::analysis(.x)))

  # Return weights proportional to fold sizes
  fold_sizes / sum(fold_sizes)
}

#' Extract fold weights from rset or tuning objects
#'
#' This function provides a consistent interface to access fold weights
#' regardless of whether they were added to an rset object or are stored
#' in `tune_results` after tuning.
#'
#' @param x An rset object with fold weights, or a `tune_results` object.
#' @return A numeric vector of fold weights, or NULL if no weights are present.
#' @export
#' @examples
#' \dontrun{
#' library(rsample)
#' folds <- vfold_cv(mtcars, v = 3)
#' weighted_folds <- add_fold_weights(folds, c(0.2, 0.3, 0.5))
#' get_fold_weights(weighted_folds)
#' }
get_fold_weights <- function(x) {
  if (inherits(x, "rset")) {
    # For rset objects, weights are stored as an attribute
    attr(x, ".fold_weights")
  } else if (inherits(x, c("tune_results", "resample_results"))) {
    # For tune results, use the internal function
    return(.get_fold_weights(x))
  } else {
    cli::cli_abort("{.arg x} must be an rset or tune_results object.")
  }
}

#' @export
print.rset <- function(x, ...) {
  fold_weights <- attr(x, ".fold_weights")

  if (!is.null(fold_weights)) {
    # Create a tibble with fold weights as a column
    x_tbl <- tibble::as_tibble(x)
    x_tbl$fold_weight <- fold_weights
    print(x_tbl, ...)
  } else {
    # Use default behavior
    NextMethod("print")
  }
}

#' @export
print.manual_rset <- function(x, ...) {
  fold_weights <- attr(x, ".fold_weights")

  if (!is.null(fold_weights)) {
    # Create a tibble with fold weights as a column
    x_tbl <- tibble::as_tibble(x)
    x_tbl$fold_weight <- fold_weights
    print(x_tbl, ...)
  } else {
    # Use default behavior for manual_rset
    NextMethod("print")
  }
}

# ------------------------------------------------------------------------------

#' Save most recent results to search path
#' @param x An object.
#' @return NULL, invisibly.
#' @details The function will assign `x` to `.Last.tune.result` and put it in
#' the search path.
#' @export
.stash_last_result <- function(x) {
  if (!"org:r-lib" %in% search()) {
    do.call(
      "attach",
      list(new.env(), pos = length(search()), name = "org:r-lib")
    )
  }
  env <- as.environment("org:r-lib")
  env$.Last.tune.result <- x
  invisible(NULL)
}
