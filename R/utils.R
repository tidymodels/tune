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
  }
  else {
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
# entries from initial results, e.g. `Model1_Preprocessor3`, are assigned
# `.iter = 0`.
.config_to_.iter <- function(.config) {
  .iter <- .config
  nonzero <- grepl("Iter", .iter)
  .iter <- ifelse(nonzero, gsub("Iter", "", .iter), "0")
  .iter <- as.numeric(.iter)
  .iter
}

`%||%` <- function (x, y) {if (rlang::is_null(x)) y else x}

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


# ------------------------------------------------------------------------------

#' Save most recent results to search path
#' @param x An object.
#' @return NULL, invisibly.
#' @details The function will assign `x` to `.Last.tune.result` and put it in
#' the search path.
#' @export
.stash_last_result <- function(x) {
  if (! "org:r-lib" %in% search()) {
    do.call("attach", list(new.env(), pos = length(search()),
                           name = "org:r-lib"))
  }
  env <- as.environment("org:r-lib")
  env$.Last.tune.result <- x
  invisible(NULL)
}

