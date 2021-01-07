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
    msg <- "The `...` are not used in this function but one or more objects were passed: "
    msg <- paste0(msg, paste0("'", names(dots), "'", collapse = ", "))
    rlang::warn(msg)
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

# new_tibble() currently doesn't strip attributes
# https://github.com/tidyverse/tibble/pull/769
new_bare_tibble <- function(x, ..., class = character()) {
  x <- vctrs::new_data_frame(x)
  tibble::new_tibble(x, nrow = nrow(x), ..., class = class)
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
    res <- tibble::tibble()
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


# Get a textual summary of the type of resampling
#' @export
pretty.tune_results <- function(x, ...) {
  attr(x, "rset_info")$label
}


