is_recipe <- function(x) {
  inherits(x, "recipe")
}

is_preprocessor <- function(x) {
  is_recipe(x) || rlang::is_formula(x)
}

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
