#' Determine if case weights should be passed on to yardstick
#'
#' @description
#' This S3 method defines the logic for deciding when a case weight vector
#' should be passed to yardstick metric functions and used to measure model
#' performance. The current logic is that frequency weights (i.e.
#' [hardhat::frequency_weights()]) are the only situation where this should
#' occur.
#'
#' @param x A vector
#' @return A single `TRUE` or `FALSE`.
#' @export
#' @examples
#' library(parsnip)
#' library(dplyr)
#'
#' frequency_weights(1:10) %>%
#'   .use_case_weights_with_yardstick()
#'
#' importance_weights(seq(1, 10, by = .1))%>%
#'   .use_case_weights_with_yardstick()
.use_case_weights_with_yardstick <- function(x) {
  UseMethod(".use_case_weights_with_yardstick")
}

#' @export
.use_case_weights_with_yardstick.default <- function(x) {
  message <- c(
    paste0("Unknown case weights object with class <", class(x)[[1]], ">. "),
    i = paste0(
      "Define a `.use_case_weights_with_yardstick()` method for this type to ",
      "declare whether or not these case weights should be passed on to yardstick."
    ),
    i = "See `?.use_case_weights_with_yardstick` for more information."
  )

  rlang::abort(message)
}

#' @rdname dot-use_case_weights_with_yardstick
#' @export
.use_case_weights_with_yardstick.hardhat_importance_weights <- function(x) {
  FALSE
}

#' @rdname dot-use_case_weights_with_yardstick
#' @export
.use_case_weights_with_yardstick.hardhat_frequency_weights <- function(x) {
  TRUE
}

# ------------------------------------------------------------------------------

extract_case_weights <- function(data, workflow) {
  col <- extract_case_weights_col(workflow)

  if (!rlang::is_quosure(col)) {
    rlang::abort("`col` must exist and be a quosure at this point.", .internal = TRUE)
  }

  loc <- eval_select_case_weights(col, data)

  case_weights <- data[[loc]]

  if (!hardhat::is_case_weights(case_weights)) {
    rlang::abort(paste0(
      "Case weights must be a supported case weights type, as determined by ",
      "`hardhat::is_case_weights()`."
    ))
  }

  case_weights
}

extract_case_weights_col <- function(x) {
  # workflows:::extract_case_weights_col()
  x$pre$actions$case_weights$col
}

eval_select_case_weights <- function(col,
                                     data,
                                     ...,
                                     call = rlang::caller_env()) {
  # workflows:::eval_select_case_weights()

  rlang::check_dots_empty()

  # `col` is saved as a quosure, so it carries along the evaluation environment
  env <- rlang::empty_env()

  loc <- tidyselect::eval_select(
    expr = col,
    data = data,
    env = env,
    error_call = call
  )

  if (length(loc) != 1L) {
    message <- paste0(
      "`col` must specify exactly one column from ",
      "`data` to extract case weights from."
    )

    rlang::abort(message, call = call)
  }

  loc
}

case_weights_column_name <- function() {
  ".case_weights"
}
