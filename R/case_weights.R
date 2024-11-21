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
    "An object with {.cls hardhat_case_weights} was expected, not
    {.obj_type_friendly {x}}.",
    i = paste0(
      "Define a {.fn .use_case_weights_with_yardstick} method for this type to ",
      "declare whether or not these case weights should be passed on to
       {.pkg yardstick}."
    ),
    i = "See {.help .use_case_weights_with_yardstick} for more information."
  )

  cli::cli_abort(message)
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
    cli::cli_abort("{.arg col} must exist and be a quosure at this point.", .internal = TRUE)
  }

  loc <- eval_select_case_weights(col, data)

  case_weights <- data[[loc]]

  if (!hardhat::is_case_weights(case_weights)) {
    cli::cli_abort(
      "Case weights must be a supported case weights type, as determined by
      {.fn hardhat::is_case_weights}."
    )
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
    cli::cli_abort(
      "{.arg col} must specify exactly one column from {.arg data} to
       extract case weights from.",
      call = call
    )
  }

  loc
}

case_weights_column_name <- function() {
  ".case_weights"
}
