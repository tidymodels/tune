#' Determination of parameter sets for other objects
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' These methods have been deprecated in favor of [extract_parameter_set_dials()].
#'
#' @param x An object
#' @param ... Not currently used.
#' @return A parameter set object
#'
#' @keywords internal
#' @export
parameters.workflow <- function(x, ...) {
  lifecycle::deprecate_warn(
    "0.1.6.9003",
    "parameters.workflow()",
    "hardhat::extract_parameter_set_dials()"
  )
  hardhat::extract_parameter_set_dials(x, ...)
}

#' @export
#' @rdname parameters.workflow
parameters.model_spec <- function(x, ...) {
  lifecycle::deprecate_warn(
    "0.1.6.9003",
    "parameters.model_spec()",
    "hardhat::extract_parameter_set_dials()"
  )
  hardhat::extract_parameter_set_dials(x, ...)
}

#' @export
#' @rdname parameters.workflow
parameters.recipe <- function(x, ...) {
  lifecycle::deprecate_warn(
    "0.1.6.9003",
    "parameters.workflow()",
    "hardhat::extract_parameter_set_dials()"
  )
  hardhat::extract_parameter_set_dials(x, ...)
}
