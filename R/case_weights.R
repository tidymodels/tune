
# used in predict_model() to extract weights and determine if they are used
maybe_assessment_weights <- function(rsplit) {
  dat <- rsample::assessment(rsplit)
  is_case_wt <- purrr::map_lgl(dat, ~ inherits(.x, "hardhat_case_weights"))
  num_wt_cols <- sum(is_case_wt)
  # check more than one
  if (num_wt_cols == 1) {
    res <- dat[[which(is_case_wt)]]
    # Now that we have the case weights, should they be used?
    res <- performance_case_weights(res)
  } else if (num_wt_cols == 0) {
    res <- NULL
  } else {
    rlang::stop("Only one case weight column is allowed.")
  }
  res
}

get_case_weight_data <- function(x) {
  # Get case weight vector (if any)
  if (any(names(x) == ".case_weight")) {
    res <- x$.case_weight
  } else {
    res <- NULL
  }
  res
}

# ------------------------------------------------------------------------------

#' Determine if case weights are needed to estimate model performance
#'
#' @param x A vector
#' @param ... Not currently used.
#' @return A numeric vector or NULL
#' @examples
#' freq_wts <- frequency_weights(1:10)
#' performance_case_weights(freq_wts)
#'
#' imp_wts <- importance_weights(seq(1, 10, by = .1))
#' performance_case_weights(imp_wts)
#'
#' not_wts <- seq(1, 10, by = .1)
#' performance_case_weights(not_wts)
#' @export
performance_case_weights <- function(x, ...) {
  UseMethod("performance_case_weights")
}

#' @rdname performance_case_weights
#' @export
performance_case_weights.default <- function(x, ...) {
  NULL
}

#' @rdname performance_case_weights
#' @export
performance_case_weights.hardhat_importance_weights <- function(x, ...) {
  NULL
}

#' @rdname performance_case_weights
#' @export
performance_case_weights.hardhat_frequency_weights <- function(x, ...) {
  as.integer(x)
}

