
# used in predict_model() to extract weights and determine if they are used
maybe_assessment_weights <- function(rsplit) {
  dat <- rsample::assessment(rsplit)
  is_case_wt <- purrr::map_lgl(dat, ~ inherits(.x, "hardhat_case_weights"))
  num_wt_cols <- sum(is_case_wt)

  if (num_wt_cols == 1) {
    res <- dat[[which(is_case_wt)]]
    # Now that we have the case weights, should they be used?
    if (!.use_case_weights_with_yardstick(res)) {
      res <- NULL
    }
  } else if (num_wt_cols == 0) {
    res <- NULL
  } else {
    rlang::abort("Only one case weight column is allowed.")
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

#' Determine if yardstick used case weights
#'
#' @description
#' This S3 method defines the logical for when a case weight vector should be
#' passed to the yardstick function and used to measure model performance.
#' The current logical is that frequency weights are the only situation where
#' this should occur.
#'
#' @param x A vector
#' @param ... Not currently used.
#' @return A logical.
#' @examples
#' library(dplyr)
#'
#' frequency_weights(1:10) %>%
#'   .use_case_weights_with_yardstick()
#'
#' importance_weights(seq(1, 10, by = .1))%>%
#'   .use_case_weights_with_yardstick()
#'
#' seq(1, 10, by = .1) %>%
#'   .use_case_weights_with_yardstick()
#' @export
.use_case_weights_with_yardstick <- function(x, ...) {
  UseMethod(".use_case_weights_with_yardstick")
}

#' @rdname .use_case_weights_with_yardstick
#' @export
.use_case_weights_with_yardstick.default <- function(x, ...) {
  FALSE
}

#' @rdname .use_case_weights_with_yardstick
#' @export
.use_case_weights_with_yardstick.hardhat_importance_weights <-
  function(x, ...) {
    FALSE
  }

#' @rdname .use_case_weights_with_yardstick
#' @export
.use_case_weights_with_yardstick.hardhat_frequency_weights <-
  function(x, ...) {
    TRUE
  }

# TODO case weights: should collect_predictions return the weights?
