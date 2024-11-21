#' Acquisition function for scoring parameter combinations
#'
#' These functions can be used to score candidate tuning parameter combinations
#' as a function of their predicted mean and variation.
#'
#' @details The acquisition functions often combine the mean and variance
#' predictions from the Gaussian process model into an objective to be
#' optimized.
#'
#' For this documentation, we assume that the metric in question is better when
#' _maximized_ (e.g. accuracy, the coefficient of determination, etc).
#'
#' The expected improvement of a point `x` is based on the predicted mean and
#' variation at that point as well as the current best value (denoted here as
#' `x_b`). The vignette linked below contains the formulas for this acquisition
#' function. When the `trade_off` parameter is greater than zero, the
#' acquisition function will down-play the effect of the _mean_ prediction and
#' give more weight to the variation. This has the effect of searching for new
#' parameter combinations that are in areas that have yet to be sampled.
#'
#' Note that for [exp_improve()] and [prob_improve()], the `trade_off` value is
#' in the units of the outcome. The functions are parameterized so that the
#' `trade_off` value should always be non-negative.
#'
#' The confidence bound function does not take into account the current best
#' results in the data.
#'
#' If a function is passed to  [exp_improve()] or [prob_improve()], the function
#' can have multiple arguments but only the first (the current iteration number)
#' is given to the function. In other words, the function argument should have
#' defaults for all but the first argument. See [expo_decay()] as an example of
#' a function.
#'
#' @param trade_off A number or function that describes the trade-off between
#'   exploitation and exploration. Smaller values favor exploitation.
#' @param eps A small constant to avoid division by zero.
#' @param kappa A positive number (or function) that corresponds to the
#'   multiplier of the standard deviation in a confidence bound (e.g. 1.96 in
#'   normal-theory 95 percent confidence intervals). Smaller values lean more
#'   towards exploitation.
#' @return An object of class `prob_improve`, `exp_improve`, or `conf_bounds`
#'   along with an extra class of `acquisition_function`.
#'
#' @seealso [tune_bayes()], [expo_decay()]
#' @examples
#' prob_improve()
#' @export
prob_improve <- function(trade_off = 0, eps = .Machine$double.eps) {
  if (!is.numeric(trade_off) & !is_function(trade_off)) {
    cli::cli_abort("{.arg trade_off} should be a number or a function.")
  }

  lab <- "the probability of improvement"

  if (rlang::is_function(trade_off)) {
    farg <- names(formals(trade_off))
    if (length(farg) == 0) {
      cli::cli_abort("The {.fn trade_off} function should have at least one argument.")
    }
    lab <- paste(lab, "with variable trade-off values.")
  }

  res <- list(trade_off = trade_off, eps = eps, lable = lab)
  class(res) <- c("prob_improve", "acquisition_function")
  res
}

#' @export
print.prob_improve <- function(x, ...) {
  cat("Acquisition Function: probability of improvment\n")
  invisible(x)
}

#' @export
predict.prob_improve <-
  function(object, new_data, maximize, iter, best, ...) {
    check_bool(maximize)
    check_number_decimal(best, allow_infinite = FALSE)

    if (is.function(object$trade_off)) {
      trade_off <- object$trade_off(iter)
    } else {
      trade_off <- object$trade_off
    }

    new_data <-
      new_data %>%
      mutate(.sd = ifelse(.sd <= object$eps, object$eps, .sd))

    if (maximize) {
      new_data <-
        new_data %>%
        mutate(delta = ((.mean - best - trade_off) / .sd))
    } else {
      new_data <-
        new_data %>%
        mutate(delta = ((trade_off + best - .mean) / .sd))
    }
    new_data %>%
      dplyr::mutate(objective = pnorm(delta)) %>%
      dplyr::select(objective)
  }

# ------------------------------------------------------------------------------

#' @export
#' @rdname prob_improve
exp_improve <- function(trade_off = 0, eps = .Machine$double.eps) {
  if (!is.numeric(trade_off) & !is_function(trade_off)) {
    cli::cli_abort("{.arg trade_off} should be a number or a function.")
  }

  lab <- "the expected improvement"

  if (rlang::is_function(trade_off)) {
    farg <- names(formals(trade_off))
    if (length(farg) == 0) {
      cli::cli_abort("The {.fn trade_off} function should have at least one argument.")
    }
    lab <- paste(lab, "with variable trade-off values.")
  }
  res <- list(trade_off = trade_off, eps = eps, label = lab)
  class(res) <- c("exp_improve", "acquisition_function")
  res
}

#' @export
# NOTE `maximize` is the direction of the metric, not the acquisition function
predict.exp_improve <- function(object, new_data, maximize, iter, best, ...) {
  check_bool(maximize)
  check_number_decimal(best, allow_infinite = FALSE)

  if (is.function(object$trade_off)) {
    trade_off <- object$trade_off(iter)
  } else {
    trade_off <- object$trade_off
  }

  new_data <-
    new_data %>%
    mutate(sd_trunc = ifelse(.sd <= object$eps, object$eps, .sd))

  if (maximize) {
    new_data <- new_data %>% mutate(delta = .mean - best - trade_off)
  } else {
    new_data <- new_data %>% mutate(delta = trade_off + best - .mean)
  }
  new_data <-
    new_data %>%
    mutate(
      snr = delta / sd_trunc,
      z = ifelse(.sd <= object$eps, 0, snr),
      objective = (delta * pnorm(z)) + (sd_trunc * dnorm(z))
    )

  new_data %>% dplyr::select(objective)
}


#' @export
#' @rdname prob_improve
conf_bound <- function(kappa = 0.1) {
  if (!is.numeric(kappa) & !is_function(kappa)) {
    cli::cli_abort("{.arg kappa} should be a number or a function.")
  }
  lab <- "the confidence bound"
  if (rlang::is_function(kappa)) {
    farg <- names(formals(kappa))
    if (length(farg) == 0) {
      cli::cli_abort("The {.fn trade_off} function should have at least one argument.")
    }
    lab <- paste(lab, "with variable kappa values.")
  }
  res <- list(kappa = kappa, label = lab)
  class(res) <- c("conf_bound", "acquisition_function")
  res
}

#' @export
predict.conf_bound <- function(object, new_data, maximize, iter, ...) {
  check_bool(maximize)

  if (is.function(object$kappa)) {
    kappa <- object$kappa(iter)
  } else {
    kappa <- object$kappa
  }

  # `tune` is setup to always maximize the objective function
  if (maximize) {
    new_data <- new_data %>% mutate(objective = .mean + kappa * .sd)
  } else {
    new_data <- new_data %>% mutate(objective = -(.mean + kappa * .sd))
  }
  new_data %>% dplyr::select(objective)
}
