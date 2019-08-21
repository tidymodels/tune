#' Acquisition function for scoring parameter combinations
#'
#' These functions can be used to score candidate tuning parameter combinations
#' as a function of their predicted mean and variation.
#'
#' The acquisition functions often combine the mean and variance predictions
#' from the Gaussian process model into an objective to be optimized.
#'
#' There is a choice between exploitation and exploration.
#'
#' @param trade_off A number or function that describes the trade-off between
#' exploitation and exploration.
#' @param eps A small constant to avoid division by zero.
#' @param kappa A positive number (or function) that corresponds to the multiplier
#' of the standard deviation in a confidence bounds (e.g. 1.96 in normal-theory
#' 95$\%$ confidence intervals).
#' @return An object of class `prob_improve`, `exp_improve`, or `conf_bounds`
#' along with an extra class of `acquisition_function`.`
#' @examples
#' prob_improve()
#' @export
prob_improve <- function(trade_off = 0, eps = .Machine$double.eps) {
  if (!is.numeric(trade_off) & !is_function(trade_off)) {
    stop("`trade_off` should be a number or a function.", call. = FALSE)
  }
  if (rlang::is_function(trade_off)) {
    farg <- names(formals(trade_off))
    if(length(farg) != 1) {
      stop("The `trade_off` function should only have a single argument.", call. = FALSE)
    }
  }
  res <- list(trade_off = trade_off, eps = eps)
  class(res) <- c("prob_improve", "acquisition_function")
  res
}

print.prob_improve <- function(x, ...) {
  cat("Acquisition Function: probability of improvment\n")
  invisible(x)
}

predict.prob_improve <-
  function(object, new_data, maximize, iter, best,  ...) {
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
        mutate(delta = ((.mean - best - trade_off)/.sd))
    } else {
      new_data <-
        new_data %>%
        mutate(delta = ((trade_off + best - .mean )/.sd))
    }
    tibble(objective = pnorm(delta))
  }

# ------------------------------------------------------------------------------

#' @export
#' @rdname prob_improve
exp_improve <- function(trade_off = 0, eps = .Machine$double.eps) {
  if (!is.numeric(trade_off) & !is_function(trade_off)) {
    stop("`trade_off` should be a number or a function.", call. = FALSE)
  }
  if (rlang::is_function(trade_off)) {
    farg <- names(formals(trade_off))
    if(length(farg) != 1) {
      stop("The `trade_off` function should only have a single argument.", call. = FALSE)
    }
  }
  res <- list(trade_off = trade_off, eps = eps)
  class(res) <- c("exp_improve", "acquisition_function")
  res

}

#' @export
predict.exp_improve <- function(object, new_data, maximize, iter, best,  ...) {
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
      snr = delta/sd_trunc,
      z = ifelse(.sd <= object$eps, 0, snr),
      objective = (delta * pnorm(z)) + (sd_trunc * dnorm(z))
    )

  new_data %>% dplyr::select(objective)
}


#' @export
#' @rdname prob_improve
conf_bound <- function(kappa = qnorm(0.975)) {
  if (!is.numeric(kappa) & !is_function(kappa)) {
    stop("`kappa` should be a number or a function.", call. = FALSE)
  }
  if (rlang::is_function(kappa)) {
    farg <- names(formals(kappa))
    if(length(farg) != 1) {
      stop("The `kappa` function should only have a single argument.", call. = FALSE)
    }
  }
  res <- list(kappa = kappa)
  class(res) <- c("conf_bound", "acquisition_function")
  res

}

#' @export
predict.conf_bound<- function(object, new_data, maximize, iter, ...) {
  if (is.function(object$kappa)) {
    kappa <- object$kappa(iter)
  } else {
    kappa <- object$kappa
  }

  if (maximize) {
    new_data <- new_data %>% mutate(objective = .mean - kappa * .sd)
  } else {
    new_data <- new_data %>% mutate(objective = .mean + kappa * .sd)
  }
  new_data %>% dplyr::select(objective)
}

