#' Determine names of the outcome data in a workflow
#'
#' @param x An object.
#' @param ... Further arguments passed to or from other methods (such as `data`).
#' @param data The training set data (if needed).
#' @param call The call to be displayed in warnings or errors.
#' @return A character string of variable names
#' @keywords internal
#' @examples
#' library(dplyr)
#' lm(cbind(mpg, wt) ~ ., data = mtcars) %>%
#'   purrr::pluck(terms) %>%
#'   outcome_names()
#' @export
outcome_names <- function(x, ...) {
  UseMethod("outcome_names")
}

#' @export
#' @rdname outcome_names
outcome_names.terms <- function(x, ...) {
  if (length(x) == 2) {
    res <- character(0)
  } else {
    res <- all.vars(x[[2]])
  }
  res
}

#' @export
#' @rdname outcome_names
outcome_names.formula <- outcome_names.terms


#' @export
#' @rdname outcome_names
outcome_names.recipe <- function(x, ...) {
  y <- summary(x)
  y$variable[y$role == "outcome" & !is.na(y$role)]
}

#' @export
#' @rdname outcome_names
outcome_names.workflow <- function(x, ..., call = caller_env()) {
  if (!is.null(x$pre$mold)) {
    y_vals <- extract_mold(x)$outcomes
    res <- colnames(y_vals)
  } else {
    preprocessor <- extract_preprocessor(x)
    res <- outcome_names(preprocessor, ..., call = call)
  }
  res
}

#' @export
#' @rdname outcome_names
outcome_names.workflow_variables <- function(
	x,
	data = NULL,
	...,
	call = caller_env()
) {
	if (is.null(data)) {
		cli::cli_abort(
			"To determine the outcome names when {.fn add_variables} is used, please
       pass the training set data to the {.arg data} argument.",
			call = call
		)
	}
	res <- rlang::eval_tidy(x$outcomes, data, env = call)
	res
}

#' @export
#' @rdname outcome_names
outcome_names.tune_results <- function(x, ..., call = caller_env()) {
  att <- attributes(x)
  if (any(names(att) == "outcomes")) {
    res <- att$outcomes
  } else {
    res <- NA_character_
  }
  res
}
