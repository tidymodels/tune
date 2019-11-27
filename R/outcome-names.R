#' Determine names of the outcome data in a workflow
#'
#' @param x An object.
#' @param ... Not used.
#' @return A character string of variable names
#' @keywords internal
#' @examples
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
  y$variable[y$role == "outcome"]
}

#' @export
#' @rdname outcome_names
outcome_names.workflow <- function(x, ...) {
  if (has_preprocessor_formula(x)) {
    return(outcome_names(get_wflow_form(x)))
  }

  if (has_preprocessor_recipe(x)) {
    return(outcome_names(get_wflow_recipe(x)))
  }

  rlang::abort("The workflow must have a formula or recipe preprocessor.")
}
