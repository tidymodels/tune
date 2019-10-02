# nocov start
#' Simple workflow functions
#'
#' These functions emulate funcitons and data structures of the upcoming workflows
#' package. In the future, these api's may change and these functions will not
#' be contained in the `tune` package.
#'
#' @param object,x a workflow object.
#' @param model A parsnip model specificatoin.
#' @param recipe A recipe object.
#' @param formula A basic model formula.
#' @param ... Not currently used.
#' @return A workflow object.
#'
#'
#' @export
workflow <- function() {
  res <- list(pre = list(), fit = list())
  class(res) <- "workflow"
  res
}

#' @export
#' @rdname workflow
add_model <- function(object, model) {
  if (!inherits(model, "model_spec")) {
    rlang::abort("`model` should be a parsnip model specification.")
  }
  # emulates the structure of a workflow
  object$fit$model$model <- model
  object
}

#' @export
#' @rdname workflow
add_recipe <- function(object, recipe) {
  if (!inherits(recipe, "recipe")) {
    rlang::abort("`recipe` should be a recipe.")
  }
  if (!is.null(object$pre$formula_processor)) {
    rlang::abort("workflow already contains a formula; please start another workflow.")
  }
  # emulates the structure of a workflow
  object$pre$recipe$recipe <- recipe
  object
}

#' @export
#' @rdname workflow
add_formula <- function(object, formula) {
  if (!inherits(formula, "formula")) {
    rlang::abort("`formula` should be a formula")
  }
  if (!is.null(object$pre$recipe)) {
    rlang::abort("workflow already contains a recipe; please start another workflow.")
  }
  # emulates the structure of a workflow
  object$pre$formula_processor$formula_processor <- formula
  object
}

#' @export
#' @rdname workflow
print.workflow <- function(x, ...) {
  cat("workflow object\n")
  if (!is.null(x$fit)) {
    cat(" model object:\n")
    print(get_wflow_model(x))
  } else {
    cat(" (no model specified)\n")
  }
  if (!is.null(x$pre$formula_processor)) {
    cat(" formula:\n")
    y <- get_wflow_form(x)
    cat(deparse(y), "\n")
  }
  if (!is.null(x$pre$recipe)) {
    cat(" recipe:\n")
    print(get_wflow_pre(x))
  }
  invisible(x)
}

# ------------------------------------------------------------------------------

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
  if (any(names(x$pre) == "formula_processor")) {
    res <- outcome_names(x$pre$formula_processor$formula_processor)
  } else {
    res <- outcome_names(x$pre$recipe$recipe)
  }
  res
}

# nocov end
