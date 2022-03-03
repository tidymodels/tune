#' Determination of parameter sets for other objects
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' These methods have been deprecated in favor of [extract_parameter_set_dials()].
#'
#' These methods extend the generic [dials::parameters()] to work with more
#' complex objects, such as recipes, model specifications, and workflows.
#' @param x An object
#' @param ... Not currently used.
#' @return A parameter set object
#' @examples
#' \donttest{
#' library(tibble)
#' library(recipes)
#'
#' recipe(mpg ~ ., data = mtcars) %>%
#'   step_impute_knn(all_predictors(), neighbors = tune()) %>%
#'   step_pca(all_predictors(), num_comp = tune()) %>%
#'   dials::parameters()
#'
#' # A peak under the hood
#' tibble::as_tibble(.Last.value)
#'
#' recipe(mpg ~ ., data = mtcars) %>%
#'   step_ns(disp, deg_free = tune("disp df")) %>%
#'   step_ns(wt, deg_free = tune("wt df")) %>%
#'   dials::parameters()
#'
#' recipe(mpg ~ ., data = mtcars) %>%
#'   step_normalize(all_predictors()) %>%
#'   dials::parameters()
#'
#' library(parsnip)
#'
#' boost_tree(trees = tune(), min_n = tune()) %>%
#'   set_engine("xgboost") %>%
#'   dials::parameters()
#'
#' boost_tree(trees = tune(), min_n = tune()) %>%
#'   set_engine("C5.0", rules = TRUE) %>%
#'   dials::parameters()
#' }
#'
#' @keywords internal
#' @export
parameters.workflow <- function(x, ...) {
  lifecycle::deprecate_soft(
    "0.1.6.9003",
    "parameters.workflow()",
    "hardhat::extract_parameter_set_dials()"
  )
  hardhat::extract_parameter_set_dials(x, ...)
}

#' @export
#' @rdname parameters.workflow
parameters.model_spec <- function(x, ...) {
  lifecycle::deprecate_soft(
    "0.1.6.9003",
    "parameters.model_spec()",
    "hardhat::extract_parameter_set_dials()"
  )
  hardhat::extract_parameter_set_dials(x, ...)
}

#' @export
#' @rdname parameters.workflow
parameters.recipe <- function(x, ...) {
  lifecycle::deprecate_soft(
    "0.1.6.9003",
    "parameters.workflow()",
    "hardhat::extract_parameter_set_dials()"
  )
  hardhat::extract_parameter_set_dials(x, ...)
}
