#' Determination of parameter sets for other objects
#'
#' These methods extend the generic [dials::parameters()] to work with more
#' complex objects, such as recipes, model specifications, and workflows.
#' @param x An object
#' @param ... Not currently used.
#' @return A parameter set object
#' @examples
#' library(tibble)
#' library(recipes)
#'
#' recipe(mpg ~ ., data = mtcars) %>%
#'   step_knnimpute(all_predictors(), neighbors = tune()) %>%
#'   step_pca(all_predictors(), num_comp = tune()) %>%
#'   dials::parameters()
#'
#'  # A peak under the hood
#'  tibble::as_tibble(.Last.value)
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
#' @keywords internal
#' @export
parameters.workflow <- function(x, ...) {
  model <- workflows::pull_workflow_spec(x)
  param_data <- dials::parameters(model)

  if (has_preprocessor_recipe(x)) {
    recipe <- workflows::pull_workflow_preprocessor(x)
    recipe_param_data <- dials::parameters(recipe)

    param_data <- dplyr::bind_rows(param_data, recipe_param_data)
  }

  dials::parameters_constr(
    param_data$name,
    param_data$id,
    param_data$source,
    param_data$component,
    param_data$component_id,
    param_data$object
  )
}

#' @export
#' @rdname parameters.workflow
parameters.model_spec <- function(x, ...) {
  all_args <- tunable(x)
  tuning_param <- tune_args(x)

  res <-
    dplyr::inner_join(
      tuning_param %>% dplyr::select(-tunable, -component_id),
      all_args,
      by = c("name", "source", "component")
    ) %>%
    mutate(object = map(call_info, eval_call_info))

  dials::parameters_constr(
    res$name,
    res$id,
    res$source,
    res$component,
    res$component_id,
    res$object
  )

}

#' @export
#' @rdname parameters.workflow
parameters.recipe <- function(x, ...) {
  all_args <- tunable(x)
  tuning_param <- tune_args(x)
  res <-
    dplyr::inner_join(
      tuning_param %>% dplyr::select(-tunable),
      all_args,
      by = c("name", "source", "component", "component_id")
    ) %>%
    mutate(object = map(call_info, eval_call_info))

  dials::parameters_constr(
    res$name,
    res$id,
    res$source,
    res$component,
    res$component_id,
    res$object
  )
}


# ------------------------------------------------------------------------------

eval_call_info <-  function(x) {
  if (!is.null(x)) {
    # Look for other options
    allowed_opts <- c("range", "trans", "values")
    if (any(names(x) %in% allowed_opts)) {
      opts <- x[names(x) %in% allowed_opts]
    } else {
      opts <- list()
    }
    res <- try(rlang::eval_tidy(rlang::call2(x$fun, .ns = x$pkg, !!!opts)), silent = TRUE)
    if (inherits(res, "try-error")) {
      stop(paste0("Error when calling ", x$fun, "(): ", as.character(res)))
    }
  } else {
    res <- NA
  }
  res
}

