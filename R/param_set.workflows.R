#' Determination of parameter sets for other objects
#'
#' These methods extend the generic `dials::param_set()` to work with more
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
#'   param_set()
#'
#'  # A peak under the hood
#'  tibble::as_tibble(.Last.value)
#'
#' recipe(mpg ~ ., data = mtcars) %>%
#'   step_ns(disp, deg_free = tune("disp df")) %>%
#'   step_ns(wt, deg_free = tune("wt df")) %>%
#'   param_set()
#'
#' recipe(mpg ~ ., data = mtcars) %>%
#'   step_normalize(all_predictors()) %>%
#'   param_set()
#'
#' library(parsnip)
#'
#' boost_tree(trees = tune(), min_n = tune()) %>%
#'   set_engine("xgboost") %>%
#'   param_set()
#'
#' boost_tree(trees = tune(), min_n = tune()) %>%
#'   set_engine("C5.0", rules = TRUE) %>%
#'   param_set()
#' @keywords internal
#' @export
param_set.workflow <- function(x, ...) {
  param_data <- param_set(x$fit$model$model)
  if (any(names(x$pre) == "recipe")) {
    param_data <-
      dplyr::bind_rows(
        param_data,
        param_set(x$pre$recipe$recipe)
      )
  }
  dials::param_set_constr(
    param_data$name,
    param_data$id,
    param_data$source,
    param_data$component,
    param_data$component_id,
    param_data$object
  )
}

#' @export
#' @rdname param_set.workflow
param_set.model_spec <- function(x, ...) {
  all_args <- tunable(x)
  tuning_param <- tune_args(x)

  res <-
    dplyr::inner_join(
      tuning_param %>% dplyr::select(-tunable, -component_id),
      all_args,
      by = c("name", "source", "component")
    ) %>%
    mutate(object = map(call_info, eval_call_info))

  dials::param_set_constr(
    res$name,
    res$id,
    res$source,
    res$component,
    res$component_id,
    res$object
  )

}

#' @export
#' @rdname param_set.workflow
param_set.recipe <- function(x, ...) {
  all_args <- tunable(x)
  tuning_param <- tune_args(x)
  res <-
    dplyr::inner_join(
      tuning_param %>% dplyr::select(-tunable),
      all_args,
      by = c("name", "source", "component", "component_id")
    ) %>%
    mutate(object = map(call_info, eval_call_info))

  dials::param_set_constr(
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

