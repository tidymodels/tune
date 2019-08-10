#' Merge parameter grid values into objects
#'
#' @description
#'
#' `merge()` can be used to easily update any of the arguments in a
#'  \pkg{parsnip} model or recipe.
#'
#' @param x A recipe or model specification object.
#' @param y A data frame or a parameter grid resulting from one of the
#'  `grid_*` functions. The column names should correspond to the parameter
#'  names (or their annotations) in the object.
#' @param ... Not used but required for S3 completeness.
#'
#' @return A tibble with a column `x` that has as many rows as were in `y`.
#' @keywords internal
#' @examples
#' library(tibble)
#' library(recipes)
#'
#' pca_rec <-
#'   recipe(mpg ~ ., data = mtcars) %>%
#'   step_knnimpute(all_predictors(), neighbors = tune()) %>%
#'   step_pca(all_predictors(), num_comp = tune())
#'
#' pca_grid <-
#'   tribble(
#'     ~neighbors, ~num_comp,
#'              1,         1,
#'              5,         1,
#'              1,         2,
#'              5,         2
#'   )
#'
#' merge(pca_rec, pca_grid)
#'
#' spline_rec <-
#'   recipe(mpg ~ ., data = mtcars) %>%
#'   step_ns(disp, deg_free = tune("disp df")) %>%
#'   step_ns(wt, deg_free = tune("wt df"))
#'
#' spline_grid <-
#'   tribble(
#'     ~"disp df", ~ "wt df",
#'     3,         3,
#'     5,         3,
#'     3,         5,
#'     5,         5
#'   )
#'
#' merge(pca_rec, pca_grid)
#'
#' library(parsnip)
#' library(dials)
#'
#' xgb_mod <-
#'   boost_tree(trees = tune(), min_n = tune()) %>%
#'   set_engine("xgboost")
#'
#' set.seed(254)
#' xgb_grid <-
#'   param_set(xgb_mod) %>%
#'   finalize(iris) %>%
#'   grid_max_entropy(size = 3)
#'
#' merge(xgb_mod, xgb_grid)
#' @export
merge.recipe <- function(x, y, ...) {
  merger(x, y, ...)
}

#' @export
#' @rdname merge.recipe
merge.model_spec <- function(x, y, ...) {
  merger(x, y, ...)
}

update_model <- function(grid, object, pset, step_id, nms, ...) {
  for (i in nms) {
    param_info <- pset %>% dplyr::filter(id == i & source == "model_spec")
    if (nrow(param_info) > 1) {
      # TODO figure this out and write a better message
      stop("There are too many things.", call. = FALSE)
    }
    if (nrow(param_info) == 1) {
      if (param_info$component_id == "main") {
        object$args[[param_info$name]] <- grid[[i]]
      } else {
        object$eng_args[[param_info$name]] <- grid[[i]]
      }
    }
  }
  object
}

update_recipe <- function(grid, object, pset, step_id, nms, ...) {
  for (i in nms) {
    param_info <- pset %>% dplyr::filter(id == i & source == "recipe")
    # check nrow()
    idx <- which(step_id == param_info$component_id)
    # check index
    # should use the contructor but maybe dangerous/difficult
    object$steps[[ idx ]][[param_info$name]] <- grid[[i]]
  }
  object
}


merger <- function(x, y, ...) {
  if (!is.data.frame(y)) {
    stop("The second argument should be a data frame.", call. = FALSE)
  }
  pset <- param_set(x)

  if (nrow(pset) == 0) {
    res <- tibble::tibble(x = map(1:nrow(y), ~ x))
    return(res)
  }
  grid_name <- colnames(y)
  # We will deliberately allow `y` to lack some tunable parameters in `x`

  step_ids <- purrr::map_chr(x$steps, ~ .x$id)

  if (inherits(x, "recipe")) {
    updater <- update_recipe
  } else {
    updater <- update_model
  }

  y %>%
    dplyr::mutate(
      ..object = map(1:nrow(y), ~ updater(y[.x,], x, pset, step_ids, grid_name))
    ) %>%
    dplyr::select(x = ..object)
}

#' @importFrom utils globalVariables
utils::globalVariables(c("..object"))
