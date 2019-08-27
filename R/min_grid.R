#' Determine the minimum set of model fits
#'
#' `min_grid` determines exactly what models should be fit in order to
#'  evaluate the entire set of tuning parameter combinations. This is for
#'  internal use only and the API may change in the near future.
#' @param x A model specification.
#' @param grid A tibble with tuning parameter combinations.
#' @param ... Not currently used.
#' @return A tibble with the minimum tuning parameters to fit and an additional
#' list column with the parameter combinations used for prediction.
#' @keywords internal
#' @export
min_grid <- function(x, grid, ...) {
  # x is a `model_spec` object from parsnip
  # grid is a tibble of tuning parameter values with names
  #  matching the parameter names.
  UseMethod("min_grid")
}

# As an example, if we fit a boosted tree  model and tune over
# trees = 1:20 and min_n = c(20, 30)
# we should only have to fit two models:
#
#   trees = 20 & min_n = 20
#   trees = 20 & min_n = 30
#
# The logic related to how this "mini grid" gets made is model-specific.
#
# To get the full set of predictions, we need to know, for each of these two
# models, what values of num_terms to give to the multi_predict() function.
#
# The current idea is to have a list column of the extra models for prediction.
# For the example above:
#
#   # A tibble: 2 x 3
#     trees min_n .submodels
#     <dbl> <dbl> <list>
#   1    20    20 <named list [1]>
#   2    20    30 <named list [1]>
#
# and the .submodels would both be
#
#  list(trees = 1:19)
#
# There are a lot of other things to consider in future versions like grids
# where there are multiple columns with the same name (maybe the results of
# a recipe) and so on.

#'@export
#'@rdname min_grid
min_grid.model_spec <- function(x, grid, ...) {
  blank_submodels(grid)
}

# ------------------------------------------------------------------------------
# helper functions

# Template for model results that do no have the sub-model feature
blank_submodels <- function(grid) {
  grid %>%
    dplyr::mutate(.submodels = map(1:nrow(grid), ~ list()))
}

get_fixed_args <- function(info) {
  # Get non-sub-model columns to iterate over
  fixed_args <- info$name[!info$has_submodel]
}

get_submodel_info <- function(spec, grid) {
  param_info <-
    get_from_env(paste0(class(spec)[1], "_args")) %>%
    dplyr::filter(engine == spec$engine) %>%
    dplyr::select(name = parsnip, has_submodel)

  # In case a recipe or other activity has grid parameter columns,
  # add those to the results
  grid_names <- names(grid)
  is_mod_param <- grid_names %in% param_info$name
  if (any(!is_mod_param)) {
    param_info <-
      param_info %>%
      dplyr::bind_rows(
        tibble::tibble(name = grid_names[!is_mod_param],
                       has_submodel = FALSE)
      )
  }
  param_info %>% dplyr::filter(name %in% grid_names)
}

# ------------------------------------------------------------------------------
# specific methods

# ------------------------------------------------------------------------------
# Boosted trees

#' @export
#' @export min_grid.boost_tree
#' @rdname min_grid
min_grid.boost_tree <- function(x, grid, ...) {
  grid_names <- names(grid)
  param_info <- get_submodel_info(x, grid)

  # No ability to do submodels? Finish here:
  if (!any(param_info$has_submodel)) {
    return(blank_submodels(grid))
  }

  fixed_args <- get_fixed_args(param_info)

  # For boosted trees, fit the model with the most trees (conditional on the
  # other parameters) so that you can do predictions on the smaller models.
  fit_only <-
    grid %>%
    dplyr::group_by(!!!rlang::syms(fixed_args)) %>%
    dplyr::summarize(trees = max(trees, na.rm = TRUE)) %>%
    dplyr::ungroup()

  # Add a column .submodels that is a list with what should be predicted
  # by `multi_predict()` (assuming `predict()` has already been executed
  # on the original value of 'trees')
  min_grid_df <-
    dplyr::full_join(fit_only %>% rename(max_tree = trees), grid, by = fixed_args) %>%
    dplyr::filter(trees != max_tree) %>%
    dplyr::group_by(!!!rlang::syms(fixed_args)) %>%
    dplyr::summarize(.submodels = list(list(trees = trees))) %>%
    dplyr::ungroup() %>%
    dplyr::full_join(fit_only, grid, by = fixed_args)

  min_grid_df  %>% dplyr::select(dplyr::one_of(grid_names), .submodels)
}

# ------------------------------------------------------------------------------
# linear regression

#' @export
#' @export min_grid.linear_reg
#' @rdname min_grid
min_grid.linear_reg <- function(x, grid, ...) {
  no_penalty(grid)

  grid_names <- names(grid)
  param_info <- get_submodel_info(x, grid)

  if (!any(param_info$has_submodel)) {
    return(blank_submodels(grid))
  }

  fixed_args <- get_fixed_args(param_info)

  if (all(names(grid) == "penalty")) {
    res <- penalty_only(grid)
  } else {
    if (length(unique(grid$penalty)) == 1) {
      res <- one_penalty(grid)
    } else {
      res <- penalty_and_others(grid, fixed_args)
    }
  }
  res
}


no_penalty <- function(x) {
  if (all(colnames(x) != "penalty")) {
    stop("At least one penalty value is required for glmnet.", call. = FALSE)
  }
  invisible(NULL)
}

penalty_only <- function(grid) {
  fit_only <- tibble(penalty = max(grid$penalty, na.rm = TRUE))
  fit_only$.submodels <- list(list(penalty = grid$penalty[-which.max(grid$penalty)]))
  dplyr::select(fit_only, dplyr::one_of(names(grid)), .submodels)
}

one_penalty <- function(grid) {
  grid$.submodels <- list(list())
  grid
}


penalty_and_others <- function(grid, fixed_args) {
  fit_only <-
    grid %>%
    dplyr::group_by(!!!rlang::syms(fixed_args)) %>%
    dplyr::summarize(penalty = max(penalty, na.rm = TRUE)) %>%
    dplyr::ungroup()
  min_grid_df <-
    dplyr::full_join(fit_only %>% rename(max_penalty = penalty), grid, by = fixed_args) %>%
    dplyr::filter(penalty != max_penalty) %>%
    dplyr::group_by(!!!rlang::syms(fixed_args)) %>%
    dplyr::summarize(.submodels = list(list(penalty = penalty))) %>%
    dplyr::ungroup() %>%
    dplyr::full_join(fit_only, grid, by = fixed_args)
  dplyr::select(min_grid_df, dplyr::one_of(names(grid)), .submodels)
}


# ------------------------------------------------------------------------------
# logistic regression


#' @export
#' @export min_grid.logistic_reg
#' @rdname min_grid
min_grid.logistic_reg <- min_grid.linear_reg


# ------------------------------------------------------------------------------
# mars


#' @export
#' @export min_grid.mars
#' @rdname min_grid
min_grid.mars <- function(x, grid, ...) {

  grid_names <- names(grid)
  param_info <- get_submodel_info(x, grid)

  if (!any(param_info$has_submodel)) {
    return(blank_submodels(grid))
  }

  fixed_args <- get_fixed_args(param_info)

  fit_only <-
    grid %>%
    dplyr::group_by(!!!rlang::syms(fixed_args)) %>%
    dplyr::summarize(num_terms = max(num_terms, na.rm = TRUE)) %>%
    dplyr::ungroup()

  min_grid_df <-
    dplyr::full_join(fit_only %>% rename(max_terms = num_terms), grid, by = fixed_args) %>%
    dplyr::filter(num_terms != max_terms) %>%
    dplyr::group_by(!!!rlang::syms(fixed_args)) %>%
    dplyr::summarize(.submodels = list(list(num_terms = num_terms))) %>%
    dplyr::ungroup() %>%
    dplyr::full_join(fit_only, grid, by = fixed_args)

  min_grid_df  %>% dplyr::select(dplyr::one_of(grid_names), .submodels)
}

# ------------------------------------------------------------------------------
# multinomial regression

#' @export
#' @export min_grid.multinom_reg
#' @rdname min_grid
min_grid.multinom_reg <- min_grid.linear_reg

# ------------------------------------------------------------------------------
# Knn

#' @export
#' @export min_grid.nearest_neighbor
#' @rdname min_grid
min_grid.nearest_neighbor <- function(x, grid, ...) {

  grid_names <- names(grid)
  param_info <- get_submodel_info(x, grid)

  if (!any(param_info$has_submodel)) {
    return(blank_submodels(grid))
  }

  fixed_args <- get_fixed_args(param_info)

  fit_only <-
    grid %>%
    dplyr::group_by(!!!rlang::syms(fixed_args)) %>%
    dplyr::summarize(neighbors = max(neighbors, na.rm = TRUE)) %>%
    dplyr::ungroup()

  min_grid_df <-
    dplyr::full_join(fit_only %>% rename(max_neighbor = neighbors), grid, by = fixed_args) %>%
    dplyr::filter(neighbors != max_neighbor) %>%
    dplyr::rename(sub_neighbors = neighbors, neighbors = max_neighbor) %>%
    dplyr::group_by(!!!rlang::syms(fixed_args)) %>%
    dplyr::summarize(.submodels = list(list(neighbors = sub_neighbors))) %>%
    dplyr::ungroup() %>%
    dplyr::full_join(fit_only, grid, by = fixed_args)

  min_grid_df  %>% dplyr::select(dplyr::one_of(grid_names), .submodels)
}




