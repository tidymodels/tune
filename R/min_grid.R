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


#' Determine the minimum set of model fits
#'
#' `min_grid()` determines exactly what models should be fit in order to
#'  evaluate the entire set of tuning parameter combinations. This is for
#'  internal use only and the API may change in the near future.
#'
#' `fit_max_value()` can be used in other packages to implement a `min_grid()`
#' method.
#' @param x A model specification.
#' @param grid A tibble with tuning parameter combinations.
#' @param ... Not currently used.
#' @return A tibble with the minimum tuning parameters to fit and an additional
#' list column with the parameter combinations used for prediction.
#' @keywords internal
#' @examples
#' library(dplyr)
#' library(dials)
#' library(parsnip)
#'
#' ## -----------------------------------------------------------------------------
#' ## No ability to exploit submodels:
#'
#' svm_spec <-
#'   svm_poly(cost = tune(), degree = tune()) %>%
#'   set_engine("kernlab") %>%
#'   set_mode("regression")
#'
#' svm_grid <-
#'   svm_spec %>%
#'   extract_parameter_set_dials() %>%
#'   grid_regular(levels = 3)
#'
#' min_grid(svm_spec, svm_grid)
#'
#' ## -----------------------------------------------------------------------------
#' ## Can use submodels
#'
#' xgb_spec <-
#'   boost_tree(trees = tune(), min_n = tune()) %>%
#'   set_engine("xgboost") %>%
#'   set_mode("regression")
#'
#' xgb_grid <-
#'   xgb_spec %>%
#'   extract_parameter_set_dials() %>%
#'   grid_regular(levels = 3)
#'
#' min_grid(xgb_spec, xgb_grid)
#' @export
#' @rdname min_grid
min_grid.model_spec <- function(x, grid, ...) {
  # x is a `model_spec` object from parsnip
  # grid is a tibble of tuning parameter values with names
  #  matching the parameter names.
  blank_submodels(grid)
}

# ------------------------------------------------------------------------------
# helper functions

# Template for model results that do no have the sub-model feature
blank_submodels <- function(grid) {
  grid %>%
    dplyr::mutate(.submodels = purrr::map(1:nrow(grid), ~ list())) %>%
    dplyr::mutate_if(is.factor, as.character)
}

get_fixed_args <- function(info) {
  # Get non-sub-model columns to iterate over
  fixed_args <- info$name[!info$has_submodel]
}

get_submodel_info <- function(spec) {
  if (is.null(spec$engine)) {
    rlang::abort("Please set the model's engine.")
  }
  param_info <-
    get_from_env(paste0(class(spec)[1], "_args")) %>%
    dplyr::filter(engine == spec$engine) %>%
    dplyr::select(name = parsnip, has_submodel) %>%
    dplyr::full_join(
      hardhat::extract_parameter_set_dials(spec) %>% tibble::as_tibble() %>% dplyr::select(name, id),
      by = "name"
    ) %>%
    dplyr::mutate(id = ifelse(is.na(id), name, id)) %>%
    # In case the parameter is an engine parameter
    dplyr::mutate(has_submodel = ifelse(is.na(has_submodel), FALSE, has_submodel))

  param_info
}

# Assumes only one sub-model parameter and that the fitted one is the
# maximum value
submod_only <- function(grid) {
  if (nrow(grid) == 1) {
    grid$.submodels <- list(list())
    return(grid)
  }
  nm <- colnames(grid)[1]
  fit_only <- tibble::tibble(nm = max(grid[[nm]], na.rm = TRUE))
  names(fit_only) <- nm
  sub_mods <- list(grid[[nm]][-which.max(grid[[nm]])])
  names(sub_mods) <- nm
  fit_only$.submodels <- list(sub_mods)
  dplyr::select(fit_only, dplyr::one_of(names(grid)), .submodels)
}

# Assumes more than one sub-model parameter and that the fitted are the
# maximum values
submod_only_multi <- function(grid) {
  if (nrow(grid) == 1) {
    grid$.submodels <- list(list())
    return(grid)
  }
  nm <- colnames(grid)

  # The incoming grid is not necessarily sorted by all sub-model parameters
  # so, let's just get the max of the first parameter and move it.
  max_idx <- which.max(grid[[1]]) # this only returns the first max idx.

  fit_only <- grid[max_idx, , drop = FALSE]
  sub_mods <- as.list(grid[-max_idx, , drop = FALSE])
  fit_only$.submodels <- list(sub_mods)
  dplyr::select(fit_only, dplyr::all_of(nm), .submodels)
}

# Assumes only one sub-model parameter and that the fitted one is the
# maximum value
submod_and_others <- function(grid, fixed_args) {
  orig_names <- names(grid)
  subm_nm <- orig_names[!(orig_names %in% fixed_args)]

  # avoid more rlangedness related to names until end:
  grid <- grid %>% dplyr::rename(..val = !!subm_nm)

  fit_only <-
    grid %>%
    dplyr::group_by(!!!rlang::syms(fixed_args)) %>%
    dplyr::summarize(max_val = max(..val, na.rm = TRUE)) %>%
    dplyr::ungroup()

  min_grid_df <-
    dplyr::full_join(fit_only, grid, by = fixed_args) %>%
    dplyr::filter(..val != max_val) %>%
    dplyr::group_by(!!!rlang::syms(fixed_args)) %>%
    dplyr::summarize(.submodels = list(tibble::lst(!!subm_nm := ..val))) %>%
    dplyr::ungroup() %>%
    dplyr::full_join(fit_only, by = fixed_args) %>%
    dplyr::rename(!!subm_nm := max_val)

  min_grid_df$.submodels <-
    dplyr::if_else(
      !purrr::map_lgl(min_grid_df$.submodels, rlang::is_null),
      min_grid_df$.submodels,
      purrr::map(1:nrow(min_grid_df), ~ list())
    )

  dplyr::select(min_grid_df, dplyr::one_of(orig_names), .submodels) %>%
    dplyr::mutate_if(is.factor, as.character)
}

# Assumes more than one sub-model parameter and that the fitted are the
# maximum values
submod_and_others_multi <- function(grid, fixed_args) {
  orig_names <- colnames(grid)
  subm_nm <- orig_names[!(orig_names %in% fixed_args)]

  # ugly, but does the job:
  # stores the row number for later use
  # group by the fixed_args
  # select the max from the first sub-model parameter
  # select the correspondent line on the other parameters
  fit_only <- grid %>%
    dplyr::mutate(idxs = dplyr::row_number()) %>%
    dplyr::group_by(!!!rlang::syms(fixed_args)) %>%
    dplyr::summarize(dplyr::across(c(!!subm_nm, "idxs"), ~ (.x[which.max(.data[[subm_nm[1]]])]))) %>%
    dplyr::ungroup()

  # now filter only the combinations not in the `fit_only`
  # and nest it in a named list.
  # No need for `rlang::is_null` here.
  submodels <- grid %>%
    dplyr::mutate(idxs = dplyr::row_number()) %>%
    dplyr::group_by(!!!rlang::syms(fixed_args)) %>%
    dplyr::arrange(!!!rlang::syms(fixed_args)) %>%
    dplyr::filter(!(idxs %in% fit_only$idxs)) %>%
    dplyr::summarize(.submodels = list(as.list(dplyr::across(!!subm_nm, ~ .x)))) %>%
    dplyr::ungroup()

  # join and select only what matters
  min_grid_df <- fit_only %>%
    dplyr::full_join(submodels, by = fixed_args)

  dplyr::select(min_grid_df, dplyr::all_of(orig_names), .submodels) %>%
    dplyr::mutate_if(is.factor, as.character)
}

# Determine the correct sub-model structure when the sub-model parameter's
# fit value should be the maximum (e.g. fit the largest number of boosting
# iterations and use muti_predict() for the others)
# Assumes a single sub-model parameter
#' @export
#' @rdname min_grid
fit_max_value <- function(x, grid, ...) {
  gr_nms <- names(grid)
  param_info <- get_submodel_info(x)
  sub_nm <- param_info$id[param_info$has_submodel]

  if (length(sub_nm) == 0 | !any(names(grid) %in% sub_nm)) {
    return(blank_submodels(grid))
  }

  fixed_args <- gr_nms[!(gr_nms %in% sub_nm)] # allow multiple submodel args support

  if (length(fixed_args) == 0) {
    res <- submod_only(grid)
  } else {
    res <- submod_and_others(grid, fixed_args)
  }
  res
}


# ------------------------------------------------------------------------------
# specific methods

# ------------------------------------------------------------------------------
# Boosted trees

#' @export
#' @export min_grid.boost_tree
#' @rdname min_grid
min_grid.boost_tree <- fit_max_value

# ------------------------------------------------------------------------------
# linear regression

#' @export
#' @export min_grid.linear_reg
#' @rdname min_grid
min_grid.linear_reg <- function(x, grid, ...) {
  # This is basically `fit_max_value()` with an extra error trap
  gr_nms <- names(grid)
  param_info <- get_submodel_info(x)
  sub_nm <- param_info$id[param_info$has_submodel]

  if (x$engine == "glmnet") {
    no_penalty(grid, sub_nm)
  }

  if (length(sub_nm) == 0) {
    return(blank_submodels(grid))
  }

  fixed_args <- gr_nms[gr_nms != sub_nm]

  if (length(fixed_args) == 0) {
    res <- submod_only(grid)
  } else {
    res <- submod_and_others(grid, fixed_args)
  }
  res
}

no_penalty <- function(x, nm) {
  if (length(nm) == 0 || all(colnames(x) != nm)) {
    rlang::abort("At least one penalty value is required for glmnet.")
  }
  invisible(NULL)
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
min_grid.mars <- fit_max_value

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
min_grid.nearest_neighbor <- fit_max_value

## -----------------------------------------------------------------------------
## rules package

#' @export
#' @export min_grid.cubist_rules
#' @rdname min_grid
min_grid.cubist_rules <- fit_max_value

#' @export
#' @export min_grid.C5_rules
#' @rdname min_grid
min_grid.C5_rules <- fit_max_value

#' @export
#' @export min_grid.rule_fit
#' @rdname min_grid
min_grid.rule_fit <- fit_max_value

## -----------------------------------------------------------------------------
## plsmod package

#' @export
#' @export min_grid.pls
#' @rdname min_grid
min_grid.pls <- fit_max_value

## -----------------------------------------------------------------------------
## poissonreg package

#' @export
#' @export min_grid.poisson_reg
#' @rdname min_grid
min_grid.poisson_reg <- fit_max_value
