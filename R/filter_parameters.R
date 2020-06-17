#' Remove some tuning parameter results
#'
#' For objects produced by the `tune_*()` functions, there may only be a subset
#' of tuning parameter combinations of interest. For large data sets, it might be
#' helpful to be able to remove some results. This function trims the `.metrics`
#' column of unwanted results as well as columns `.predictions` and `.extracts`
#' (if they were requested).
#'
#' @param x An object of class `tune_results` that has multiple tuning parameters.
#' @param parameters A tibble of tuning parameter values that can be used to
#'  filter the predicted values before processing. This tibble should only have
#'  columns for tuning parameter identifiers (e.g. `"my_param"` if
#'  `tune("my_param")` was used). There can be multiple rows and one or more
#'  columns.
#' @return A version of `x` where the lists columns only retain the parameter
#' combinations in `parameters`.
#' @examples
#' # For grid search:
#' data("example_ames_knn")
#'
#' ## -----------------------------------------------------------------------------
#' # select all combinations using the 'rank' weighting scheme
#'
#' ames_grid_search
#' filter_parameters(ames_grid_search, tibble::tibble(weight_func = "rank"))
#'
#' ## -----------------------------------------------------------------------------
#' # Keep only the results from the numerically best combination
#'
#' ames_iter_search
#' filter_parameters(ames_iter_search, select_best(ames_iter_search, metric = "rmse"))
#' @details
#' Removing some parameter combinations might affect the results of `autoplot()`
#' for the object.
#' @export
filter_parameters <- function(x, parameters) {

  cl_x <- as.character(match.call()$x)
  # check for type
  if (!inherits(x, "tune_results")) {
    rlang::abort(paste0(cl_x, " should have class 'tune_results'."))
  }

  # check parameters vs x
  param_names <- .get_tune_parameter_names(x)
  filter_names <- names(parameters)
  if (length(intersect(filter_names, param_names)) == 0){
    msg <- paste0("There are no columns in 'parameters' that match with ",
                  as.character(cl_x))
    rlang::abort(msg)
  }
  extra_names <- setdiff(filter_names, param_names)
  if (length(extra_names) > 0) {
    msg <- paste0("There are unneeded columns in `parameters` that were ignored: ",
                  paste0("'", extra_names, "'", collapse = ", "))
    rlang::warn(msg)
    parameters <- parameters[, filter_names %in% param_names]
  }

  # run a test to make sure that there are no issues in filtering
  tst_orig <- bind_rows(x$.metrics)
  tst_filtered <- filter_iter(tst_orig, parameters)
  if (nrow(tst_filtered) == 0) {
    rlang::abort("No parameter combinations were selected using your subset.")
  }

  x$.metrics <- purrr::map(x$.metrics, filter_iter, .subset = parameters)
  if (any(names(x) == ".predictions")) {
    x$.predictions <- purrr::map(x$.predictions, filter_iter, .subset = parameters)
  }
  if (any(names(x) == ".extracts")) {
    x$.extracts <- purrr::map(x$.extracts, filter_iter, .subset = parameters)
  }

  x
}

filter_iter <- function(x, .subset) {
  if (isTRUE(nrow(x) == 0) || all(is.null(x))) {
    return(x)
  }
  dplyr::inner_join(x, .subset, by = names(.subset))
}
