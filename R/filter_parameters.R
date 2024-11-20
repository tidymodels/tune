#' Remove some tuning parameter results
#'
#' For objects produced by the `tune_*()` functions, there may only be a subset
#' of tuning parameter combinations of interest. For large data sets, it might be
#' helpful to be able to remove some results. This function trims the `.metrics`
#' column of unwanted results as well as columns `.predictions` and `.extracts`
#' (if they were requested).
#'
#' @param x An object of class `tune_results` that has multiple tuning parameters.
#' @param ... Expressions that return a logical value, and are defined in terms
#'  of the tuning parameter values. If multiple expressions are included, they
#'  are combined with the `&` operator. Only rows for which all conditions
#'  evaluate to `TRUE` are kept.
#' @param parameters A tibble of tuning parameter values that can be used to
#'  filter the predicted values before processing. This tibble should only have
#'  columns for tuning parameter identifiers (e.g. `"my_param"` if
#'  `tune("my_param")` was used). There can be multiple rows and one or more
#'  columns. **If used, this parameter must be named.**
#' @return A version of `x` where the lists columns only retain the parameter
#' combinations in `parameters` or satisfied by the filtering logic.
#' @examples
#' library(dplyr)
#' library(tibble)
#'
#' # For grid search:
#' data("example_ames_knn")
#'
#' ## -----------------------------------------------------------------------------
#' # select all combinations using the 'rank' weighting scheme
#'
#' ames_grid_search %>%
#'   collect_metrics()
#'
#' filter_parameters(ames_grid_search, weight_func == "rank") %>%
#'   collect_metrics()
#'
#' rank_only <- tibble::tibble(weight_func = "rank")
#' filter_parameters(ames_grid_search, parameters = rank_only) %>%
#'   collect_metrics()
#'
#' ## -----------------------------------------------------------------------------
#' # Keep only the results from the numerically best combination
#'
#' ames_iter_search %>%
#'   collect_metrics()
#'
#' best_param <- select_best(ames_iter_search, metric = "rmse")
#' ames_iter_search %>%
#'   filter_parameters(parameters = best_param) %>%
#'   collect_metrics()
#' @details
#' Removing some parameter combinations might affect the results of `autoplot()`
#' for the object.
#' @export
filter_parameters <- function(x, ..., parameters = NULL) {
  cl_x <- as.character(match.call()$x)
  check_filter_dots(rlang::enquos(...))
  # check for type
  if (!inherits(x, "tune_results")) {
    cli::cli_abort("{.arg {caller_arg(x)}} should have class {.cls tune_results};
                    {.obj_type_friendly {x}} was passed.")
  }
  x <- filter_by_join(x, parameters, nm = cl_x)
  x <- filter_by_filter(x, ...)
  x
}

filter_by_join <- function(x, parameters = NULL, nm = "") {
  if (is.null(parameters)) {
    return(x)
  }
  # check parameters vs x
  param_names <- .get_tune_parameter_names(x)
  filter_names <- names(parameters)
  filter_names <- filter_names[filter_names != ".config"]
  if (length(intersect(filter_names, param_names)) == 0) {
    cli::cli_abort(
      "There are no columns in {.arg parameters} that match with {.val {nm}}."
    )
  }
  extra_names <- setdiff(filter_names, param_names)
  if (length(extra_names) > 0) {
    cli_warn(
      "{qty(extra_names)} The column{?s} {.var {extra_names}} passed in \\
       {.arg parameters} {?is/are} not needed and will be ignored."
    )

    parameters <- parameters[, filter_names %in% param_names]
  }

  # run a test to make sure that there are no issues in filtering
  tst_orig <- bind_rows(x$.metrics)
  tst_filtered <- filter_join_iter(tst_orig, parameters)
  if (nrow(tst_filtered) == 0) {
    cli::cli_abort("No parameter combinations were selected using your subset.")
  }

  x$.metrics <- purrr::map(x$.metrics, filter_join_iter, .subset = parameters)
  if (any(names(x) == ".predictions")) {
    x$.predictions <- purrr::map(x$.predictions, filter_join_iter, .subset = parameters)
  }
  if (any(names(x) == ".extracts")) {
    x$.extracts <- purrr::map(x$.extracts, filter_join_iter, .subset = parameters)
  }

  x
}

filter_join_iter <- function(x, .subset) {
  if (isTRUE(nrow(x) == 0) || all(is.null(x))) {
    return(x)
  }
  dplyr::inner_join(x, .subset, by = names(.subset))
}

filter_by_filter <- function(x, ...) {
  dots <- rlang::enquos(...)
  if (rlang::is_empty(dots)) {
    return(x)
  }
  x$.metrics <- purrr::map(x$.metrics, ~ dplyr::filter(.x, !!!dots))
  if (any(names(x) == ".predictions")) {
    x$.predictions <- purrr::map(x$.predictions, ~ dplyr::filter(.x, !!!dots))
  }
  if (any(names(x) == ".extracts")) {
    x$.extracts <- purrr::map(x$.extracts, ~ dplyr::filter(.x, !!!dots))
  }
  x
}

check_filter_dots <- function(dots, call = rlang::caller_env()) {
  res <- purrr::map(dots, ~try(rlang::eval_tidy(.x), silent = TRUE))

  if (any(purrr::map_lgl(res, inherits, "data.frame"))) {
    cli::cli_abort(
      c(
        "An element passed to {.arg ...} is a data frame rather than a filter
         expression.",
        "i" = "Did you forget to name the {.arg parameters} argument?"
      ),
      call = call
    )
  }
}
