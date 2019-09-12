#' Obtain and format predictions produced by tuning
#'
#' @param x The results of `tune_grid()` or `tune_Bayes()` where the predictions
#' were saved.
#' @param nest_by A character string to indicate which (if any) columns should
#' be in the resulting tibble and which should be nested. Values are `"nothing"`
#' (i.e. no nesting), `"resamples"` (nest the parameter values), and `"parameters"`
#' (nest the resamples).
#' @param wflow The workflow associated with the results (only required for
#' `nest_by = "parameters"`).
#' @return A tibble. The column names depend on the results and the mode of the
#' model.
#' @export
get_predictions <- function(x, nest_by = "nothing", wflow = NULL) {
  nests <- c("nothing", "resamples", "parameters")
  if (!any(nest_by == nests)) {
    stop(
      "`nest_by` should be one of ",
      glue::glue_collapse(paste0("'", nests, "'", collapse = ", ")),
      call. = FALSE
    )
  }
  if (any(colnames(x) == ".iter")) {
    keep_cols <- c(".predictions", ".iter")
  } else {
    keep_cols <- ".predictions"
  }
  x <- dplyr::select(x, dplyr::starts_with("id"), !!!keep_cols)
  if (nest_by == "nothing") {
    x <- tidyr::unnest(x)
  } else {
    if (nest_by == "parameters") {
      if (is.null(wflow)) {
        stop("Please supply the corresponding workflow object.", call. = FALSE)
      }
      keep_cols <- param_set(wflow)$id
      if (any(colnames(x) == ".iter")) {
        keep_cols <- c(keep_cols, ".iter")
      }

      x <- tidyr::unnest(x)
      if (tidyr_new_interface()) {
        x <- tidyr::nest(x, .predictions = -!!keep_cols)
      } else {
        x <- tidyr::nest(-!!keep_cols, .key = ".predictions")
      }
    }
  }
  x
}

# ------------------------------------------------------------------------------

tidyr_new_interface <- function() {
  packageVersion("tidyr") > "0.8.99"
}



