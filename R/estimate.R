#' Estimate performance values
#'
#' @param x An object.
#' @param ... Not currently used.
#' @return A tibble with parameter values and summaries of their results. A
#' column for the average (`mean`), the standard error of the mean (`std_err`),
#' and the number of non-missing values (`n`). These are computed for each
#' metric and estimator type.
#' @export
#' @rdname estimate.tune_results
estimate <- function(x, ...) {
  UseMethod("estimate")
}

#' @export
#' @rdname estimate.tune_results
estimate.tune_results <- function(x, ...) {
  all_bad <- is_cataclysmic(x)
  if (all_bad) {
    stop("All of the models failed.", call. = FALSE)
  }

  tibble_metrics <- purrr::map_lgl(x$.metrics, tibble::is_tibble)
  x <- x[tibble_metrics, ]

  if (any(names(x) == ".iter")) {
    keep_cols <- c(".iter", ".metrics")
  } else {
    keep_cols <- ".metrics"
  }
  x <- tidyr::unnest(x, cols = dplyr::one_of(keep_cols))
  all_col <- names(x)
  excl_cols <- c(".metric", ".estimator", ".estimate", "splits", ".notes",
                 grep("^id", all_col, value = TRUE), ".predictions", ".extracts")
  param_names <- all_col[!(all_col %in% excl_cols)]
  x %>%
    tibble::as_tibble() %>%
    dplyr::group_by(!!!rlang::syms(param_names), .metric, .estimator) %>%
    dplyr::summarize(
      mean = mean(.estimate, na.rm = TRUE),
      n = sum(!is.na(.estimate)),
      std_err = sd(.estimate, na.rm = TRUE)/sqrt(n)
    ) %>%
    ungroup()
}

