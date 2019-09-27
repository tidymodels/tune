#' Estimate performance values
#'
#' @param .data An object.
#' @param ... Not currently used.
#' @return A tibble with parameter values and summaries of their results. A
#' column for the average (`mean`), the standard error of the mean (`std_err`),
#' and the number of non-missing values (`n`). These are computed for each
#' metric and estimator type.
#' @export
#' @rdname summarize.tune_results
summarise.tune_results <- function(.data, ...) {
  all_bad <- is_cataclysmic(.data)
  if (all_bad) {
    stop("All of the models failed.", call. = FALSE)
  }

  tibble_metrics <- purrr::map_lgl(.data$.metrics, tibble::is_tibble)
  .data <- .data[tibble_metrics, ]

  if (any(names(.data) == ".iter")) {
    keep_cols <- c(".iter", ".metrics")
  } else {
    keep_cols <- ".metrics"
  }
  .data <- tidyr::unnest(.data, cols = dplyr::one_of(keep_cols))
  all_col <- names(.data)
  excl_cols <- c(".metric", ".estimator", ".estimate", "splits",
                 grep("^id", all_col, value = TRUE), ".predictions", ".extracts")
  param_names <- all_col[!(all_col %in% excl_cols)]
  .data %>%
    tibble::as_tibble() %>%
    dplyr::group_by(!!!rlang::syms(param_names), .metric, .estimator) %>%
    dplyr::summarize(
      mean = mean(.estimate, na.rm = TRUE),
      n = sum(!is.na(.estimate)),
      std_err = sd(.estimate, na.rm = TRUE)/sqrt(n)
    ) %>%
    ungroup()
}

