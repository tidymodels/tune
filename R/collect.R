#' Obtain and format results produced by tuning
#'
#' @param x The results of [tune_grid()] or [tune_bayes()]. For
#' [collect_predictions()], the control option `save_pred = TRUE` should have
#' been used.
#' @param nest_by A character string to indicate which (if any) columns should
#' be in the resulting tibble and which should be nested. Values are `"nothing"`
#' (i.e. no nesting), `"resamples"` (nest the parameter values), and `"parameters"`
#' (nest the resamples).
#' @param summarize A logical; should metrics be summarized over resamples
#' (`TRUE`) or return the values for each individual resample. Only appropriate
#' when `nest_by = "nothing"`.
#' @param wflow The workflow associated with the results (only required for
#' `nest_by = "parameters"`).
#' @return A tibble. The column names depend on the results and the mode of the
#' model.
#' @examples
#' data("example_ames_knn")
#'
#' # Summarized over resamples
#' collect_metrics(ames_grid_search)
#'
#' # Per-resample values
#' collect_metrics(ames_grid_search, summarize = FALSE)
#' @export
collect_predictions <- function(x, nest_by = "nothing", wflow = NULL) {
  collector(x, coll_col = ".predictions", nest_by, wflow)
}

#' @export
#' @rdname collect_predictions
collect_metrics <- function(x, nest_by = "nothing", summarize = TRUE, wflow = NULL) {
  if (summarize) {
    if (nest_by != "nothing") {
      rlang::abort("If `summarize = TRUE`, `nest_by` should be `'nothing'`.")
    }
    res <- estimate_tune_results(x)
  } else {
    res <- collector(x, coll_col = ".metrics", nest_by, wflow)

  }
  res
}


collector <- function(x, coll_col = ".predictions", nest_by = "nothing", wflow = NULL) {
  nests <- c("nothing", "resamples", "parameters")
  if (!any(nest_by == nests)) {
    stop(
      "`nest_by` should be one of ",
      glue::glue_collapse(paste0("'", nests, "'", collapse = ", ")),
      call. = FALSE
    )
  }
  if (any(colnames(x) == ".iter")) {
    keep_cols <- c(coll_col, ".iter")
  } else {
    keep_cols <- coll_col
  }
  x <- dplyr::select(x, dplyr::starts_with("id"), !!!keep_cols)
  if (nest_by == "nothing") {
    x <- tidyr::unnest(x, cols = c(dplyr::one_of(coll_col)))
  } else {
    if (nest_by == "parameters") {
      if (is.null(wflow)) {
        stop("Please supply the corresponding workflow object.", call. = FALSE)
      }
      keep_cols <- dials::parameters(wflow)$id
      if (any(colnames(x) == ".iter")) {
        keep_cols <- c(keep_cols, ".iter")
      }

      x <- tidyr::unnest(x, cols = c(dplyr::one_of(coll_col)))
      if (tidyr_new_interface()) {
        x <- tidyr::nest(x, cols = c(-dplyr::one_of(keep_cols)))
        names(x)[ncol(x)] <- coll_col
      } else {
        x <- tidyr::nest(-!!keep_cols, .key = ".predictions")
      }
    }
  }
  x
}

estimate_tune_results <- function(x, ...) {
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

