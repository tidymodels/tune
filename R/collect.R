#' Obtain and format results produced by tuning
#'
#' @param x The results of [tune_grid()] or [tune_bayes()]. For
#' [collect_predictions()], the control option `save_pred = TRUE` should have
#' been used.
#' @param summarize A logical; should metrics be summarized over resamples
#' (`TRUE`) or return the values for each individual resample.
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
#'
#' # ---------------------------------------------------------------------------
#'
#' library(parsnip)
#' lm_mod <-linear_reg() %>% set_engine("lm")
#' set.seed(93599150)
#' car_folds <- vfold_cv(mtcars)
#' ctrl <- control_resamples(save_pred = TRUE)
#'
#' resampled <- fit_resamples(mpg ~ ., lm_mod, resamples = car_folds, control = ctrl)
#' collect_predictions(resampled)
#' @export
collect_predictions <- function(x) {
  collector(x, coll_col = ".predictions")
}

#' @export
#' @rdname collect_predictions
collect_metrics <- function(x, summarize = TRUE) {
  if (summarize) {
    res <- estimate_tune_results(x)
  } else {
    res <- collector(x, coll_col = ".metrics")
  }
  res
}


collector <- function(x, coll_col = ".predictions") {
  if (any(colnames(x) == ".iter")) {
    keep_cols <- c(coll_col, ".iter")
  } else {
    keep_cols <- coll_col
  }
  x <- dplyr::select(x, dplyr::starts_with("id"), !!!keep_cols)
  x <- tidyr::unnest(x, cols = c(dplyr::one_of(coll_col)))
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

