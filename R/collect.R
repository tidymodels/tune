#' Obtain and format results produced by tuning functions
#'
#' @param x The results of [tune_grid()], [tune_bayes()], [fit_resamples()],
#'  or [last_fit()]. For [collect_predictions()], the control option `save_pred
#'  = TRUE` should have been used.
#' @param summarize A logical; should metrics be summarized over resamples
#' (`TRUE`) or return the values for each individual resample. Note that, if `x`
#' is created by [last_fit()], `summarize` has not effect.
#' @param parameters An optional tibble of tuning parameter values that can be
#'  used to filter the predicted values before processing. This tibble should
#'  only have columns for each tuning parameter identifier (e.g. `"my_param"``
#'  if `tune("my_param")` was used).
#' @return A tibble. The column names depend on the results and the mode of the
#' model.
#'
#' For [collect_metrics()] and [collect_predictions()], there are columns for
#' each tuning parameter (using the `id` from [tune()], if any).
#'
#' For [collect_predictions()], there are additional columns for the resampling
#' identifier(s), columns for the predicted values (e.g., `.pred`,
#' `.pred_class`, etc.), and a column for the outcome(s) using the original
#' column name(s) in the data.
#'
#' [collect_metrics()] also has columns `.metric`, and `.estimator`.  When the
#' results are summarized, there are columns for `mean`, `n`, and `std_err`.
#' When not summarized, the additional columns for the resampling identifier(s)
#' and `.estimate`.
#' @examples
#' \donttest{
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
#' library(rsample)
#'
#' lm_mod <-linear_reg() %>% set_engine("lm")
#' set.seed(93599150)
#' car_folds <- vfold_cv(mtcars)
#' ctrl <- control_resamples(save_pred = TRUE)
#'
#' resampled <- fit_resamples(mpg ~ ., lm_mod, resamples = car_folds, control = ctrl)
#' collect_predictions(resampled)
#' }
#' @export
collect_predictions <- function(x, parameters = NULL) {
  # TODO check classes of x
  # add grid argument
  # add summarize argument

  names <- colnames(x)
  coll_col <- ".predictions"

  has_coll_col <- coll_col %in% names

  if (!has_coll_col) {
    msg <- paste0(
      "The `.predictions` column does not exist. ",
      "Refit with the control argument `save_pred = TRUE` to save predictions."
    )

    rlang::abort(msg)
  }

  x <- filter_predictions(x, parameters)

  collector(x, coll_col = coll_col)
}

filter_predictions <- function(x, parameters) {
  if (is.null(parameters)) {
    return(x)
  }
  metrics <- attr(x, "metrics")
  params <- attr(x, "parameters")
  if (is.null(metrics) | is.null(params)) {
    rlang::warn(
      paste(
        strwrap(
          paste("The object is missing some attributes; it is probably from",
                "an earlier version of `tune`. The predictions can't be filtered." ),
          prefix = ""
        ),
        collapse = "\n"
      )
    )

    return(x)
  }

  metric_types <- metrics_info(metrics)$type
  param_names <- params$id
  parameters <- dplyr::select(parameters, dplyr::one_of(param_names))
  if (ncol(parameters) != length(param_names)) {
    rlang::abort(
      paste0("`parameters` should only have columns: ",
             paste0("'", param_names, "'", collapse = ", "))
    )
  }
  x$.predictions <-
    purrr::map(x$.predictions, dplyr::inner_join, parameters, by = param_names)
  x
}


# ------------------------------------------------------------------------------

#' @export
#' @rdname collect_predictions
collect_metrics <- function(x, summarize = TRUE) {

  if (inherits(x, "last_fit")) {
    return(x$.metrics[[1]])
  }

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

