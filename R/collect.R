#' Obtain and format results produced by tuning functions
#'
#' @param x The results of [tune_grid()], [tune_bayes()], [fit_resamples()],
#'  or [last_fit()]. For [collect_predictions()], the control option `save_pred
#'  = TRUE` should have been used.
#' @param summarize A logical; should metrics be summarized over resamples
#' (`TRUE`) or return the values for each individual resample. Note that, if `x`
#' is created by [last_fit()], `summarize` has no effect. For the other object
#' types, the method of summarizing predictions is detailed below.
#' @param parameters An optional tibble of tuning parameter values that can be
#'  used to filter the predicted values before processing. This tibble should
#'  only have columns for each tuning parameter identifier (e.g. `"my_param"`
#'  if `tune("my_param")` was used).
#' @return A tibble. The column names depend on the results and the mode of the
#' model.
#'
#' For [collect_metrics()] and [collect_predictions()], when unsummarized,
#' there are columns for each tuning parameter (using the `id` from [tune()],
#' if any).
#' [collect_metrics()] also has columns `.metric`, and `.estimator`.  When the
#' results are summarized, there are columns for `mean`, `n`, and `std_err`.
#' When not summarized, the additional columns for the resampling identifier(s)
#' and `.estimate`.
#'
#' For [collect_predictions()], there are additional columns for the resampling
#' identifier(s), columns for the predicted values (e.g., `.pred`,
#' `.pred_class`, etc.), and a column for the outcome(s) using the original
#' column name(s) in the data.
#'
#' [collect_predictions()] can summarize the various results over
#'  replicate out-of-sample predictions. For example, when using the bootstrap,
#'  each row in the original training set has multiple holdout predictions
#'  (across assessment sets). To convert these results to a format where every
#'  training set same has a single predicted value, the results are averaged
#'  over replicate predictions.
#'
#' For regression cases, the numeric predictions are simply averaged. For
#'  classification models, the problem is more complex. When class probabilities
#'  are used, these are averaged and then re-normalized to make sure that they
#'  add to one. If hard class predictions also exist in the data, then these are
#'  determined from the summarized probability estimates (so that they match).
#'  If only hard class predictions are in the results, then the mode is used to
#'  summarize.
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
#' library(dplyr)
#'
#' lm_mod <-linear_reg() %>% set_engine("lm")
#' set.seed(93599150)
#' car_folds <- vfold_cv(mtcars, v = 2, repeats = 3)
#' ctrl <- control_resamples(save_pred = TRUE)
#'
#' resampled <- fit_resamples(mpg ~ ., lm_mod, resamples = car_folds, control = ctrl)
#' collect_predictions(resampled) %>% arrange(.row)
#' collect_predictions(resampled, summarize = TRUE) %>% arrange(.row)
#' }
#' @export
collect_predictions <- function(x, summarize = FALSE, parameters = NULL) {
  # TODO check classes of x

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

  if (summarize) {
    x <- average_predictions(x, parameters)
  } else {
    x <- collector(x, coll_col = coll_col)
  }
  x
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

# Functions for summarizing the various results over replicate out-of-sample
#  predictions. For regression cases, the numeric predictions are simply
#  averaged. For classification models, the problem is more complex. When class
#  probabilities are used, these are averaged and then re-normalized to make
#  sure that they add to one. If hard class predictions also exist in the data,
#  then these are determined from the summarized probability estimates (so that
#  they match). If only hard class predictions are in the results, then the
#  mode is used to summarize.

numeric_summarize <- function(x, p, wider = TRUE) {
  pred_cols <- grep("^\\.pred", names(x), value = TRUE)
  nms <- names(x)
  group_cols <- nms[!(nms %in% pred_cols)]
  x <-
    x %>%
    tidyr::pivot_longer(
      cols = c(dplyr::one_of(pred_cols)),
      names_to = ".column",
      values_to = ".value"
    ) %>%
    dplyr::group_by(!!!rlang::syms(group_cols), .column) %>%
    dplyr::summarize(.value = mean(.value, na.rm = TRUE))

  if (wider) {
    x <-
      x %>%
      dplyr::ungroup() %>%
      tidyr::pivot_wider(
        id_cols = c(dplyr::one_of(group_cols)),
        names_from = ".column",
        values_from = ".value"
      )
  }
  x
}

prob_summarize <- function(x, p, wider = TRUE) {
  pred_cols <- grep("^\\.pred", names(x), value = TRUE)
  nms <- names(x)
  group_cols <- nms[!(nms %in% pred_cols)]
  x <-
    x %>%
    tidyr::pivot_longer(
      cols = c(dplyr::one_of(pred_cols)),
      names_to = ".column",
      values_to = ".value"
    ) %>%
    dplyr::group_by(!!!rlang::syms(group_cols), .column) %>%
    dplyr::summarize(.value = mean(.value, na.rm = TRUE))

  x <-
    x %>%
    dplyr::full_join(
      x %>% dplyr::summarize(.totals = sum(.value)),
      by = group_cols
    ) %>%
    dplyr::mutate(.value = .value/.totals) %>%
    dplyr::select(-.totals)

  if (wider) {
    x <-
      x %>%
      dplyr::ungroup() %>%
      tidyr::pivot_wider(
        id_cols = c(dplyr::one_of(group_cols)),
        names_from = ".column",
        values_from = ".value"
      )
  }
  x
}

prob_and_class_summarize <- function(x, p) {
  x$.pred_class <- NULL
  x <- prob_summarize(x, p, wider = FALSE)
  nms <- names(x)
  group_cols <- nms[!(nms %in% c(".column", ".value"))]
  outcome_col <- group_cols[!(group_cols %in% c(p, ".row", ".iter"))]
  lvl <- levels(x[[outcome_col]])
  ord <- is.ordered(x[[outcome_col]])
  y <-
    x %>%
    dplyr::arrange(dplyr::desc(.value), .by_group = TRUE) %>%
    dplyr::slice(1) %>%
    dplyr::mutate(
      .value = gsub("\\.pred_", "", .column),
      .value = factor(.value, levels = lvl, ordered = ord),
      .column = ".pred_class") %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(
      id_cols = c(dplyr::one_of(group_cols)),
      names_from = ".column",
      values_from = ".value"
    )
  x <-
    x %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(
      id_cols = c(dplyr::one_of(group_cols)),
      names_from = ".column",
      values_from = ".value"
    ) %>%
    dplyr::full_join(y, by = group_cols)
  x
}

class_summarize <- function(x, p) {
  pred_cols <- grep("^\\.pred", names(x), value = TRUE)
  nms <- names(x)
  group_cols <- nms[!(nms %in% pred_cols)]
  outcome_col <- group_cols[!(group_cols %in% c(p, ".row", ".iter"))]
  lvl <- levels(x[[outcome_col]])
  ord <- is.ordered(x[[outcome_col]])
  x <-
    x %>%
    dplyr::group_by(!!!rlang::syms(group_cols)) %>%
    dplyr::count(.pred_class) %>%
    dplyr::arrange(dplyr::desc(n), .by_group = TRUE) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(-n)
  x
}


average_predictions <- function(x, grid = NULL) {
  metric_types <- metrics_info(attr(x, "metrics"))$type
  param_names <- attr(x, "parameters")$id

  if (!is.null(grid)) {
    grid <- dplyr::select(grid, dplyr::one_of(param_names))
    if (ncol(grid) != length(param_names)) {
      rlang::abort(
        paste0("`grid` should only have columns: ",
               paste0("'", param_names, "'", collapse = ", "))
      )
    }
    x$.predictions <-
      purrr::map(x$.predictions, dplyr::inner_join, grid, by  = param_names)
  }

  x <-
    x %>%
    collect_predictions() %>%
    dplyr::select(-starts_with("id"))

  if (all(metric_types == "numeric")) {
    x <- numeric_summarize(x, param_names)
  } else {
    if (all(metric_types == "prob")) {
      x <- prob_summarize(x, param_names)
    } else {
      if (all(metric_types == "class")) {
        x <- class_summarize(x, param_names)
      } else {
        x <- prob_and_class_summarize(x, param_names)
      }
    }
  }
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

