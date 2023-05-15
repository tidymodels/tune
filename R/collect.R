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
#' @param ... Not currently used.
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
#'
#' [collect_notes()] returns a tibble with columns for the resampling
#' indicators, the location (preprocessor, model, etc.), type (error or warning),
#' and the notes.
#'
#' [collect_extracts()] returns a tibble with columns for the resampling
#' indicators, the location (preprocessor, model, etc.), and objects extracted
#' from workflows via the `extract` argument to [control functions][control_grid()].
#' @examplesIf tune:::should_run_examples(suggests = "kknn")
#' data("example_ames_knn")
#' # The parameters for the model:
#' extract_parameter_set_dials(ames_wflow)
#'
#' # Summarized over resamples
#' collect_metrics(ames_grid_search)
#'
#' # Per-resample values
#' collect_metrics(ames_grid_search, summarize = FALSE)
#'
#'
#' # ---------------------------------------------------------------------------
#'
#' library(parsnip)
#' library(rsample)
#' library(dplyr)
#' library(recipes)
#' library(tibble)
#'
#' lm_mod <- linear_reg() %>% set_engine("lm")
#' set.seed(93599150)
#' car_folds <- vfold_cv(mtcars, v = 2, repeats = 3)
#' ctrl <- control_resamples(save_pred = TRUE, extract = extract_fit_engine)
#'
#' spline_rec <-
#'   recipe(mpg ~ ., data = mtcars) %>%
#'   step_ns(disp, deg_free = tune("df"))
#'
#' grid <- tibble(df = 3:6)
#'
#' resampled <-
#'   lm_mod %>%
#'   tune_grid(spline_rec, resamples = car_folds, control = ctrl, grid = grid)
#'
#' collect_predictions(resampled) %>% arrange(.row)
#' collect_predictions(resampled, summarize = TRUE) %>% arrange(.row)
#' collect_predictions(resampled, summarize = TRUE, grid[1, ]) %>% arrange(.row)
#'
#' collect_extracts(resampled)
#'
#' @export
collect_predictions <- function(x, ...) {
  UseMethod("collect_predictions")
}

#' @export
#' @rdname collect_predictions
collect_predictions.default <- function(x, ...) {
  rlang::abort("No `collect_predictions()` exists for this type of object.")
}

#' @export
#' @rdname collect_predictions
collect_predictions.tune_results <- function(x, summarize = FALSE, parameters = NULL, ...) {
  if (!inherits(x, "tune_results")) {
    rlang::abort(
      paste0(
        "`x` should be an object produced by one of the `tune_*()` functions,",
        "`fit_resamples()` or `last_fit()`."
      )
    )
  }

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
  params <- attr(x, "parameters")
  if (is.null(params)) {
    rlang::warn(
      paste(
        strwrap(
          paste(
            "The object is missing some attributes; it is probably from",
            "an earlier version of `tune`. The predictions can't be filtered."
          ),
          prefix = ""
        ),
        collapse = "\n"
      )
    )

    return(x)
  }

  param_names <- params$id
  parameters <- dplyr::select(parameters, dplyr::any_of(param_names))
  if (ncol(parameters) != length(param_names)) {
    rlang::abort(
      paste0(
        "`parameters` should only have columns: ",
        paste0("'", param_names, "'", collapse = ", ")
      )
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

numeric_summarize <- function(x) {
  pred_cols <- grep("^\\.pred", names(x), value = TRUE)
  nms <- names(x)
  group_cols <- nms[!(nms %in% pred_cols)]
  x <-
    x %>%
    dplyr::group_by(!!!rlang::syms(group_cols)) %>%
    dplyr::summarise(
      dplyr::across(dplyr::starts_with(".pred"), mean_na_rm)
    )

  x
}

prob_summarize <- function(x, p) {
  pred_cols <- grep("^\\.pred", names(x), value = TRUE)

  if (any(names(x) == ".pred_class")) {
    compute_class <- TRUE
    x$.pred_class <- NULL
    pred_cols <- pred_cols[pred_cols != ".pred_class"]
  } else {
    compute_class <- FALSE
  }

  nms <- names(x)
  y_cols <- nms[!(nms %in% c(".row", ".iter", ".config", pred_cols, p))]
  group_cols <- nms[!(nms %in% pred_cols)]

  x <-
    x %>%
    dplyr::group_by(!!!rlang::syms(group_cols)) %>%
    dplyr::summarise(
      dplyr::across(dplyr::starts_with(".pred_"), mean_na_rm)
    ) %>%
    ungroup()

  # In case the class probabilities do not add up to 1 after averaging
  group_cols <- group_cols[group_cols != y_cols]
  totals <-
    x %>%
    dplyr::select(-dplyr::all_of(y_cols)) %>%
    tidyr::pivot_longer(
      cols = c(dplyr::all_of(pred_cols)),
      names_to = ".column",
      values_to = ".value"
    ) %>%
    dplyr::group_by(!!!rlang::syms(group_cols)) %>%
    dplyr::summarize(.totals = sum(.value)) %>%
    dplyr::ungroup()

  x <-
    x %>%
    dplyr::full_join(totals, by = group_cols) %>%
    dplyr::mutate(
      dplyr::across(dplyr::starts_with(".pred_"),
      ~ .x / .totals
    )) %>%
    dplyr::select(-.totals)

  # If we started with hard class predictions, recompute them based on the
  # newly averaged probability estimates.
  if (compute_class) {
    lvl <- levels(x[[y_cols]])
    ord <- is.ordered(x[[y_cols]])
    class_pred <-
      x %>%
      dplyr::select(-dplyr::all_of(y_cols)) %>%
      tidyr::pivot_longer(
        cols = c(dplyr::all_of(pred_cols)),
        names_to = ".column",
        values_to = ".value"
      ) %>%
      dplyr::group_by(!!!rlang::syms(group_cols)) %>%
      dplyr::arrange(dplyr::desc(.value), .by_group = TRUE) %>%
      dplyr::slice(1) %>%
      dplyr::mutate(
        .pred_class = gsub("\\.pred_", "", .column),
        .pred_class = factor(.pred_class, levels = lvl, ordered = ord)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(-.value, -.column)
    x <- full_join(x, class_pred, by = group_cols)
  }
  x
}

# define a `mean()` wrapper to avoid slowdown in
# evaluation of anonymous function
mean_na_rm <- function(x) {
  mean(x, na.rm = TRUE)
}

class_summarize <- function(x, p) {
  pred_cols <- grep("^\\.pred", names(x), value = TRUE)
  nms <- names(x)
  group_cols <- nms[!(nms %in% pred_cols)]
  outcome_col <- group_cols[!(group_cols %in% c(p, ".row", ".iter", ".config"))]
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
    grid <- dplyr::select(grid, dplyr::all_of(param_names))
    if (ncol(grid) != length(param_names)) {
      rlang::abort(
        paste0(
          "`grid` should only have columns: ",
          paste0("'", param_names, "'", collapse = ", ")
        )
      )
    }
    x$.predictions <-
      purrr::map(x$.predictions, dplyr::inner_join, grid, by = param_names)
  }

  x <-
    x %>%
    collect_predictions() %>%
    dplyr::select(-starts_with("id"))

  if (all(metric_types == "numeric")) {
    x <- numeric_summarize(x)
  } else if (any(metric_types == "prob")) {
    x <- prob_summarize(x, param_names)
  } else {
    x <- class_summarize(x, param_names)
  }

  if (dplyr::is_grouped_df(x)) {
    x <- dplyr::ungroup(x)
  }
  x
}


# ------------------------------------------------------------------------------

#' @export
#' @rdname collect_predictions
collect_metrics <- function(x, ...) {
  UseMethod("collect_metrics")
}

#' @export
collect_metrics.default <- function(x, ...) {
  rlang::abort("No `collect_metric()` exists for this type of object.")
}

#' @export
#' @rdname collect_predictions
collect_metrics.tune_results <- function(x, summarize = TRUE, ...) {
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
  is_iterative <- any(colnames(x) == ".iter")
  if (is_iterative) {
    keep_cols <- c(coll_col, ".iter")
  } else {
    keep_cols <- coll_col
  }

  id_cols <- colnames(x)[grepl("id", colnames(x))]
  keep_cols <- c(id_cols, keep_cols)
  x <- x[keep_cols]
  coll_col <- x[[coll_col]]
  sizes <- list_sizes(coll_col)

  res <-
    vec_cbind(
      vec_rep_each(x[, id_cols], times = sizes),
      list_unchop(coll_col)
    )

  if (is_iterative) {
    res <-
      vec_cbind(
        res,
        vec_rep_each(x[, ".iter"], times = sizes)
      )
  }

  arrange_cols <- c(".iter", ".config")
  arrange_cols <- arrange_cols[rlang::has_name(res, arrange_cols)]

  vec_slice(res, vec_order(res[arrange_cols]))
}

#' @export
#' @keywords internal
#' @rdname empty_ellipses
# Get relationship between the parameter values, .config, and (potentially) .iter
.config_key_from_metrics <- function(x) {
  param_names <- .get_tune_parameter_names(x)
  tibble_metrics <- purrr::map_lgl(x[[".metrics"]], tibble::is_tibble)
  x <- vec_slice(x, tibble_metrics)
  x <- x[, colnames(x) %in% c(".iter", ".metrics")]

  metrics <- x[[".metrics"]]

  out <- list_unchop(metrics)
  out <- out[c(param_names, ".config")]

  if (rlang::has_name(x, ".iter")) {
    iter <- x[[".iter"]]
    out[[".iter"]] <- vec_rep_each(iter, times = list_sizes(metrics))
  }

  out <- vec_unique(out)

  out
}

#' @export
#' @keywords internal
#' @rdname empty_ellipses
estimate_tune_results <- function(x, col_name = ".metrics", ...) {
  param_names <- .get_tune_parameter_names(x)
  id_names <- grep("^id", names(x), value = TRUE)

  all_bad <- is_cataclysmic(x)
  if (all_bad) {
    rlang::abort("All models failed. Run `show_notes(.Last.tune.result)` for more information.")
  }

  # The mapping of tuning parameters and .config.
  config_key <- .config_key_from_metrics(x)

  tibble_metrics <- purrr::map_lgl(x[[col_name]], tibble::is_tibble)
  x <- x[tibble_metrics, c(id_names, col_name)]
  x <- tidyr::unnest(x, cols = c(all_of(col_name)))

  if (col_name == ".extracts") {
    # For sub-model parameters, there are parameter columns in the current tibble
    # but the list column of tibbles may also have some of those parameter columns
    # too (and these are more accurate). We will see if there is overlap and
    # delete the top-level columns.
    outer_names <- names(x)
    inner_names <- names(x$.extracts[[1]])
    conflicts <- intersect(outer_names, inner_names)
    conflicts <- intersect(conflicts, param_names)
    if (length(conflicts) > 0) {
      x <- dplyr::select(x, -dplyr::all_of(conflicts))
    }
    # Now bring the extract values to the top
    x <- tidyr::unnest(x, cols = .extracts)
    # There may _still_ be list columns and, if there are, un-nest these.
    is_list_col <- purrr::map_lgl(x, is.list)
    if (any(is_list_col)) {
      list_cols <- names(is_list_col)[is_list_col]
      x <- tidyr::unnest(x, cols = c(dplyr::all_of(list_cols)))
    }
    # Again, for models with sub-model parameters, the current .config may not
    # be accurate so we remove it and merge back in later.
    x$.config <- NULL
    if (any(names(x) == ".iter")) {
      x$.iter <- NULL
    }

    x <- dplyr::distinct(x)
  }
  group_cols <- c(".metric", ".estimator")
  if (".by" %in% names(x)) {
    group_cols <- c(group_cols, ".by")
  }
  x <- x %>%
    tibble::as_tibble() %>%
    dplyr::group_by(!!!rlang::syms(param_names), !!!rlang::syms(group_cols)) %>%
    dplyr::summarize(
      mean = mean(.estimate, na.rm = TRUE),
      n = sum(!is.na(.estimate)),
      std_err = sd(.estimate, na.rm = TRUE) / sqrt(n),
      .groups = "drop"
    )

  # only join when parameters are being tuned (#600)
  if (length(param_names) == 0) {
    x <- x %>%
      dplyr::bind_cols(config_key)
  } else {
    x <- x %>%
      dplyr::full_join(config_key, by = param_names)
  }

  arrange_names <- intersect(c(".iter", ".config"), names(x))
  dplyr::arrange(x, !!!rlang::syms(arrange_names))
}

# ------------------------------------------------------------------------------

#' @export
#' @rdname collect_predictions
collect_notes <- function(x, ...) {
  UseMethod("collect_notes")
}

#' @export
collect_notes.default <- function(x, ...) {
  rlang::abort("No `collect_notes()` exists for this type of object.")
}

#' @export
#' @rdname collect_predictions
collect_notes.tune_results <- function(x, ...) {
  if (inherits(x, "last_fit")) {
    return(x$.notes[[1]])
  }

  x %>%
    dplyr::select(dplyr::starts_with("id"), dplyr::any_of(".iter"), .notes) %>%
    tidyr::unnest(cols = .notes)
}

# ----------------------------------------------------------------------------

#' @export
#' @rdname collect_predictions
collect_extracts <- function(x, ...) {
  UseMethod("collect_extracts")
}

#' @export
collect_extracts.default <- function(x, ...) {
  rlang::abort("No `collect_extracts()` exists for this type of object.")
}

#' @export
#' @rdname collect_predictions
collect_extracts.tune_results <- function(x, ...) {
  if (!".extracts" %in% colnames(x)) {
    cli::cli_abort(c(
      "!" = "The {.var .extracts} column does not exist.",
      "i" = "Please supply a {.help [control object](tune::control_grid)} with \\
             a non-{.var NULL} {.arg extract} argument during resample fitting."
    ))
  }

  x %>%
    dplyr::select(dplyr::starts_with("id"), dplyr::any_of(".iter"), .extracts) %>%
    tidyr::unnest(cols = .extracts)
}

