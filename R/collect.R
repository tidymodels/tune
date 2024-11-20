#' Obtain and format results produced by tuning functions
#'
#' @param x The results of [tune_grid()], [tune_bayes()], [fit_resamples()],
#'  or [last_fit()]. For [collect_predictions()], the control option `save_pred
#'  = TRUE` should have been used.
#' @param ... Not currently used.
#' @param summarize A logical; should metrics be summarized over resamples
#' (`TRUE`) or return the values for each individual resample. Note that, if `x`
#' is created by [last_fit()], `summarize` has no effect. For the other object
#' types, the method of summarizing predictions is detailed below.
#' @param parameters An optional tibble of tuning parameter values that can be
#'  used to filter the predicted values before processing. This tibble should
#'  only have columns for each tuning parameter identifier (e.g. `"my_param"`
#'  if `tune("my_param")` was used).
#' @param type One of `"long"` (the default) or `"wide"`. When `type = "long"`,
#'  output has columns `.metric` and one of `.estimate` or `mean`.
#'  `.estimate`/`mean` gives the values for the `.metric`. When `type = "wide"`,
#'  each metric has its own column and the `n` and `std_err` columns are removed,
#'  if they exist.
#'
#' @return A tibble. The column names depend on the results and the mode of the
#' model.
#'
#' For [collect_metrics()] and [collect_predictions()], when unsummarized,
#' there are columns for each tuning parameter (using the `id` from [tune()],
#' if any).
#'
#' [collect_metrics()] also has columns `.metric`, and `.estimator` by default.
#' For [collect_metrics()] methods that have a `type` argument, supplying
#' `type = "wide"` will pivot the output such that each metric has its own
#' column. When the results are summarized, there are columns for `mean`, `n`,
#' and `std_err`. When not summarized, the additional columns for the resampling
#' identifier(s) and `.estimate`.
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
#' For regression cases, the numeric predictions are simply averaged.
#'
#' For classification models, the problem is more complex. When class probabilities
#'  are used, these are averaged and then re-normalized to make sure that they
#'  add to one. If hard class predictions also exist in the data, then these are
#'  determined from the summarized probability estimates (so that they match).
#'  If only hard class predictions are in the results, then the mode is used to
#'  summarize.
#'
#' With censored outcome models, the predicted survival probabilities (if any)
#'  are averaged while the static predicted event times are summarized using the
#'  median.
#'
#' [collect_notes()] returns a tibble with columns for the resampling
#' indicators, the location (preprocessor, model, etc.), type (error or warning),
#' and the notes.
#'
#' [collect_extracts()] collects objects extracted from fitted workflows
#' via the `extract` argument to [control functions][control_grid()]. The
#' function returns a tibble with columns for the resampling
#' indicators, the location (preprocessor, model, etc.), and extracted objects.
#'
#' @section Hyperparameters and extracted objects:
#'
#' When making use of submodels, tune can generate predictions and calculate
#' metrics for multiple model `.config`urations using only one model fit.
#' However, this means that if a function was supplied to a
#' [control function's][control_grid()] `extract` argument, tune can only
#' execute that extraction on the one model that was fitted. As a result,
#' in the `collect_extracts()` output, tune opts to associate the
#' extracted objects with the hyperparameter combination used to
#' fit that one model workflow, rather than the hyperparameter
#' combination of a submodel. In the output, this appears like
#' a hyperparameter entry is recycled across many `.config`
#' entries---this is intentional.
#'
#' See \url{https://parsnip.tidymodels.org/articles/Submodels.html} to learn
#' more about submodels.
#'
#' @examplesIf tune:::should_run_examples(suggests = c("kknn", "splines2"))
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
#'   step_spline_natural(disp, deg_free = tune("df"))
#'
#' grid <- tibble(df = 3:6)
#'
#' resampled <-
#'   lm_mod %>%
#'   tune_grid(spline_rec, resamples = car_folds, control = ctrl, grid = grid)
#'
#' collect_predictions(resampled) %>% arrange(.row)
#' collect_predictions(resampled, summarize = TRUE) %>% arrange(.row)
#' collect_predictions(
#'   resampled,
#'   summarize = TRUE,
#'   parameters = grid[1, ]
#' ) %>% arrange(.row)
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
  cli::cli_abort(
    "No {.fn collect_predictions} exists for {.obj_type_friendly {x}}."
  )
}

#' @export
#' @rdname collect_predictions
collect_predictions.tune_results <- function(x, ..., summarize = FALSE, parameters = NULL) {
  rlang::check_dots_empty()

  names <- colnames(x)
  coll_col <- ".predictions"

  has_coll_col <- coll_col %in% names

  if (!has_coll_col) {
    cli::cli_abort(
      "The {.field .predictions} column does not exist. Please refit with the
      control argument {.code save_pred = TRUE} to save predictions."
    )
  }

  x <- filter_predictions(x, parameters)

  if (summarize) {
    x <- average_predictions(x, parameters)
  } else {
    x <- collector(x, coll_col = coll_col)
  }

  x <- dplyr::relocate(
    x,
    dplyr::any_of(".pred"), dplyr::any_of(".pred_class"),
    dplyr::any_of(".pred_time"), dplyr::starts_with(".pred")
  )

  x
}

filter_predictions <- function(x, parameters) {
  if (is.null(parameters)) {
    return(x)
  }
  params <- attr(x, "parameters")
  if (is.null(params)) {
    cli::cli_warn(
      "The object is missing some attributes; it is probably from an earlier 
       version of {.pkg tune}. The predictions can't be filtered."
    )

    return(x)
  }

  param_names <- params$id
  parameters <- dplyr::select(parameters, dplyr::any_of(param_names))
  if (ncol(parameters) != length(param_names)) {
    cli::cli_abort("{.arg parameters} should only have columns:
                    {.val {param_names}}.")
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
  y_cols <- nms[!(nms %in% c(".row", ".iter", ".config", ".eval_time", pred_cols, p))]
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

surv_summarize <- function(x, param, y) {
  pred_cols <- grep("^\\.pred", names(x), value = TRUE)
  nms <- names(x)

  outcomes <- x[, c(".row", y)] %>% dplyr::slice(1, .by = .row)

  res <- NULL

  # Use the median to summarize the static estimates
  if (any(pred_cols == ".pred_time")) {
    res <-
      dplyr::summarize(
        x,
        .pred_time = median(.pred_time),
        .by = c(.row, .config, dplyr::any_of(param), dplyr::any_of(".iter"))
      )
  }

  # Simple mean to summarize dynamic probability predictions
  if (any(pred_cols == ".pred")) {
    nest_cols <- c(".eval_time", ".pred_survival", ".weight_censored")
    tmp <-
      x %>%
      dplyr::select(.pred,
                    .config,
                    .row,
                    dplyr::any_of(param),
                    dplyr::any_of(".iter")) %>%
      tidyr::unnest(.pred) %>%
      dplyr::summarize(
        .pred_survival = mean(.pred_survival, na.rm = TRUE),
        .weight_censored = mean(.weight_censored, na.rm = TRUE),
        .by = c(
          .row,
          .eval_time,
          .config,
          dplyr::any_of(param),
          dplyr::any_of(".iter")
        )
      ) %>%
      tidyr::nest(
        .pred = c(dplyr::all_of(nest_cols)),
        .by = c(.row, .config,
                dplyr::any_of(param),
                dplyr::any_of(".iter"))
      )

    if (!is.null(res)) {
      dot_iter <- grep("\\.iter", nms, value = TRUE)
      res <-
        dplyr::full_join(tmp, res, by = c(".row", ".config", param, dot_iter))
    } else {
      res <- tmp
    }
  }

  res <- dplyr::full_join(outcomes, res, by = ".row")
  res[order(res$.row, res$.config), nms]
}

average_predictions <- function(x, grid = NULL) {
  metric_types <- metrics_info(attr(x, "metrics"))$type
  param_names <- attr(x, "parameters")$id
  y_nms <- .get_tune_outcome_names(x)

  if (!is.null(grid)) {
    grid <- dplyr::select(grid, dplyr::all_of(param_names))
    if (ncol(grid) != length(param_names)) {
      cli::cli_abort("{.arg grid} should only have columns: {.val {param_names}}.")
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
    # Note that this will recompute the hard class predictions since the
    # probability estimates are changing. That's why there is a separate
    # branch below that summarizes the hard class predictions when those are
    # the only predictions.
    x <- prob_summarize(x, param_names)
  } else if (any(metric_types == "class")) {
    x <- class_summarize(x, param_names)
  } else if (any(metric_types %in% c("survival", "time"))) {
    x <- surv_summarize(x, param_names, y_nms)
  } else {
    cli::cli_abort(
      "We don't know about metrics of type: {.val {unique(metric_types)}}."
    )
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
  cli::cli_abort(
    "No {.fn collect_metrics} exists for {.obj_type_friendly {x}}."
  )
}

#' @export
#' @rdname collect_predictions
collect_metrics.tune_results <- function(x, ..., summarize = TRUE, type = c("long", "wide")) {
  rlang::check_dots_empty()
  rlang::arg_match0(type, values = c("long", "wide"))

  if (inherits(x, "last_fit")) {
    res <- x$.metrics[[1]]
  } else {
    if (summarize) {
      res <- estimate_tune_results(x)
    } else {
      res <- collector(x, coll_col = ".metrics")
    }
  }

  if (identical(type, "wide")) {
    res <- pivot_metrics(x, res)
  }

  res
}

pivot_metrics <- function(x, x_metrics) {
  params <- .get_tune_parameter_names(x)
  res <- paste_param_by(x_metrics)

  tidyr::pivot_wider(
    res,
    id_cols = c(
      dplyr::any_of(c(params, ".config", ".iter", ".eval_time")),
      starts_with("id")
    ),
    names_from = .metric,
    values_from = dplyr::any_of(c(".estimate", "mean"))
  )
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
  sizes <- vctrs::list_sizes(coll_col)

  res <-
    vctrs::vec_cbind(
      vctrs::vec_rep_each(x[, id_cols], times = sizes),
      vctrs::list_unchop(coll_col)
    )

  if (is_iterative) {
    res <-
      vctrs::vec_cbind(
        res,
        vctrs::vec_rep_each(x[, ".iter"], times = sizes)
      )
  }

  arrange_cols <- c(".eval_time", ".iter", ".config")
  arrange_cols <- arrange_cols[rlang::has_name(res, arrange_cols)]

  vec_slice(res, vctrs::vec_order(res[arrange_cols]))
}

#' @export
#' @keywords internal
#' @rdname empty_ellipses
# Get relationship between the parameter values, .config, and (potentially) .iter
.config_key_from_metrics <- function(x) {
  param_names <- .get_tune_parameter_names(x)
  tibble_metrics <- purrr::map_lgl(x[[".metrics"]], tibble::is_tibble)
  x <- vctrs::vec_slice(x, tibble_metrics)
  x <- x[, colnames(x) %in% c(".iter", ".metrics")]

  metrics <- x[[".metrics"]]

  out <- vctrs::list_unchop(metrics)
  out <- out[c(param_names, ".config")]

  if (rlang::has_name(x, ".iter")) {
    iter <- x[[".iter"]]
    out[[".iter"]] <- vctrs::vec_rep_each(iter, times = vctrs::list_sizes(metrics))
  }

  out <- vctrs::vec_unique(out)

  out
}

#' @export
#' @keywords internal
#' @rdname empty_ellipses
estimate_tune_results <- function(x, ..., col_name = ".metrics") {
  rlang::check_dots_empty()
  param_names <- .get_tune_parameter_names(x)
  id_names <- grep("^id", names(x), value = TRUE)
  group_cols <- .get_extra_col_names(x)

  all_bad <- is_cataclysmic(x)
  if (all_bad) {
    cli::cli_abort(
      "All models failed. Run {.code show_notes(.Last.tune.result)} for more
       information."
    )
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

  x <- x %>%
    tibble::as_tibble() %>%
    vctrs::vec_slice(., .$id != "Apparent") %>%
    dplyr::group_by(!!!rlang::syms(param_names), .metric, .estimator,
                    !!!rlang::syms(group_cols)) %>%
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

  arrange_names <- intersect(c(".iter", ".config", ".eval_time"), names(x))
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
  cli::cli_abort(
    "No {.fn collect_notes} exists for {.obj_type_friendly {x}}."
  )
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
  cli::cli_abort(
    "No {.fn collect_extracts} exists for {.obj_type_friendly {x}}."
  )
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

