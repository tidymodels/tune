
extract_details <- function(object, extractor) {
  if (is.null(extractor)) {
    return(list())
  }
  extractor(object)
}

# ------------------------------------------------------------------------------

# Grab the new results, make sure that they align row-wise with the rsample
# object and then bind columns
pulley <- function(resamples, res, col, order) {
  if (all(purrr::map_lgl(res, inherits, "simpleError"))) {
    res <-
      resamples %>%
      mutate(col = purrr::map(splits, ~NULL)) %>%
      setNames(c(names(resamples), col))
    return(res)
  }

  all_null <- all(purrr::map_lgl(res, is.null))

  id_cols <- grep("^id", names(resamples), value = TRUE)

  resamples <- vctrs::vec_slice(resamples, order)

  pulled_vals <- purrr::map(res, ~ .x[[col]]) %>% purrr::list_rbind()

  if (nrow(pulled_vals) == 0) {
    res <-
      resamples %>%
      mutate(col = purrr::map(splits, ~NULL)) %>%
      setNames(c(names(resamples), col))
    return(res)
  }

  pulled_vals <- tidyr::nest(pulled_vals, data = -starts_with("id"))
  names(pulled_vals)[ncol(pulled_vals)] <- col

  res <- new_bare_tibble(resamples)
  res <- full_join(res, pulled_vals, by = id_cols)
  res <- reup_rs(resamples, res)
  res
}

maybe_repair <- function(x) {
  not_null <- !purrr::map_lgl(x, is.null)
  is_tibb <- purrr::map_lgl(x, tibble::is_tibble)
  ok <- not_null & is_tibb
  if (!any(ok)) {
    return(x)
  }

  good_val <- which(ok)[1]
  template <- x[[good_val]][0, ]

  insert_val <- function(x, y) {
    if (is.null(x)) {
      x <- y
    }
    x
  }

  x <- purrr::map(x, insert_val, y = template)
  x
}


pull_metrics <- function(resamples, res, control, order) {
  out <- pulley(resamples, res, ".metrics", order = order)
  out$.metrics <- maybe_repair(out$.metrics)
  out
}

pull_extracts <- function(resamples, res, control, order) {
  if (!is.null(control$extract)) {
    resamples <- pulley(resamples, res, ".extracts", order = order)
  }
  resamples
}

pull_predictions <- function(resamples, res, control, order) {
  if (control$save_pred) {
    resamples <- pulley(resamples, res, ".predictions", order = order)
    resamples$.predictions <- maybe_repair(resamples$.predictions)
  }
  resamples
}

pull_all_outcome_names <- function(resamples, res) {
  all_outcome_names <- purrr::map(res, ~ .x[[".all_outcome_names"]])
  resamples$.all_outcome_names <- all_outcome_names
  resamples
}

reduce_all_outcome_names <- function(resamples) {
  all_outcome_names <- resamples$.all_outcome_names
  all_outcome_names <- purrr::list_flatten(all_outcome_names)
  all_outcome_names <- vctrs::vec_unique(all_outcome_names)

  n_unique <- length(all_outcome_names)

  # All models failed
  if (n_unique == 0L) {
    return(character())
  }

  if (n_unique > 1L) {
    cli::cli_warn(
      "More than one set of outcomes were used when tuning. This should never
       happen. Please review how the outcome is specified in your model."
    )
  }

  outcome_names <- all_outcome_names[[1L]]

  outcome_names
}

ensure_tibble <- function(x) {
  if (is.null(x)) {
    res <- tibble::new_tibble(list(.notes = character(0)), nrow = 0)
  } else {
    res <- tibble::new_tibble(list(.notes = x), nrow = length(x))
  }
  res
}

pull_notes <- function(resamples, res, control, order) {
  notes <- purrr::map(res, ~ purrr::pluck(.x, ".notes"))
  resamples$.notes <- notes[order]

  resamples
}

# ------------------------------------------------------------------------------

append_metrics <- function(collection,
                           predictions,
                           metrics,
                           param_names,
                           outcome_name,
                           event_level,
                           split_labels,
                           .config = NULL,
                           metrics_info) {
  if (inherits(predictions, "try-error")) {
    return(collection)
  }

  tmp_est <- .estimate_metrics(
    dat = predictions,
    metric = metrics,
    param_names = param_names,
    outcome_name = outcome_name,
    event_level = event_level,
    metrics_info = metrics_info
  )

  tmp_est <- cbind(tmp_est, split_labels)

  if (!rlang::is_null(.config)) {
    tmp_est <- cbind(tmp_est, .config)
  }

  dplyr::bind_rows(collection, tmp_est)
}

append_predictions <- function(collection, predictions, split_labels, control, .config = NULL) {
  if (!control$save_pred) {
    return(NULL)
  }
  if (inherits(predictions, "try-error")) {
    return(collection)
  }

  predictions <- vec_cbind(predictions, split_labels)

  if (!rlang::is_null(.config)) {
    by <- setdiff(names(.config), ".config")

    if (length(by) == 0L) {
      # Nothing to tune, just bind on config
      predictions <- vec_cbind(predictions, .config)
    } else {
      predictions <- dplyr::inner_join(predictions, .config, by = by)
    }
  }

  dplyr::bind_rows(collection, predictions)
}

append_extracts <- function(collection, extracts) {
  dplyr::bind_rows(collection, extracts)
}

make_extracts <- function(extract, grid, split_labels, .config = NULL) {
  extracts <- dplyr::bind_cols(grid, split_labels)
  extracts$.extracts <- list(extract)

  if (!rlang::is_null(.config)) {
    extracts <- cbind(extracts, .config)
  }

  extracts
}

append_outcome_names <- function(all_outcome_names, outcome_names) {
  c(all_outcome_names, list(outcome_names))
}

extract_metrics_config <- function(param_names, metrics) {
  metrics_config_names <- c(param_names, ".config")
  out <- metrics[metrics_config_names]
  vec_unique(out)
}

#' Convenience functions to extract model
#'
#' `r lifecycle::badge("soft-deprecated")`
#'
#' Use [`extract_fit_engine()`][extract_fit_engine.tune_results()] instead of `extract_model()`.
#'
#' When extracting the fitted results, the workflow is easily accessible. If
#' there is only interest in the model, this functions can be used
#' as a shortcut
#' @param x A fitted workflow object.
#' @return A fitted model.
#' @export
extract_model <- function(x) {
  lifecycle::deprecate_warn(
    "0.1.6",
    "extract_model()",
    "extract_fit_engine()"
  )
  parsnip_fit <- extract_fit_parsnip(x)
  model <- parsnip_fit$fit
  model
}
