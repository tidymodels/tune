
extract_details <- function(object, extractor) {
  if (is.null(extractor)) {
    return(list())
  }
  extractor(object)
}

# ------------------------------------------------------------------------------

# Grab the new results, make sure that they align row-wise with the rsample
# object and then bind columns
pulley <- function(resamples, res, col) {
  if (all(purrr::map_lgl(res, inherits, "simpleError"))) {
    res <-
      resamples %>%
      mutate(col = purrr::map(splits, ~NULL)) %>%
      setNames(c(names(resamples), col))
    return(res)
  }

  all_null <- all(purrr::map_lgl(res, is.null))

  id_cols <- grep("^id", names(resamples), value = TRUE)
  resamples <- dplyr::arrange(resamples, !!!syms(id_cols))
  pulled_vals <- try(purrr::map_dfr(res, ~ .x[[col]]), silent = TRUE)

  if (inherits(pulled_vals, "try-error") || nrow(pulled_vals) == 0) {
    res <-
      resamples %>%
      mutate(col = purrr::map(splits, ~NULL)) %>%
      setNames(c(names(resamples), col))
    return(res)
  }

  if (tidyr_new_interface()) {
    pulled_vals <- tidyr::nest(pulled_vals, data = -starts_with("id"))
    names(pulled_vals)[ncol(pulled_vals)] <- col
  } else {
    pulled_vals <- tidyr::nest(pulled_vals, -starts_with("id"), .key = !!col)
  }

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


pull_metrics <- function(resamples, res, control) {
  out <- pulley(resamples, res, ".metrics")
  out$.metrics <- maybe_repair(out$.metrics)
  out
}

pull_extracts <- function(resamples, res, control) {
  if (!is.null(control$extract)) {
    resamples <- pulley(resamples, res, ".extracts")
  }
  resamples
}

pull_predictions <- function(resamples, res, control) {
  if (control$save_pred) {
    resamples <- pulley(resamples, res, ".predictions")
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
  all_outcome_names <- rlang::flatten(all_outcome_names)
  all_outcome_names <- vctrs::vec_unique(all_outcome_names)

  n_unique <- length(all_outcome_names)

  # All models failed
  if (n_unique == 0L) {
    return(character())
  }

  if (n_unique > 1L) {
    rlang::warn(paste0(
      "More than one set of outcomes were used when tuning. ",
      "This should never happen. ",
      "Review how the outcome is specified in your model."
    ))
  }

  outcome_names <- all_outcome_names[[1L]]

  outcome_names
}

ensure_tibble <- function(x) {
  if (is.null(x)) {
    res <- tibble::tibble(.notes = character(0))
  } else {
    res <- tibble::tibble(.notes = x)
  }
  res
}

pull_notes <- function(resamples, res, control) {
  resamples$.notes <- purrr::map(res, ~ purrr::pluck(.x, ".notes"))
  resamples
}

# ------------------------------------------------------------------------------

append_metrics <- function(collection,
                           predictions,
                           metrics,
                           param_names,
                           outcome_name,
                           event_level,
                           split,
                           .config = NULL) {
  if (inherits(predictions, "try-error")) {
    return(collection)
  }

  tmp_est <- .estimate_metrics(
    dat = predictions,
    metric = metrics,
    param_names = param_names,
    outcome_name = outcome_name,
    event_level = event_level
  )

  tmp_est <- cbind(tmp_est, labels(split))

  if (!rlang::is_null(.config)) {
    tmp_est <- cbind(tmp_est, .config)
  }

  dplyr::bind_rows(collection, tmp_est)
}

append_predictions <- function(collection, predictions, split, control, .config = NULL) {
  if (!control$save_pred) {
    return(NULL)
  }
  if (inherits(predictions, "try-error")) {
    return(collection)
  }

  predictions <- vec_cbind(predictions, labels(split))

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

append_extracts <- function(collection, workflow, grid, split, ctrl, .config = NULL) {
  extracts <- dplyr::bind_cols(grid, labels(split))
  extracts$.extracts <- list(extract_details(workflow, ctrl$extract))

  if (!rlang::is_null(.config)) {
    extracts <- cbind(extracts, .config)
  }

  dplyr::bind_rows(collection, extracts)
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
  lifecycle::deprecate_soft(
    "0.1.6",
    "extract_model()",
    "extract_fit_engine()"
  )
  parsnip_fit <- extract_fit_parsnip(x)
  model <- parsnip_fit$fit
  model
}
