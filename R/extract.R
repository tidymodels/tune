
extract_details <- function(object, extractor) {
  if (is.null(extractor)) {
    return(list())
  }
  try(extractor(object), silent = TRUE)
}

# ------------------------------------------------------------------------------

# Grab the new results, make sure that they align row-wise with the rsample
# object and then bind columns
pulley <- function(resamples, res, col) {
  if (all(purrr::map_lgl(res, inherits, "simpleError"))) {
    res <-
      resamples %>%
      mutate(col = purrr::map(splits, ~ NULL)) %>%
      setNames(c(names(resamples), col))
    return(res)
  }

  all_null <- all(purrr::map_lgl(res, is.null))

  id_cols <- grep("^id", names(resamples), value = TRUE)
  resamples <- dplyr::arrange(resamples, !!!syms(id_cols))
  pulled_vals <- purrr::map_dfr(res, ~.x[[col]])

  if (nrow(pulled_vals)  == 0) {
    res <-
      resamples %>%
      mutate(col = purrr::map(splits, ~ NULL)) %>%
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
  template <- x[[good_val]][0,]

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

ensure_tibble <- function(x) {
  if (is.null(x)) {
    res <- tibble(.notes = character(0))
  } else {
    res <- tibble(.notes = x)
  }
  res
}

pull_notes <- function(resamples, res, control) {
  notes <- purrr::map(res, ~ purrr::pluck(.x, ".notes"))
  notes <- purrr::map(notes, ensure_tibble)
  resamples$.notes <- notes
  resamples
}

# ------------------------------------------------------------------------------

append_metrics <- function(collection, predictions, workflow, metrics, split, .config = NULL) {
  if (inherits(predictions, "try-error")) {
    return(collection)
  }
  tmp_est <- estimate_metrics(predictions, metrics, workflow)
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
  predictions <- cbind(predictions, labels(split))
  if (!rlang::is_null(.config)) {
    predictions <- cbind(predictions, .config)
  }
  dplyr::bind_rows(collection, predictions)
}

append_extracts <- function(collection, workflow, param, split, ctrl, .config = NULL) {
  if (any(names(param) == ".submodels")) {
    param <- param %>% dplyr::select(-.submodels)
  }

  extracts <-
    param %>%
    dplyr::bind_cols(labels(split)) %>%
    mutate(
      .extracts = list(
        extract_details(workflow, ctrl$extract)
      )
    )

  if (!rlang::is_null(.config)) {
    extracts <- cbind(extracts, .config)
  }

  dplyr::bind_rows(collection, extracts)
}

#' Convenience functions to extract model or recipe
#'
#' When extracting the fitted results, the workflow is easily accessible. If
#' there is only interest in the recipe or model, these functions can be used
#' as shortcuts
#' @param x A fitted workflow object.
#' @return A fitted model or recipe. If a formula is used instead of a recipe,
#' `extract_recipe()` returns `NULL`.
#' @export
extract_recipe <- function(x) {
  if (has_preprocessor_recipe(x)) {
    recipe <- workflows::pull_workflow_prepped_recipe(x)
  } else {
    recipe <- NULL
  }
  recipe
}

#' @export
#' @rdname extract_recipe
extract_model <- function(x) {
  parsnip_fit <- workflows::pull_workflow_fit(x)
  model <- parsnip_fit$fit
}
