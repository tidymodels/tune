
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
  if (all(map_lgl(res, inherits, "simpleError"))) {
    res <-
      resamples %>%
      mutate(col = map(splits, ~ NULL)) %>%
      setNames(c(names(resamples), col))
    return(res)
  }

  all_null <- all(map_lgl(res, is.null))

  id_cols <- grep("^id", names(resamples), value = TRUE)
  resamples <- dplyr::arrange(resamples, !!!syms(id_cols))
  pulled_vals <- purrr::map_dfr(res, ~.x[[col]])

  if (nrow(pulled_vals)  == 0) {
    res <-
      resamples %>%
      mutate(col = map(splits, ~ NULL)) %>%
      setNames(c(names(resamples), col))
    return(res)
  }

  if (tidyr_new_interface()) {
    pulled_vals <- tidyr::nest(pulled_vals, data = -starts_with("id"))
    names(pulled_vals)[ncol(pulled_vals)] <- col
  } else {
    pulled_vals <- tidyr::nest(pulled_vals, -starts_with("id"), .key = !!col)
  }

  res <- full_join(resamples, pulled_vals, by = id_cols)
  res <- reup_rs(resamples, res)
  res
}

maybe_repair <- function(x) {
  not_null <- !map_lgl(x, is.null)
  is_tibb <- map_lgl(x, tibble::is_tibble)
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

  x <- map(x, insert_val, y = template)
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
  notes <- map(res, ~ purrr::pluck(.x, ".notes"))
  notes <- map(notes, ensure_tibble)
  resamples$.notes <- notes
  resamples
}

# ------------------------------------------------------------------------------

append_metrics <- function(collection, predictions, workflow, metrics, split) {
  if (inherits(predictions, "try-error")) {
    return(collection)
  }
  tmp_est <- estimate_metrics(predictions, metrics, workflow)
  tmp_est <- cbind(tmp_est, labels(split))
  dplyr::bind_rows(collection, tmp_est)
}

append_predictions <- function(collection, predictions, split, control) {
  if (!control$save_pred) {
    return(NULL)
  }
  if (inherits(predictions, "try-error")) {
    return(collection)
  }
  predictions <- cbind(predictions, labels(split))
  dplyr::bind_rows(collection, predictions)
}

append_extracts <- function(collection, workflow, param, split, ctrl) {
  if (any(names(param) == ".submodels")) {
    param <- param %>% dplyr::select(-.submodels)
  }

  parsnip_fit <- get_wflow_fit(workflow)
  model <- parsnip_fit$fit

  if (has_wflow_recipe(workflow)) {
    mold <- get_wflow_mold(workflow)
    recipe <- mold$blueprint$recipe
  } else {
    recipe <- NULL
  }

  extracts <-
    param %>%
    dplyr::bind_cols(labels(split)) %>%
    mutate(
      .extracts = list(
        extract_details(
          list(recipe = recipe, model = model),
          ctrl$extract
        )
      )
    )

  dplyr::bind_rows(collection, extracts)
}


