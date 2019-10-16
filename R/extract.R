
extract_details <- function(object, extractor) {
  if (is.null(extractor)) {
    return(list())
  }
  try(extractor(object), silent = TRUE)
}

# ------------------------------------------------------------------------------

# Grab the new results, make sure that they align row-wise with the rsample
# object and then bind columns
pulley <- function(rs, res, col) {
  if (all(map_lgl(res, inherits, "simpleError"))) {
    res <-
      rs %>%
      mutate(col = map(splits, ~ NULL)) %>%
      setNames(c(names(rs), col))
    return(res)
  }

  all_null <- all(map_lgl(res, is.null))

  id_cols <- grep("^id", names(rs), value = TRUE)
  rs <- dplyr::arrange(rs, !!!syms(id_cols))
  pulled_vals <- purrr::map_dfr(res, ~.x[[col]])

  if (nrow(pulled_vals)  == 0) {
    res <-
      rs %>%
      mutate(col = map(splits, ~ NULL)) %>%
      setNames(c(names(rs), col))
    return(res)
  }

  if (tidyr_new_interface()) {
    pulled_vals <- tidyr::nest(pulled_vals, data = -starts_with("id"))
    names(pulled_vals)[ncol(pulled_vals)] <- col
  } else {
    pulled_vals <- tidyr::nest(pulled_vals, -starts_with("id"), .key = !!col)
  }

  res <- full_join(rs, pulled_vals, by = id_cols)
  res <- reup_rs(rs, res)
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


pull_metrics <- function(rs, res, control) {
  out <- pulley(rs, res, ".metrics")
  out$.metrics <- maybe_repair(out$.metrics)
  out
}

pull_extracts <- function(rs, res, control) {
  if (!is.null(control$extract)) {
    rs <- pulley(rs, res, ".extracts")
  }
  rs
}

pull_predictions <- function(rs, res, control) {
  if (control$save_pred) {
    rs <- pulley(rs, res, ".predictions")
    rs$.predictions <- maybe_repair(rs$.predictions)
  }
  rs
}

ensure_tibble <- function(x) {
  if (is.null(x)) {
    res <- tibble(.notes = character(0))
  } else {
    res <- tibble(.notes = x)
  }
  res
}

pull_notes <- function(rs, res, control) {
  notes <- map(res, ~ purrr::pluck(.x, ".notes"))
  notes <- map(notes, ensure_tibble)
  rs$.notes <- notes
  rs
}

# ------------------------------------------------------------------------------

append_metrics <- function(collection, predictions, workflow, perf, split) {
  if (inherits(predictions, "try-error")) {
    return(collection)
  }
  tmp_est <- estimate_perf(predictions, perf, workflow)
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

append_extracts <- function(collection, rec, mod, param, split, ctrl) {
  if (any(names(param) == ".submodels")) {
    param <- param %>% dplyr::select(-.submodels)
  }

  tmp_extr <-
    param %>%
    dplyr::bind_cols(labels(split)) %>%
    mutate(
      .extracts = list(
        extract_details(
          list(recipe = rec, model = mod),
          ctrl$extract
        )
      )
    )
  dplyr::bind_rows(collection, tmp_extr)
}


