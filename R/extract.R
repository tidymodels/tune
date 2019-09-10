
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
  id_cols <- grep("^id", names(rs), value = TRUE)
  rs <- dplyr::arrange(rs, !!!syms(id_cols))
  pulled_vals <- purrr::map_dfr(res, ~.x[[col]])

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

pull_metrics <- function(rs, res) {
  pulley(rs, res, ".metrics")
}

pull_extracts <- function(rs, res, control) {
  if (!is.null(control$extract)) {
    rs <- pulley(rs, res, ".extract")
  }
  rs
}

pull_predictions <- function(rs, res, control) {
  if (control$save_pred) {
    rs <- pulley(rs, res, ".predictions")
  }
  rs
}

# ------------------------------------------------------------------------------

append_metrics <- function(collection, predictions, workflow, perf, split) {
  tmp_est <- estimate_perf(predictions, perf, workflow)
  tmp_est <- cbind(tmp_est, labels(split))
  dplyr::bind_rows(collection, tmp_est)
}

append_predictions <- function(collection, predictions, split, control) {
  if (!control$save_pred) {
    return(NULL)
  }
  predictions <- cbind(predictions, labels(split))
  dplyr::bind_rows(collection, predictions)
}
