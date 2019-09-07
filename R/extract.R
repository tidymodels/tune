
extract_details <- function(object, extractor) {
  if (is.null(extractor)) {
    return(list())
  }
  try(extractor(object), silent = TRUE)
}

# ------------------------------------------------------------------------------

# Grab the new results, make sure that they align row-wise with the rsample
# object and then bind columns
pull_engine <- function(rs, res, col) {
  id_cols <- grep("^id", names(rs), value = TRUE)
  rs <- dplyr::arrange(rs, !!!syms(id_cols))
  pulled_vals <-
    purrr::map_dfr(res, ~.x[[col]]) %>%
    tidyr::nest(-starts_with("id"), .key = !!col) %>%
    dplyr::arrange(!!!syms(id_cols)) %>%
    dplyr::select(-!!id_cols)
  rs %>% bind_cols(pulled_vals)
}

pull_metrics <- function(rs, res) {
  pull_engine(rs, res, ".metrics")
}

pull_extracts <- function(rs, res, control) {
  if (!is.null(control$extract)) {
    rs <- pull_engine(rs, res, ".extract")
  }
  rs
}

pull_predictions <- function(rs, res, control) {
  if (control$save_pred) {
    rs <- pull_engine(rs, res, ".predictions")
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
