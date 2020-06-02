# ------------------------------------------------------------------------------
# tune_results

tune_results_reconstruct <- function(x, to) {
  if (tune_results_can_reconstruct(x, to)) {
    new_tune_results_from_template(x, to)
  } else {
    new_bare_tibble(x)
  }
}

new_tune_results_from_template <- function(x, to) {
  attrs <- attributes(to)

  new_tune_results(
    x = x,
    parameters = attrs$parameters,
    metrics = attrs$metrics,
    rset_info = attrs$rset_info
  )
}

# Invariants:
# - Rows cannot be added or removed
# - Rows can be reordered
# - Columns can be added
# - Columns can be reordered
# - The `splits`, `id`, `.metrics` and `.notes` columns all must exist
tune_results_can_reconstruct <- function(x, to) {
  x_names <- names(x)
  to_names <- names(to)

  x_indicator <-
    col_equals_splits(x_names) |
    col_starts_with_id(x_names) |
    col_equals_dot_metrics(x_names) |
    col_equals_dot_notes(x_names)

  to_indicator <-
    col_equals_splits(to_names) |
    col_starts_with_id(to_names) |
    col_equals_dot_metrics(to_names) |
    col_equals_dot_notes(to_names)

  x_tune_names <- x_names[x_indicator]
  to_tune_names <- to_names[to_indicator]

  # Ignore ordering
  x_tune_names <- sort(x_tune_names)
  to_tune_names <- sort(to_tune_names)

  # Early return if names aren't identical
  if (!identical(x_tune_names, to_tune_names)) {
    return(FALSE)
  }

  # Avoid all non-bare-data-frame S3 dispatch and
  # don't compare outer data frame attributes.
  # Only look at column names and actual column data.
  x <- new_data_frame(x)
  to <- new_data_frame(to)

  # Early return if number of rows doesn't match
  if (!identical(vec_size(x), vec_size(to))) {
    return(FALSE)
  }

  x_tune_cols <- x[x_tune_names]
  to_tune_cols <- to[x_tune_names]

  # Row order doesn't matter, but we must order by the `id` cols rather
  # than a list-col. vctrs doesn't really order list-cols.
  id_col_indicator <- col_starts_with_id(x_tune_names)
  x_id_cols <- x_tune_cols[id_col_indicator]
  to_id_cols <- to_tune_cols[id_col_indicator]

  x_order <- vec_order(x_id_cols)
  to_order <- vec_order(to_id_cols)

  x_tune_cols <- vec_slice(x_tune_cols, x_order)
  to_tune_cols <- vec_slice(to_tune_cols, to_order)

  # Check identical structures of sorted tune specific columns
  identical(x_tune_cols, to_tune_cols)
}

# ------------------------------------------------------------------------------
# resample_results

resample_results_reconstruct <- function(x, to) {
  if (resample_results_can_reconstruct(x, to)) {
    new_resample_results_from_template(x, to)
  } else {
    new_bare_tibble(x)
  }
}

new_resample_results_from_template <- function(x, to) {
  attrs <- attributes(to)

  new_resample_results(
    x = x,
    parameters = attrs$parameters,
    metrics = attrs$metrics,
    rset_info = attrs$rset_info
  )
}

# Invariants:
# - Same as `tune_results`
resample_results_can_reconstruct <- function(x, to) {
  tune_results_can_reconstruct(x, to)
}

# ------------------------------------------------------------------------------

col_equals_splits <- function(x) {
  vec_equal(x, "splits")
}

col_equals_dot_notes <- function(x) {
  vec_equal(x, ".notes")
}

col_equals_dot_metrics <- function(x) {
  vec_equal(x, ".metrics")
}

col_starts_with_id <- function(x) {
  grepl("(^id$)|(^id[1-9]$)", x)
}
