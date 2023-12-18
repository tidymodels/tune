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
    eval_time = attrs$eval_time,
    eval_time_target = attrs$eval_time_target,
    outcomes = attrs$outcomes,
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
  results_can_reconstruct(
    x = x,
    to = to,
    detect_cols_fn = detect_cols_tune_results,
    detect_order_cols_fn = detect_order_cols_tune_results
  )
}

results_can_reconstruct <- function(x, to, detect_cols_fn, detect_order_cols_fn) {
  x_names <- names(x)
  to_names <- names(to)

  x_indicator <- detect_cols_fn(x_names)
  to_indicator <- detect_cols_fn(to_names)

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

  # Row order doesn't matter, so reorder both inputs in the same way
  order_cols_indicator <- detect_order_cols_fn(x_tune_names)

  x_order_cols <- x_tune_cols[order_cols_indicator]
  to_order_cols <- to_tune_cols[order_cols_indicator]

  x_order <- vec_order(x_order_cols)
  to_order <- vec_order(to_order_cols)

  if (!identical(x_order, to_order)) {
    x_tune_cols <- vec_slice(x_tune_cols, x_order)
    to_tune_cols <- vec_slice(to_tune_cols, to_order)
  }

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
    eval_time = attrs$eval_time,
    eval_time_target = attrs$eval_time_target,
    outcomes = attrs$outcomes,
    rset_info = attrs$rset_info
  )
}

# Invariants:
# - Same as `tune_results`
resample_results_can_reconstruct <- function(x, to) {
  tune_results_can_reconstruct(x, to)
}

# ------------------------------------------------------------------------------
# iteration_results

iteration_results_reconstruct <- function(x, to) {
  if (iteration_results_can_reconstruct(x, to)) {
    new_iteration_results_from_template(x, to)
  } else {
    new_bare_tibble(x)
  }
}

new_iteration_results_from_template <- function(x, to) {
  attrs <- attributes(to)

  new_iteration_results(
    x = x,
    parameters = attrs$parameters,
    metrics = attrs$metrics,
    eval_time = attrs$eval_time,
    eval_time_target = attrs$eval_time_target,
    outcomes = attrs$outcomes,
    rset_info = attrs$rset_info,
    workflow = attrs$workflow
  )
}

# Invariants:
# - Same as `tune_results`, but also special cases the `.iter` column
iteration_results_can_reconstruct <- function(x, to) {
  results_can_reconstruct(
    x = x,
    to = to,
    detect_cols_fn = detect_cols_iteration_results,
    detect_order_cols_fn = detect_order_cols_iteration_results
  )
}

# ------------------------------------------------------------------------------

# - `detect_cols_*()` detects the "special" columns required by the subclass
# - `detect_order_cols_*()` detects the columns used to order the subclass rows.
#   The order columns are generally the special columns but without the
#   list-columns, since `vec_order()` doesn't really order those.

detect_cols_tune_results <- function(x) {
  col_equals_splits(x) | col_starts_with_id(x) | col_equals_dot_metrics(x) | col_equals_dot_notes(x)
}

detect_cols_iteration_results <- function(x) {
  detect_cols_tune_results(x) | col_equals_dot_iter(x)
}

detect_order_cols_tune_results <- function(x) {
  col_starts_with_id(x)
}

detect_order_cols_iteration_results <- function(x) {
  detect_order_cols_tune_results(x) | col_equals_dot_iter(x)
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

col_equals_dot_iter <- function(x) {
  vec_equal(x, ".iter")
}

col_starts_with_id <- function(x) {
  grepl("(^id$)|(^id[1-9]$)", x)
}
