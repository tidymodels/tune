dplyr_pre_1.0.0 <- function() {
  utils::packageVersion("dplyr") <= "0.8.5"
}

# ------------------------------------------------------------------------------
# tune_results

# Registered in `.onLoad()`
mutate_tune_results <- function(.data, ...) {
  out <- NextMethod()
  tune_results_reconstruct(out, .data)
}

# Registered in `.onLoad()`
arrange_tune_results <- function(.data, ...) {
  out <- NextMethod()
  tune_results_reconstruct(out, .data)
}

# Registered in `.onLoad()`
filter_tune_results <- function(.data, ...) {
  out <- NextMethod()
  tune_results_reconstruct(out, .data)
}

# Registered in `.onLoad()`
rename_tune_results <- function(.data, ...) {
  out <- NextMethod()
  tune_results_reconstruct(out, .data)
}

# Registered in `.onLoad()`
select_tune_results <- function(.data, ...) {
  out <- NextMethod()
  tune_results_reconstruct(out, .data)
}

# Registered in `.onLoad()`
slice_tune_results <- function(.data, ...) {
  out <- NextMethod()
  tune_results_reconstruct(out, .data)
}

# ------------------------------------------------------------------------------
# resample_results

# Registered in `.onLoad()`
mutate_resample_results <- function(.data, ...) {
  out <- NextMethod()
  resample_results_reconstruct(out, .data)
}

# Registered in `.onLoad()`
arrange_resample_results <- function(.data, ...) {
  out <- NextMethod()
  resample_results_reconstruct(out, .data)
}

# Registered in `.onLoad()`
filter_resample_results <- function(.data, ...) {
  out <- NextMethod()
  resample_results_reconstruct(out, .data)
}

# Registered in `.onLoad()`
rename_resample_results <- function(.data, ...) {
  out <- NextMethod()
  resample_results_reconstruct(out, .data)
}

# Registered in `.onLoad()`
select_resample_results <- function(.data, ...) {
  out <- NextMethod()
  resample_results_reconstruct(out, .data)
}

# Registered in `.onLoad()`
slice_resample_results <- function(.data, ...) {
  out <- NextMethod()
  resample_results_reconstruct(out, .data)
}
