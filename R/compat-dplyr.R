# Registered in `.onLoad()`
dplyr_reconstruct_tune_results <- function(data, template) {
  tune_results_reconstruct(data, template)
}

# Registered in `.onLoad()`
dplyr_reconstruct_resample_results <- function(data, template) {
  resample_results_reconstruct(data, template)
}

# Registered in `.onLoad()`
dplyr_reconstruct_iteration_results <- function(data, template) {
  iteration_results_reconstruct(data, template)
}
