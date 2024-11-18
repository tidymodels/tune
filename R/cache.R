# For iterative searches, the same preprocessor fits are recomputed
# for every iteration. Hook into the `tune_env`, an environment that
# defines a call to a tuning function, to cache repeated fits while tuning.
has_cached_result <- function(split_id, param_desc) {
  cache <- cached_results()

  if (!split_id %in% names(cache) || !param_desc %in% names(cache[[split_id]])) {
    return(FALSE)
  }

  TRUE
}

get_cached_result <- function(split_id, param_desc) {
  cache <- cached_results()
  cache[[split_id]][[param_desc]]
}

set_cached_result <- function(split_id, param_desc, workflow) {
  cache <- cached_results()
  cache[[split_id]][[param_desc]] <- workflow
  workflow
}

cached_results <- function() {
  env <- tune_env$progress_env
  if (!"cache" %in% names(env)) {
    rlang::env_bind(env, cache = rlang::new_environment())
  }

  env$cache
}
