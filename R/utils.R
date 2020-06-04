is_recipe <- function(x) {
  inherits(x, "recipe")
}

is_preprocessor <- function(x) {
  is_recipe(x) || rlang::is_formula(x)
}

is_workflow <- function(x) {
  inherits(x, "workflow")
}

# new_tibble() currently doesn't strip attributes
# https://github.com/tidyverse/tibble/pull/769
new_bare_tibble <- function(x, ..., class = character()) {
  x <- vctrs::new_data_frame(x)
  tibble::new_tibble(x, nrow = nrow(x), ..., class = class)
}

new_tune_results <- function(x, parameters, metrics, rset_info, ..., class = character()) {
  new_bare_tibble(
    x = x,
    parameters = parameters,
    metrics = metrics,
    rset_info = rset_info,
    ...,
    class = c(class, "tune_results")
  )
}

new_resample_results <- function(x, parameters, metrics, rset_info) {
  new_tune_results(
    x = x,
    parameters = parameters,
    metrics = metrics,
    rset_info = rset_info,
    class = "resample_results"
  )
}
