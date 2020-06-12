is_recipe <- function(x) {
  inherits(x, "recipe")
}

is_preprocessor <- function(x) {
  is_recipe(x) || rlang::is_formula(x)
}

is_workflow <- function(x) {
  inherits(x, "workflow")
}

## similar to rsample func, but only makes one name, not a vector of names
names0 <- function(num, prefix = "x") {
  if (num < 1) {
    rlang::abort("`num` should be > 0")
  }
  ind <- format(num)
  ind <- gsub(" ", "0", ind)
  paste0(prefix, ind)
}

# new_tibble() currently doesn't strip attributes
# https://github.com/tidyverse/tibble/pull/769
new_bare_tibble <- function(x, ..., class = character()) {
  x <- vctrs::new_data_frame(x)
  tibble::new_tibble(x, nrow = nrow(x), ..., class = class)
}
