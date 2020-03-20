is_recipe <- function(x) {
  inherits(x, "recipe")
}

is_preprocessor <- function(x) {
  is_recipe(x) || rlang::is_formula(x)
}

is_workflow <- function(x) {
  inherits(x, "workflow")
}
