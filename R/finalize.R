#' Splice final parameters into objects
#'
#' The `finalize_*` functions take a list or tibble of tuning parameter values and
#' update objects with those values.
#'
#' @param x A recipe, `parsnip` model specification, or workflow.
#' @param parameters A list or 1-row tibble of parameter values. Note that the
#'  column names of the tibble should be the `id` fields attached to `tune()`.
#'  For example, in the `Examples` section below, the model has `tune("K")`. In
#'  this case, the parameter tibble should be "K" and not "neighbors".
#' @return An updated version of `x`.
#' @export
#' @examples
#' \donttest{
#' data("example_ames_knn")
#'
#' library(parsnip)
#' knn_model <-
#'   nearest_neighbor(
#'     mode = "regression",
#'     neighbors = tune("K"),
#'     weight_func = tune(),
#'     dist_power = tune()
#'   ) %>%
#'   set_engine("kknn")
#'
#' lowest_rmse <- select_best(ames_grid_search, metric = "rmse")
#' lowest_rmse
#'
#' knn_model
#' finalize_model(knn_model, lowest_rmse)
#' }
finalize_model <- function(x, parameters) {
  if (!inherits(x, "model_spec")) {
    stop("`x` should be a parsnip model specification.")
  }
  check_final_param(parameters)
  pset <- parameters(x)
  if (tibble::is_tibble(parameters)) {
    parameters <- as.list(parameters)
  }

  parameters <- parameters[names(parameters) %in% pset$id]

  discordant <- dplyr::filter(pset, id != name & id %in% names(parameters))
  if (nrow(discordant) > 0) {
    for (i in 1:nrow(discordant)) {
      names(parameters)[ names(parameters) == discordant$id[i] ] <-
        discordant$name[i]
    }
  }
  rlang::exec(update, object = x, !!!parameters)
}

#' @export
#' @rdname finalize_model
finalize_recipe <- function(x, parameters) {
  if (!inherits(x, "recipe")) {
    stop("`x` should be a recipe.")
  }
  check_final_param(parameters)
  pset <-
    dials::parameters(x) %>%
    dplyr::filter(id %in% names(parameters) & source == "recipe")

  if (tibble::is_tibble(parameters)) {
    parameters <- as.list(parameters)
  }

  parameters <- parameters[names(parameters) %in% pset$id]
  parameters <- parameters[pset$id]
  split <- vec_split(pset, pset$component_id)
  pset <- split[["val"]]
  for (i in seq_along(pset)) {
    x <- complete_steps(parameters[[i]], pset[[i]], x)
  }
  x
}

#' @export
#' @rdname finalize_model
finalize_workflow <- function(x, parameters) {
  if (!inherits(x, "workflow")) {
    stop("`x` should be a workflow")
  }
  check_final_param(parameters)

  mod <- workflows::pull_workflow_spec(x)
  mod <- finalize_model(mod, parameters)
  x <- set_workflow_spec(x, mod)

  if (has_preprocessor_recipe(x)) {
    rec <- workflows::pull_workflow_preprocessor(x)
    rec <- finalize_recipe(rec, parameters)
    x <- set_workflow_recipe(x, rec)
  }

  x
}

# ------------------------------------------------------------------------------

check_final_param <- function(x) {
  if (!is.list(x) & !tibble::is_tibble(x)) {
    rlang::abort("The parameter object should be a list or tibble")
  }
  if (tibble::is_tibble(x) && nrow(x) > 1) {
    rlang::abort("The parameter tibble should have a single row.")
  }
  invisible(x)
}

complete_steps <- function(param, pset, object) {
  # find the corresponding step in the recipe
  step_ids <- purrr::map_chr(object$steps, ~ .x$id)
  step_index <- which(pset$component_id == step_ids)
  tmp <- object$steps[[step_index]]

  list_para <- list(param)
  names(param) <- pset$name

  tmp <- rlang::exec(update, object = tmp, !!!param)
  object$steps[[step_index]] <- tmp
  object
}




