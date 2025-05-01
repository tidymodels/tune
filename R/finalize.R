#' Splice final parameters into objects
#'
#' The `finalize_*` functions take a list or tibble of tuning parameter values and
#' update objects with those values.
#'
#' @param x A recipe, \pkg{parsnip} model specification, \pkg{tailor}
#'  postprocessor, or workflow.
#' @param parameters A list or 1-row tibble of parameter values. Note that the
#'  column names of the tibble should be the `id` fields attached to `tune()`.
#'  For example, in the `Examples` section below, the model has `tune("K")`. In
#'  this case, the parameter tibble should be "K" and not "neighbors".
#' @return An updated version of `x`.
#' @export
#' @examplesIf tune:::should_run_examples(suggests = "kknn")
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
finalize_model <- function(x, parameters) {
  if (!inherits(x, "model_spec")) {
    cli::cli_abort("{.arg x} should be a parsnip model specification, not
                    {.obj_type_friendly {x}}.")
  }
  check_final_param(parameters)
  pset <- hardhat::extract_parameter_set_dials(x)
  if (tibble::is_tibble(parameters)) {
    parameters <- as.list(parameters)
  }

  parameters <- parameters[names(parameters) %in% pset$id]

  discordant <- dplyr::filter(pset, id != name & id %in% names(parameters))
  if (nrow(discordant) > 0) {
    for (i in 1:nrow(discordant)) {
      names(parameters)[names(parameters) == discordant$id[i]] <-
        discordant$name[i]
    }
  }
  rlang::exec(update, object = x, !!!parameters)
}

#' @export
#' @rdname finalize_model
finalize_recipe <- function(x, parameters) {
  if (!inherits(x, "recipe")) {
    cli::cli_abort("{.arg x} should be a recipe, not {.obj_type_friendly {x}}.")
  }
  check_final_param(parameters)
  pset <-
    hardhat::extract_parameter_set_dials(x) %>%
    dplyr::filter(id %in% names(parameters) & source == "recipe")

  if (tibble::is_tibble(parameters)) {
    parameters <- as.list(parameters)
  }

  parameters <- parameters[names(parameters) %in% pset$id]
  parameters <- parameters[pset$id]
  split <- vec_split(pset, pset$component_id)
  pset <- split[["val"]]
  for (i in seq_along(pset)) {
    pset_params <- parameters[names(parameters) %in% pset[[i]]$id]
    x <- complete_steps(pset_params, pset[[i]], x)
  }
  x
}

#' @export
#' @rdname finalize_model
finalize_workflow <- function(x, parameters) {
  if (!inherits(x, "workflow")) {
    cli::cli_abort("{.arg x} should be a workflow, not {.obj_type_friendly {x}}.")
  }
  check_final_param(parameters)

  mod <- extract_spec_parsnip(x)
  mod <- finalize_model(mod, parameters)
  x <- set_workflow_spec(x, mod)

  if (has_preprocessor_recipe(x)) {
    rec <- extract_preprocessor(x)
    rec <- finalize_recipe(rec, parameters)
    x <- set_workflow_recipe(x, rec)
  }

  if (has_postprocessor(x)) {
    tailor <- extract_postprocessor(x)
    tailor <- finalize_tailor(tailor, parameters)
    x <- set_workflow_tailor(x, tailor)
  }

  x
}

#' @export
#' @rdname finalize_model
finalize_tailor <- function(x, parameters) {
  if (!inherits(x, "tailor")) {
    cli::cli_abort("{.arg x} should be a tailor, not {.obj_type_friendly {x}}.")
  }
  check_final_param(parameters)
  pset <-
    hardhat::extract_parameter_set_dials(x) %>%
    dplyr::filter(id %in% names(parameters) & source == "tailor") %>%
    dplyr::as_tibble()

  if (tibble::is_tibble(parameters)) {
    parameters <- as.list(parameters)
  }

  parameters <- parameters[names(parameters) %in% pset$id]
  parameters <- parameters[pset$id]

  for (i in seq_along(x$adjustments)) {
    adj <- x$adjustments[[i]]
    adj_comps <- purrr::map_lgl(pset$component, ~ inherits(adj, .x))
    if (any(adj_comps)) {
      adj_ids <- pset$id[adj_comps]
      prm_nm <- pset$name[adj_comps]
      adj_prms <- parameters[names(parameters) %in% adj_ids]
      names(adj_prms) <- prm_nm
      adj$arguments <- purrr::list_modify(adj$arguments, !!!adj_prms)
      x$adjustments[[i]] <- adj
    }
  }
  x
}

# ------------------------------------------------------------------------------

check_final_param <- function(x) {
  if (!is.list(x) & !tibble::is_tibble(x)) {
    cli::cli_abort("The parameter object should be a {.cls list} or
                   {.cls tibble}, not {.obj_type_friendly {x}}.")
  }
  if (tibble::is_tibble(x) && nrow(x) > 1) {
    cli::cli_abort("The parameter {.arg tibble} should have a single row.")
  }
  invisible(x)
}

complete_steps <- function(param, pset, object) {
  # find the corresponding step in the recipe
  step_ids <- purrr::map_chr(object$steps, ~ .x$id)
  step_index <- which(unique(pset$component_id) == step_ids)
  step_to_update <- object$steps[[step_index]]

  names(param) <- pset$name

  step_to_update <- rlang::exec(update, object = step_to_update, !!!param)
  object$steps[[step_index]] <- step_to_update
  object
}
