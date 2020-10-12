predict_model <- function(split, workflow, grid, metrics, submodels = NULL) {
  model <- workflows::pull_workflow_fit(workflow)

  forged <- forge_from_workflow(split, workflow)

  x_vals <- forged$predictors
  y_vals <- forged$outcomes

  orig_rows <- as.integer(split, data = "assessment")

  # Determine the type of prediction that is required
  type_info <- metrics_info(metrics)
  types <- unique(type_info$type)

  res <- NULL
  merge_vars <- c(".row", names(grid))

  for (type_iter in types) {
    # Regular predictions
    tmp_res <-
      predict(model, x_vals, type = type_iter) %>%
      mutate(.row = orig_rows) %>%
      cbind(grid, row.names = NULL)

    if (!is.null(submodels)) {
      submod_length <- lengths(submodels)
      has_submodels <- any(submod_length > 0)

      if (has_submodels) {
        submod_param <- names(submodels)
        mp_call <-
          call2(
            "multi_predict",
            .ns = "parsnip",
            object = expr(model),
            new_data = expr(x_vals),
            type = type_iter,
            !!!make_submod_arg(grid, model, submodels)
          )
        tmp_res <-
          eval_tidy(mp_call) %>%
          mutate(.row = orig_rows) %>%
          unnest(cols = dplyr::starts_with(".pred")) %>%
          cbind(dplyr::select(grid, -tidyselect::all_of(submod_param)), row.names = NULL) %>%
          # go back to user-defined name
          dplyr::rename(!!!make_rename_arg(grid, model, submodels)) %>%
          dplyr::select(dplyr::one_of(names(tmp_res))) %>%
          dplyr::bind_rows(tmp_res)
      }
    }

    if (!is.null(res)) {
      res <- dplyr::full_join(res, tmp_res, by = merge_vars)
    } else {
      res <- tmp_res
    }

    rm(tmp_res)
  } # end type loop

  # Add outcome data
  y_vals <- dplyr::mutate(y_vals, .row = orig_rows)
  res <- dplyr::full_join(res, y_vals, by = ".row")

  tibble::as_tibble(res)
}

make_submod_arg <- function(grid, model, submodels) {
  # Assumes only one submodel parameter per model
  real_name <-
    parsnip::get_from_env(paste(class(model$spec)[1], "args", sep = "_")) %>%
    dplyr::filter(has_submodel & engine == model$spec$engine) %>%
    dplyr::pull(parsnip)
  names(submodels) <- real_name
  submodels
}

make_rename_arg <- function(grid, model, submodels) {
  # Assumes only one submodel parameter per model
  real_name <-
    parsnip::get_from_env(paste(class(model$spec)[1], "args", sep = "_")) %>%
    dplyr::filter(has_submodel & engine == model$spec$engine) %>%
    dplyr::pull(parsnip)
  res <- list(real_name)
  names(res) <- names(submodels)
  res
}

# ------------------------------------------------------------------------------

finalize_workflow_spec <- function(workflow, grid) {
  spec <- workflows::pull_workflow_spec(workflow)
  spec <- merge(spec, grid)$x[[1]]
  set_workflow_spec(workflow, spec)
}

finalize_workflow_recipe <- function(workflow, grid) {
  recipe <- workflows::pull_workflow_preprocessor(workflow)
  recipe <- merge(recipe, grid)$x[[1]]
  set_workflow_recipe(workflow, recipe)
}

# ------------------------------------------------------------------------------

compute_grid_info <- function(workflow, grid) {
  # For `fit_resamples()`
  if (is.null(grid)) {
    return(NULL)
  }

  grid <- tibble::as_tibble(grid)

  parameters <- dials::parameters(workflow)
  parameters_model <- dplyr::filter(parameters, source == "model_spec")
  parameters_recipe <- dplyr::filter(parameters, source == "recipe")

  any_parameters_model <- nrow(parameters_model) > 0
  any_parameters_recipe <- nrow(parameters_recipe) > 0

  if (any_parameters_model) {
    if (any_parameters_recipe) {
      compute_grid_info_model_and_recipe(workflow, grid, parameters_model, parameters_recipe)
    } else {
      compute_grid_info_model(workflow, grid, parameters_model, parameters_recipe)
    }
  } else {
    if (any_parameters_recipe) {
      compute_grid_info_recipe(workflow, grid, parameters_model, parameters_recipe)
    } else {
      rlang::abort("Internal error: `workflow` should have some tunable parameters if `grid` is not `NULL`.")
    }
  }
}

compute_grid_info_recipe <- function(workflow,
                                     grid,
                                     parameters_model,
                                     parameters_recipe) {
  out <- grid

  out <- add_iter_recipe(out)
  out <- dplyr::mutate(out, .iter_model = 1L)

  if (tidyr_new_interface()) {
    out <- tidyr::nest(out, data = .iter_model)
  } else {
    out <- tidyr::nest(out, .iter_model)
  }

  out
}

compute_grid_info_model <- function(workflow,
                                    grid,
                                    parameters_model,
                                    parameters_recipe) {
  spec <- workflows::pull_workflow_spec(workflow)
  out <- min_grid(spec, grid)

  parameter_names_model <- parameters_model[["id"]]

  out <- dplyr::mutate(out, .iter_recipe = 1L)
  out <- add_iter_model(out)

  if (tidyr_new_interface()) {
    out <- tidyr::nest(out, data = c(.iter_model, tidyselect::all_of(parameter_names_model), .submodels))
  } else {
    out <- tidyr::nest(out, c(.iter_model, tidyselect::all_of(parameter_names_model), .submodels))
  }

  out
}

compute_grid_info_model_and_recipe <- function(workflow,
                                               grid,
                                               parameters_model,
                                               parameters_recipe) {
  parameter_names_model <- parameters_model[["id"]]
  parameter_names_recipe <- parameters_recipe[["id"]]

  # Nest model parameters, keep recipe parameters outside
  if (tidyr_new_interface()) {
    out <- tidyr::nest(grid, data = tidyselect::all_of(parameter_names_model))
  } else {
    out <- tidyr::nest(grid, tidyselect::all_of(parameter_names_model))
  }

  spec <- workflows::pull_workflow_spec(workflow)

  # Minify model grids
  out$data <- purrr::map(out$data, min_grid, x = spec)

  # Add model iteration
  out$data <- purrr::map(out$data, add_iter_model)

  # Add recipe iteration
  out <- add_iter_recipe(out)

  out
}

add_iter_recipe <- function(data) {
  tibble::rowid_to_column(data, var = ".iter_recipe")
}
add_iter_model <- function(data) {
  tibble::rowid_to_column(data, var = ".iter_model")
}

# ------------------------------------------------------------------------------

has_preprocessor <- function(workflow) {
  has_preprocessor_recipe(workflow) ||
    has_preprocessor_formula(workflow) ||
    has_preprocessor_variables(workflow)
}

has_preprocessor_recipe <- function(workflow) {
  "recipe" %in% names(workflow$pre$actions)
}

has_preprocessor_formula <- function(workflow) {
  "formula" %in% names(workflow$pre$actions)
}

has_preprocessor_variables <- function(workflow) {
  "variables" %in% names(workflow$pre$actions)
}

has_spec <- function(workflow) {
  "model" %in% names(workflow$fit$actions)
}

set_workflow_spec <- function(workflow, spec) {
  workflow$fit$actions$model$spec <- spec
  workflow
}

set_workflow_recipe <- function(workflow, recipe) {
  workflow$pre$actions$recipe$recipe <- recipe
  workflow
}
