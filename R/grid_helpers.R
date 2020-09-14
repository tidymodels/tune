# recipe-oriented helpers

train_recipe <- function(split, workflow, grid) {
  original_recipe <- workflows::pull_workflow_preprocessor(workflow)

  if (!is.null(grid)) {
    updated_recipe <- merge(original_recipe, grid)$x[[1]]
  } else {
    updated_recipe <- original_recipe
  }

  workflow <- set_workflow_recipe(workflow, updated_recipe)

  training <- rsample::analysis(split)

  workflow <- .fit_pre(workflow, training)

  # Always reset to the original recipe so `parameters()` can be used on this
  # object. The prepped updated recipe is stored in the mold.
  workflow <- set_workflow_recipe(workflow, original_recipe)

  workflow
}

train_model <- function(workflow, grid, control) {
  original_spec <- workflows::pull_workflow_spec(workflow)

  if (!is.null(grid)) {
    updated_spec <- merge(original_spec, grid)$x[[1]]
  } else {
    updated_spec <- original_spec
  }

  workflow <- set_workflow_spec(workflow, updated_spec)

  workflow <- .fit_model(workflow, control)

  # Always reset to the original spec so `parameters()` can be used on this
  # object. The fit model is stored in `workflow$fit$fit`
  workflow <- set_workflow_spec(workflow, original_spec)

  workflow
}

predict_model <- function(split, workflow, grid, metrics) {
  model <- workflows::pull_workflow_fit(workflow)

  forged <- forge_from_workflow(split, workflow)

  x_vals <- forged$predictors
  y_vals <- forged$outcomes

  orig_rows <- as.integer(split, data = "assessment")

  # Determine the type of prediction that is required
  type_info <- metrics_info(metrics)
  types <- unique(type_info$type)

  # Split `grid` from the parameters used to fit the model and any potential
  # sub-model parameters
  submod_col <- names(grid) == ".submodels"
  fixed_param <- grid[, !submod_col]

  res <- NULL
  merge_vars <- c(".row", names(fixed_param))

  for (type_iter in types) {
    # Regular predictions
    tmp_res <-
      predict(model, x_vals, type = type_iter) %>%
      mutate(.row = orig_rows) %>%
      cbind(fixed_param, row.names = NULL)

    if (any(submod_col)) {
      submod_length <- purrr::map_int(grid$.submodels[[1]], length)
      has_submodels <- any(submod_length > 0)

      if (has_submodels) {
        submod_param <- names(grid$.submodels[[1]])
        mp_call <-
          call2(
            "multi_predict",
            .ns = "parsnip",
            object = expr(model),
            new_data = expr(x_vals),
            type = type_iter,
            !!!make_submod_arg(grid, model)
          )
        tmp_res <-
          eval_tidy(mp_call) %>%
          mutate(.row = orig_rows) %>%
          unnest(cols = dplyr::starts_with(".pred")) %>%
          cbind(fixed_param %>% dplyr::select(-one_of(submod_param)),
                row.names = NULL) %>%
          # go back to user-defined name
          dplyr::rename(!!!make_rename_arg(grid, model)) %>%
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

make_submod_arg <- function(grid, model) {
  # Assumes only one submodel parameter per model
  real_name <-
    parsnip::get_from_env(paste(class(model$spec)[1], "args", sep = "_")) %>%
    dplyr::filter(has_submodel & engine == model$spec$engine) %>%
    dplyr::pull(parsnip)
  submods <- grid$.submodels[[1]]
  names(submods) <- real_name
  submods
}

make_rename_arg <- function(grid, model) {
  # Assumes only one submodel parameter per model
  real_name <-
    parsnip::get_from_env(paste(class(model$spec)[1], "args", sep = "_")) %>%
    dplyr::filter(has_submodel & engine == model$spec$engine) %>%
    dplyr::pull(parsnip)
  submods <- grid$.submodels[[1]]
  res <- list(real_name)
  names(res) <- names(submods)
  res
}


# ------------------------------------------------------------------------------
# Formula-oriented helpers

train_formula <- function(split, workflow) {
  training <- rsample::analysis(split)
  .fit_pre(workflow, training)
}

# ------------------------------------------------------------------------------
# Variables-oriented helpers

train_variables <- function(split, workflow) {
  training <- rsample::analysis(split)
  .fit_pre(workflow, training)
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
