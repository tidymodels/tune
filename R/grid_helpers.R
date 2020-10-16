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

forge_from_workflow <- function(split, workflow) {
  new_data <- rsample::assessment(split)

  blueprint <- workflow$pre$mold$blueprint
  forged <- hardhat::forge(new_data, blueprint, outcomes = TRUE)

  forged
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

finalize_workflow_spec <- function(workflow, grid_model) {
  # Already finalized, nothing to tune
  if (ncol(grid_model) == 0L) {
    return(workflow)
  }

  spec <- workflows::pull_workflow_spec(workflow)
  spec <- merge(spec, grid_model)$x[[1]]

  workflow <- set_workflow_spec(workflow, spec)

  workflow
}

finalize_workflow_preprocessor <- function(workflow, grid_preprocessor) {
  # Already finalized, nothing to tune
  if (ncol(grid_preprocessor) == 0L) {
    return(workflow)
  }

  recipe <- workflows::pull_workflow_preprocessor(workflow)
  recipe <- merge(recipe, grid_preprocessor)$x[[1]]

  workflow <- set_workflow_recipe(workflow, recipe)

  workflow
}

# ------------------------------------------------------------------------------

compute_grid_info <- function(workflow, grid) {
  # For `fit_resamples()`
  if (is.null(grid)) {
    out <- new_grid_info_resamples()
    return(out)
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

new_grid_info_resamples <- function() {
  msgs_preprocessor <- new_msgs_preprocessor(
    i = 1L,
    n = 1L
  )

  msgs_model <- new_msgs_model(
    i = 1L,
    n = 1L,
    msgs_preprocessor = msgs_preprocessor
  )

  iter_config <- list("Preprocessor1_Model1")

  data <- tibble::tibble(
    .iter_model = 1L,
    .iter_config = iter_config,
    .msg_model = msgs_model,
    .submodels = list(list())
  )

  out <- tibble::tibble(
    .iter_recipe = 1L,
    .msg_preprocessor = msgs_preprocessor,
    data = list(data)
  )

  out
}

compute_grid_info_recipe <- function(workflow,
                                     grid,
                                     parameters_model,
                                     parameters_recipe) {
  out <- grid

  out <- add_iter_recipe(out)
  out <- dplyr::mutate(out, .iter_model = 1L)

  n_preprocessors <- nrow(out)

  # preprocessor <i>/<n>
  msgs_preprocessor <- new_msgs_preprocessor(
    i = out[[".iter_recipe"]],
    n = n_preprocessors
  )

  # preprocessor <i>/<n>, model 1/1
  msgs_model <- new_msgs_model(
    i = 1L,
    n = 1L,
    msgs_preprocessor = msgs_preprocessor
  )

  # Preprocessor<i>_Model1
  ids <- format_with_padding(seq_len(n_preprocessors))
  iter_configs <- paste0("Preprocessor", ids, "_Model1")
  iter_configs <- as.list(iter_configs)

  out <- tibble::add_column(out, .msg_preprocessor = msgs_preprocessor, .after = ".iter_recipe")
  out <- tibble::add_column(out, .iter_config = iter_configs, .after = ".iter_model")
  out <- tibble::add_column(out, .msg_model = msgs_model, .after = ".iter_config")

  # Manually add .submodels column, which will always have empty lists
  .submodels <- rep_len(list(list()), n_preprocessors)
  out <- tibble::add_column(out, .submodels = .submodels, .after = ".msg_model")

  cols <- rlang::expr(
    c(.iter_model, .iter_config, .msg_model, .submodels)
  )

  if (tidyr_new_interface()) {
    out <- tidyr::nest(out, data = !!cols)
  } else {
    out <- tidyr::nest(out, !!cols)
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

  n_fit_models <- nrow(out)

  # preprocessor 1/1
  msgs_preprocessor <- new_msgs_preprocessor(i = 1L, n = 1L)
  msgs_preprocessor <- rep(msgs_preprocessor, times = n_fit_models)

  # preprocessor 1/1, model <i_fit>/<n_fit>
  msgs_model <- new_msgs_model(
    i = seq_len(n_fit_models),
    n = n_fit_models,
    msgs_preprocessor = msgs_preprocessor
  )

  # Preprocessor1_Model<i>
  iter_configs <- compute_config_ids(out, "Preprocessor1")

  out <- tibble::add_column(out, .msg_preprocessor = msgs_preprocessor, .after = ".iter_recipe")
  out <- tibble::add_column(out, .iter_config = iter_configs, .after = ".iter_model")
  out <- tibble::add_column(out, .msg_model = msgs_model, .after = ".iter_config")

  cols <- rlang::expr(
    c(.iter_model, .iter_config, .msg_model, tidyselect::all_of(parameter_names_model), .submodels)
  )

  if (tidyr_new_interface()) {
    out <- tidyr::nest(out, data = !!cols)
  } else {
    out <- tidyr::nest(out, !!cols)
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

  n_preprocessors <- nrow(out)

  # preprocessor <i_pre>/<n_pre>
  msgs_preprocessor <- new_msgs_preprocessor(
    i = out[[".iter_recipe"]],
    n = n_preprocessors
  )

  out <- tibble::add_column(.data = out, .msg_preprocessor = msgs_preprocessor, .after = ".iter_recipe")

  ids_preprocessor <- format_with_padding(seq_len(n_preprocessors))
  ids_preprocessor <- paste0("Preprocessor", ids_preprocessor)

  model_grids <- out[["data"]]

  for (i in seq_len(n_preprocessors)) {
    model_grid <- model_grids[[i]]

    n_fit_models <- nrow(model_grid)

    msg_preprocessor <- msgs_preprocessor[[i]]
    id_preprocessor <- ids_preprocessor[[i]]

    # preprocessor <i_pre>/<n_pre>, model <i_mod>/<n_mod>
    msgs_model <- new_msgs_model(
      i = model_grid[[".iter_model"]],
      n = n_fit_models,
      msgs_preprocessor = msg_preprocessor
    )

    # Preprocessor<i_pre>_Model<i>
    iter_configs <- compute_config_ids(model_grid, id_preprocessor)

    model_grid <- tibble::add_column(model_grid, .iter_config = iter_configs, .after = ".iter_model")
    model_grid <- tibble::add_column(model_grid, .msg_model = msgs_model, .after = ".iter_config")

    model_grids[[i]] <- model_grid
  }

  out[["data"]] <- model_grids

  out
}

new_msgs_preprocessor <- function(i, n) {
  paste0("preprocessor ", i, "/", n)
}
new_msgs_model <- function(i, n, msgs_preprocessor) {
  paste0(msgs_preprocessor, ", model ", i, "/", n)
}

add_iter_recipe <- function(data) {
  tibble::rowid_to_column(data, var = ".iter_recipe")
}
add_iter_model <- function(data) {
  tibble::rowid_to_column(data, var = ".iter_model")
}

# c(1, 10) -> c("01", "10")
format_with_padding <- function(x) {
  gsub(" ", "0", format(x))
}

compute_config_ids <- function(data, id_preprocessor) {
  submodels <- unnest(data, .submodels, keep_empty = TRUE)
  submodels <- pull(submodels, .submodels)

  # Current model that actually is fit is not included in the submodel count
  # so we add 1
  model_sizes <- lengths(submodels) + 1L

  n_total_models <- sum(model_sizes)

  ids <- format_with_padding(seq_len(n_total_models))
  ids <- paste0(id_preprocessor, "_Model", ids)

  n_fit_models <- nrow(data)

  out <- vector("list", length = n_fit_models)

  start <- 1L

  for (i in seq_len(n_fit_models)) {
    size <- model_sizes[[i]]
    stop <- start + size - 1L
    out[[i]] <- ids[rlang::seq2(start, stop)]
    start <- stop + 1L
  }

  out
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
