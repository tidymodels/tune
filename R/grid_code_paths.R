tune_nothing <- function(resamples, grid, workflow, metrics, control)  {
  resample_loop(resamples, workflow, metrics, control)
}

tune_model_with_preprocessor <- function(resamples, grid, workflow, metrics, control) {
  tune_grid_loop(resamples, grid, workflow, metrics, control, iter_model_with_preprocessor)
}
tune_model_and_recipe <- function(resamples, grid, workflow, metrics, control) {
  tune_grid_loop(resamples, grid, workflow, metrics, control, iter_model_and_recipe)
}
tune_recipe <- function(resamples, grid, workflow, metrics, control) {
  tune_grid_loop(resamples, grid, workflow, metrics, control, iter_recipe)
}

tune_grid_loop <- function(resamples, grid, workflow, metrics, control, fn_iter) {
  B <- nrow(resamples)

  `%op%` <- get_operator(control$allow_par, workflow)

  fn_iter_safely <- super_safely_iterate(fn_iter)

  load_pkgs <- c(control$pkgs, required_pkgs(workflow))

  grid_info <- compute_grid_info(workflow, grid)

  results <-
    foreach::foreach(rs_iter = 1:B, .packages = load_pkgs, .errorhandling = "pass") %op%
    fn_iter_safely(rs_iter, resamples, grid_info, grid, workflow, metrics, control)

  resamples <- pull_metrics(resamples, results, control)
  resamples <- pull_notes(resamples, results, control)
  resamples <- pull_extracts(resamples, results, control)
  resamples <- pull_predictions(resamples, results, control)
  resamples <- pull_all_outcome_names(resamples, results)

  resamples
}

# ------------------------------------------------------------------------------

iter_model_with_preprocessor <- function(rs_iter,
                                         resamples,
                                         grid_info,
                                         grid,
                                         workflow,
                                         metrics,
                                         control) {
  load_pkgs(workflow)
  load_namespace(control$pkgs)
  set.seed(resamples$.seed[[rs_iter]])

  control_parsnip <- parsnip::control_parsnip(verbosity = 0, catch = TRUE)
  control_workflow <- control_workflow(control_parsnip = control_parsnip)

  event_level <- control$event_level

  metric_est <- NULL
  extracted <- NULL
  pred_vals <- NULL
  all_outcome_names <- list()
  .notes <- NULL

  params <- dials::parameters(workflow)
  model_params <- dplyr::filter(params, source == "model_spec")

  param_names <- dplyr::pull(params, "id")
  model_param_names <- dplyr::pull(model_params, "id")

  # ----------------------------------------------------------------------------

  split <- resamples$splits[[rs_iter]]
  training <- rsample::analysis(split)

  workflow <- catch_and_log(
    .fit_pre(workflow, training),
    control,
    split,
    "preprocessor",
    notes = .notes
  )

  # check for failure
  if (is_failure(workflow)) {
    out <- list(
      .metrics = metric_est,
      .extracts = extracted,
      .predictions = pred_vals,
      .all_outcome_names = all_outcome_names,
      .notes = .notes
    )

    return(out)
  }

  # ----------------------------------------------------------------------------

  # No recipe to tune, jump straight to model parameter info
  grid_info_model <- grid_info[["data"]][[1]]

  num_mod <- nrow(grid_info_model)

  num_submodels <- grid_info_model %>%
    unnest(.submodels, keep_empty = TRUE) %>%
    pull(.submodels) %>%
    lengths()

  # ----------------------------------------------------------------------------

  original_workflow <- workflow

  for (mod_iter in 1:num_mod) {
    workflow <- original_workflow

    submodel_id <- mod_iter + sum(vec_slice(num_submodels, 1:mod_iter))
    mod_msg <- paste0("model ", format(1:num_mod)[mod_iter], "/", num_mod)
    mod_id <- vec_slice(
      recipes::names0(nrow(grid), "Model"),
      (submodel_id - num_submodels[mod_iter]):submodel_id
    )

    grid_info_model_iter <- dplyr::filter(grid_info_model, .iter_model == mod_iter)
    grid_model_iter <- dplyr::select(grid_info_model_iter, dplyr::all_of(model_param_names))
    submodels_iter <- dplyr::pull(grid_info_model_iter, ".submodels")[[1L]]

    workflow <- finalize_workflow_spec(workflow, grid_model_iter)

    workflow <- catch_and_log_fit(
      .fit_model(workflow, control_workflow),
      control,
      split,
      mod_msg,
      notes = .notes
    )

    # check for parsnip level and model level failure
    if (is_failure(workflow) || is_failure(workflow$fit$fit$fit)) {
      next
    }

    workflow <- .fit_finalize(workflow)

    # Extract names from the mold
    outcome_names <- outcome_names(workflow)
    all_outcome_names <- append_outcome_names(all_outcome_names, outcome_names)

    extracted <- append_extracts(
      extracted,
      workflow,
      grid_model_iter,
      split,
      control,
      mod_id
    )

    pred_msg <- paste(mod_msg, "(predictions)")

    tmp_pred <- catch_and_log(
      predict_model(split, workflow, grid_model_iter, metrics, submodels_iter),
      control,
      split,
      pred_msg,
      bad_only = TRUE,
      notes = .notes
    )

    # check for prediction level failure
    if (is_failure(tmp_pred)) {
      next
    }

    metric_est <- append_metrics(
      collection = metric_est,
      predictions = tmp_pred,
      metrics = metrics,
      param_names = param_names,
      outcome_name = outcome_names,
      event_level = event_level,
      split = split,
      .config = mod_id
    )

    config_id <- extract_config(param_names, metric_est)
    pred_vals <- append_predictions(pred_vals, tmp_pred, split, control, config_id)
  } # end model loop

  list(
    .metrics = metric_est,
    .extracts = extracted,
    .predictions = pred_vals,
    .all_outcome_names = all_outcome_names,
    .notes = .notes
  )
}

# ------------------------------------------------------------------------------

iter_model_and_recipe <- function(rs_iter,
                                  resamples,
                                  grid_info,
                                  grid,
                                  workflow,
                                  metrics,
                                  control) {
  load_pkgs(workflow)
  load_namespace(control$pkgs)
  set.seed(resamples$.seed[[rs_iter]])

  control_parsnip <- parsnip::control_parsnip(verbosity = 0, catch = TRUE)
  control_workflow <- control_workflow(control_parsnip = control_parsnip)

  event_level <- control$event_level

  metric_est <- NULL
  extracted <- NULL
  pred_vals <- NULL
  all_outcome_names <- list()
  .notes <- NULL

  params <- dials::parameters(workflow)
  model_params <- dplyr::filter(params, source == "model_spec")
  recipe_params <- dplyr::filter(params, source == "recipe")

  param_names <- dplyr::pull(params, "id")
  model_param_names <- dplyr::pull(model_params, "id")
  recipe_param_names <- dplyr::pull(recipe_params, "id")

  split <- resamples$splits[[rs_iter]]
  training <- rsample::analysis(split)

  # --------------------------------------------------------------------------

  num_rec <- nrow(grid_info)
  original_workflow <- workflow

  for (rec_iter in 1:num_rec) {
    workflow <- original_workflow

    rec_msg <- paste0("recipe ", format(1:num_rec)[rec_iter], "/", num_rec)
    rec_id <- vec_slice(recipes::names0(num_rec, "Recipe"), rec_iter)

    grid_info_recipe_iter <- dplyr::filter(grid_info, .iter_recipe == rec_iter)
    grid_recipe_iter <- dplyr::select(grid_info_recipe_iter, dplyr::all_of(recipe_param_names))

    workflow <- finalize_workflow_recipe(workflow, grid_recipe_iter)

    workflow <- catch_and_log(
      .fit_pre(workflow, training),
      control,
      split,
      rec_msg,
      notes = .notes
    )

    if (is_failure(workflow)) {
      next
    }

    # --------------------------------------------------------------------------

    grid_info_model <- grid_info_recipe_iter[["data"]][[1]]

    num_mod <- nrow(grid_info_model)

    num_submodels <- grid_info_model %>%
      unnest(.submodels, keep_empty = TRUE) %>%
      pull(.submodels) %>%
      lengths()

    # ------------------------------------------------------------------------

    original_prepped_workflow <- workflow

    for (mod_iter in 1:num_mod) {
      workflow <- original_prepped_workflow

      submodel_id <- mod_iter + sum(vec_slice(num_submodels, 1:mod_iter))
      mod_msg <- paste0(rec_msg, ", model ", format(1:num_mod)[mod_iter], "/", num_mod)
      mod_id <- vec_slice(
        recipes::names0(nrow(grid), "Model"),
        (submodel_id - num_submodels[mod_iter]):submodel_id
      )
      mod_id <- paste0(rec_id, "_", mod_id)

      grid_info_model_iter <- dplyr::filter(grid_info_model, .iter_model == mod_iter)
      grid_model_iter <- dplyr::select(grid_info_model_iter, dplyr::all_of(model_param_names))
      submodels_iter <- dplyr::pull(grid_info_model_iter, ".submodels")[[1]]

      workflow <- finalize_workflow_spec(workflow, grid_model_iter)

      workflow <- catch_and_log_fit(
        .fit_model(workflow, control_workflow),
        control,
        split,
        mod_msg,
        notes = .notes
      )

      # check for parsnip level and model level failure
      if (is_failure(workflow) || is_failure(workflow$fit$fit$fit)) {
        next
      }

      workflow <- .fit_finalize(workflow)

      # Extract names from the mold
      outcome_names <- outcome_names(workflow)
      all_outcome_names <- append_outcome_names(all_outcome_names, outcome_names)

      grid_iter <- dplyr::bind_cols(grid_recipe_iter, grid_model_iter)

      extracted <- append_extracts(
        extracted,
        workflow,
        grid_iter,
        split,
        control,
        mod_id
      )

      pred_msg <- paste(mod_msg, "(predictions)")

      tmp_pred <- catch_and_log(
        predict_model(split, workflow, grid_iter, metrics, submodels_iter),
        control,
        split,
        pred_msg,
        bad_only = TRUE,
        notes = .notes
      )

      # check for prediction level failure
      if (is_failure(tmp_pred)) {
        next
      }

      metric_est <- append_metrics(
        collection = metric_est,
        predictions = tmp_pred,
        metrics = metrics,
        param_names = param_names,
        outcome_name = outcome_names,
        event_level = event_level,
        split = split,
        .config = mod_id
      )

      config_id <- extract_config(param_names, metric_est)
      pred_vals <- append_predictions(pred_vals, tmp_pred, split, control, config_id)
    } # end model loop
  } # end recipe loop

  list(
    .metrics = metric_est,
    .extracts = extracted,
    .predictions = pred_vals,
    .all_outcome_names = all_outcome_names,
    .notes = .notes
  )
}

# ------------------------------------------------------------------------------

iter_recipe <- function(rs_iter,
                        resamples,
                        grid_info,
                        grid,
                        workflow,
                        metrics,
                        control) {
  load_pkgs(workflow)
  load_namespace(control$pkgs)
  set.seed(resamples$.seed[[rs_iter]])

  control_parsnip <- parsnip::control_parsnip(verbosity = 0, catch = TRUE)
  control_workflow <- control_workflow(control_parsnip = control_parsnip)

  event_level <- control$event_level

  metric_est <- NULL
  extracted <- NULL
  pred_vals <- NULL
  all_outcome_names <- list()
  .notes <- NULL

  params <- dials::parameters(workflow)
  recipe_params <- dplyr::filter(params, source == "recipe")

  param_names <- dplyr::pull(params, "id")
  recipe_param_names <- dplyr::pull(recipe_params, "id")

  split <- resamples$splits[[rs_iter]]
  training <- rsample::analysis(split)

  num_rec <- nrow(grid_info)
  original_workflow <- workflow

  for (rec_iter in 1:num_rec) {
    workflow <- original_workflow

    rec_msg <- paste0("recipe ", format(1:num_rec)[rec_iter], "/", num_rec)
    mod_msg <- paste0(rec_msg, ", model 1/1")
    rec_id <- vec_slice(recipes::names0(num_rec, "Recipe"), rec_iter)

    grid_info_recipe_iter <- dplyr::filter(grid_info, .iter_recipe == rec_iter)
    grid_recipe_iter <- dplyr::select(grid_info_recipe_iter, dplyr::all_of(recipe_param_names))

    workflow <- finalize_workflow_recipe(workflow, grid_recipe_iter)

    workflow <- catch_and_log(
      .fit_pre(workflow, training),
      control,
      split,
      rec_msg,
      notes = .notes
    )

    # check for recipe failure
    if (is_failure(workflow)) {
      next
    }

    workflow <- catch_and_log_fit(
      .fit_model(workflow, control_workflow),
      control,
      split,
      mod_msg,
      notes = .notes
    )

    # check for parsnip level and model level failure
    if (is_failure(workflow) || is_failure(workflow$fit$fit$fit)) {
      next
    }

    workflow <- .fit_finalize(workflow)

    # Extract names from the mold
    outcome_names <- outcome_names(workflow)
    all_outcome_names <- append_outcome_names(all_outcome_names, outcome_names)

    extracted <- append_extracts(
      extracted,
      workflow,
      grid_recipe_iter,
      split,
      control,
      rec_id
    )

    pred_msg <- paste(mod_msg, "(predictions)")

    tmp_pred <- catch_and_log(
      predict_model(split, workflow, grid_recipe_iter, metrics),
      control,
      split,
      pred_msg,
      bad_only = TRUE,
      notes = .notes
    )

    # check for prediction level failure
    if (is_failure(tmp_pred)) {
      next
    }

    metric_est <- append_metrics(
      collection = metric_est,
      predictions = tmp_pred,
      metrics = metrics,
      param_names = param_names,
      outcome_name = outcome_names,
      event_level = event_level,
      split = split,
      .config = rec_id
    )

    config_id <- extract_config(param_names, metric_est)
    pred_vals <- append_predictions(pred_vals, tmp_pred, split, control, config_id)
  } # end recipe loop

  list(
    .metrics = metric_est,
    .extracts = extracted,
    .predictions = pred_vals,
    .all_outcome_names = all_outcome_names,
    .notes = .notes
  )
}

# ------------------------------------------------------------------------------

super_safely_iterate <- function(fn) {
  purrr::partial(.f = super_safely_iterate_impl, fn = fn)
}

super_safely_iterate_impl <- function(fn,
                                      rs_iter,
                                      resamples,
                                      grid_info,
                                      grid,
                                      workflow,
                                      metrics,
                                      control) {
  safely_iterate <- super_safely(fn)

  # Differentiate [fit_resamples()] from [tune_grid()]
  if (is.null(grid)) {
    result <- safely_iterate(rs_iter, resamples, workflow, metrics, control)
  } else {
    result <- safely_iterate(rs_iter, resamples, grid_info, grid, workflow, metrics, control)
  }

  error <- result$error
  warnings <- result$warnings
  result <- result$result

  # No problems
  if (is.null(error) && length(warnings) == 0L) {
    return(result)
  }

  # No errors, but we might have warning notes
  if (is.null(error)) {
    res <- result
    notes <- result$.notes
  } else {
    res <- error
    notes <- NULL
  }

  problems <- list(res = res, signals = warnings)

  split <- resamples$splits[[rs_iter]]

  notes <- log_problems(notes, control, split, "internal", problems)

  # Need an output template
  if (!is.null(error)) {
    result <- list(
      .metrics = NULL,
      .extracts = NULL,
      .predictions = NULL,
      .all_outcome_names = list(),
      .notes = NULL
    )
  }

  # Update with new notes
  result[[".notes"]] <- notes

  result
}

# Capture any errors, and all warnings
# If a warning is caught, we store it for later and invoke a restart
# If an error is caught, we immediately exit
# All other messages are still passed through
super_safely <- function(fn) {
  warnings <- list()

  # Construct a try()-error to be compatible with `log_problems()`
  handle_error <- function(e) {
    e <- structure(e$message, class = "try-error", condition = e)
    list(result = NULL, error = e, warnings = warnings)
  }

  handle_warning <- function(w) {
    warnings <<- c(warnings, list(w))
    rlang::cnd_muffle(w)
  }

  safe_fn <- function(...) {
    withCallingHandlers(
      expr = tryCatch(
        expr = list(result = fn(...), error = NULL, warnings = warnings),
        error = handle_error
      ),
      warning = handle_warning
    )
  }

  safe_fn
}

# ----------------------------------------------------------------------------

is_failure <- function(x) {
  inherits(x, "try-error")
}
