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

  results <-
    foreach::foreach(rs_iter = 1:B, .packages = load_pkgs, .errorhandling = "pass") %op%
    fn_iter_safely(rs_iter, resamples, grid, workflow, metrics, control)

  resamples <- pull_metrics(resamples, results, control)
  resamples <- pull_notes(resamples, results, control)
  resamples <- pull_extracts(resamples, results, control)
  resamples <- pull_predictions(resamples, results, control)
  resamples <- pull_all_outcome_names(resamples, results)

  resamples
}

# ------------------------------------------------------------------------------

iter_model_with_preprocessor <- function(rs_iter, resamples, grid, workflow, metrics, control) {
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
  param_names <- params[["id"]]

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

  # Determine the _minimal_ number of models to fit in order to get
  # predictions on all models.
  model_spec <- workflows::pull_workflow_spec(workflow)
  mod_grid_vals <- min_grid(model_spec, grid)

  num_mod <- nrow(mod_grid_vals)
  num_submodels <- mod_grid_vals %>%
    unnest(.submodels, keep_empty = TRUE) %>%
    pull(.submodels) %>%
    map_int(length)

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

    param_val <- mod_grid_vals[mod_iter, ]
    workflow <- finalize_workflow_spec(workflow, param_val)

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
      param_val,
      split,
      control,
      mod_id
    )

    pred_msg <- paste(mod_msg, "(predictions)")

    tmp_pred <- catch_and_log(
      predict_model(split, workflow, param_val, metrics),
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

iter_model_and_recipe <- function(rs_iter, resamples, grid, workflow, metrics, control) {
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
  param_names <- params[["id"]]

  model_param <- params %>%
    dplyr::filter(source == "model_spec") %>%
    dplyr::pull(id)
  rec_param <- params %>%
    dplyr::filter(source == "recipe") %>%
    dplyr::pull(id)

  if (tidyr_new_interface()) {
    rec_grid <- tidyr::nest(tibble::as_tibble(grid), data = dplyr::one_of(model_param))
  } else {
    rec_grid <- tidyr::nest(grid, !!!model_param)
  }

  split <- resamples$splits[[rs_iter]]
  training <- rsample::analysis(split)

  # --------------------------------------------------------------------------

  num_rec <- nrow(rec_grid)
  original_workflow <- workflow

  for (rec_iter in 1:num_rec) {
    workflow <- original_workflow

    rec_msg <- paste0("recipe ", format(1:num_rec)[rec_iter], "/", num_rec)
    rec_id <- vec_slice(recipes::names0(num_rec, "Recipe"), rec_iter)

    # Current recipe parameters only
    rec_grid_vals <-
      rec_grid %>%
      dplyr::slice(rec_iter) %>%
      dplyr::select(-data)

    workflow <- finalize_workflow_recipe(workflow, rec_grid_vals)

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

    # All model tune parameters associated with the current recipe parameters
    mod_grid_vals <-
      rec_grid %>%
      dplyr::slice(rec_iter) %>%
      dplyr::select(-one_of(rec_param)) %>%
      tidyr::unnest(cols = dplyr::one_of("data"))

    # Determine the _minimal_ number of models to fit in order to get
    # predictions on all models.
    model_spec <- workflows::pull_workflow_spec(workflow)
    mod_grid_vals <- min_grid(model_spec, mod_grid_vals)

    num_submodels <- mod_grid_vals %>%
      unnest(.submodels, keep_empty = TRUE) %>%
      pull(.submodels) %>%
      map_int(length)

    num_mod <- nrow(mod_grid_vals)

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

      fixed_param <- mod_grid_vals %>% dplyr::slice(mod_iter) %>% dplyr::select(-.submodels)
      submd_param <- mod_grid_vals %>% dplyr::slice(mod_iter) %>% dplyr::select(.submodels)
      submd_param <- submd_param$.submodels[[1]]

      workflow <- finalize_workflow_spec(workflow, fixed_param)

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

      all_param <- dplyr::bind_cols(rec_grid_vals, mod_grid_vals[mod_iter, ])

      extracted <- append_extracts(
        extracted,
        workflow,
        all_param,
        split,
        control,
        mod_id
      )

      pred_msg <- paste(mod_msg, "(predictions)")

      tmp_pred <- catch_and_log(
        predict_model(split, workflow, all_param, metrics),
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

iter_recipe <- function(rs_iter, resamples, grid, workflow, metrics, control) {
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
  param_names <- params[["id"]]

  split <- resamples$splits[[rs_iter]]
  training <- rsample::analysis(split)

  num_rec <- nrow(grid)
  original_workflow <- workflow

  for (param_iter in 1:num_rec) {
    workflow <- original_workflow

    rec_msg <- paste0("recipe ", format(1:num_rec)[param_iter], "/", num_rec)
    mod_msg <- paste0(rec_msg, ", model 1/1")
    rec_id <- vec_slice(recipes::names0(num_rec, "Recipe"), param_iter)

    param_vals <- grid[param_iter, ]
    workflow <- finalize_workflow_recipe(workflow, param_vals)

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
      param_vals,
      split,
      control,
      rec_id
    )

    pred_msg <- paste(mod_msg, "(predictions)")

    tmp_pred <- catch_and_log(
      predict_model(split, workflow, param_vals, metrics),
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

super_safely_iterate_impl <- function(fn, rs_iter, resamples, grid, workflow, metrics, control) {
  safely_iterate <- super_safely(fn)

  # Differentiate [fit_resamples()] from [tune_grid()]
  if (is.null(grid)) {
    result <- safely_iterate(rs_iter, resamples, workflow, metrics, control)
  } else {
    result <- safely_iterate(rs_iter, resamples, grid, workflow, metrics, control)
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
