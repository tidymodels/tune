tune_nothing_with_recipe <- function(resamples, grid, workflow, metrics, control)  {
  resample_with_recipe(resamples, workflow, metrics, control)
}

tune_nothing_with_formula <- function(resamples, grid, workflow, metrics, control)  {
  resample_with_formula(resamples, workflow, metrics, control)
}

# ------------------------------------------------------------------------------

iter_rec_and_mod <- function(rs_iter, resamples, grid, workflow, metrics, control) {
  load_pkgs(workflow)
  load_namespace(control$pkgs)

  control_parsnip <- parsnip::control_parsnip(verbosity = 0, catch = TRUE)
  control_workflow <- control_workflow(control_parsnip = control_parsnip)

  metric_est <- NULL
  extracted <- NULL
  pred_vals <- NULL
  .notes <- NULL

  split <- resamples$splits[[rs_iter]]

  model_param <-
    dials::parameters(workflow) %>%
    dplyr::filter(source == "model_spec") %>%
    dplyr::pull(id)
  rec_param <-
    dials::parameters(workflow) %>%
    dplyr::filter(source == "recipe") %>%
    dplyr::pull(id)

  if (tidyr_new_interface()) {
    rec_grid <- tidyr::nest(tibble::as_tibble(grid), data = dplyr::one_of(model_param))
  } else {
    rec_grid <- tidyr::nest(grid, !!!model_param)
  }

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

    workflow <- catch_and_log(
      train_recipe(split, workflow, rec_grid_vals),
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
    num_submodels <- nrow(mod_grid_vals)
    mod_grid_vals <- workflows::pull_workflow_spec(workflow) %>% min_grid(mod_grid_vals)
    num_mod <- nrow(mod_grid_vals)

    # ------------------------------------------------------------------------

    original_prepped_workflow <- workflow

    for (mod_iter in 1:num_mod) {
      workflow <- original_prepped_workflow

      fixed_param <- mod_grid_vals %>% dplyr::slice(mod_iter) %>% dplyr::select(-.submodels)
      submd_param <- mod_grid_vals %>% dplyr::slice(mod_iter) %>% dplyr::select(.submodels)
      submd_param <- submd_param$.submodels[[1]]

      submd_id <- num_submodels / num_mod * mod_iter
      mod_msg <- paste0(rec_msg, ", model ", format(1:num_mod)[mod_iter], "/", num_mod)
      mod_id <- paste0(rec_id, "_",
                       vec_slice(
                         recipes::names0(num_submodels, "Model"),
                         (submd_id - num_submodels / num_mod + 1):submd_id
                       ))

      workflow <-
        catch_and_log_fit(
          train_model(workflow, fixed_param, control = control_workflow),
          control,
          split,
          mod_msg,
          notes = .notes
        )

      # check for parsnip level and model level failure
      if (is_failure(workflow) || is_failure(workflow$fit$fit$fit)) {
        next
      }

      all_param <- dplyr::bind_cols(rec_grid_vals, mod_grid_vals[mod_iter, ])

      extracted <-
        append_extracts(extracted,
                        workflow,
                        all_param,
                        split,
                        control,
                        mod_id)

      tmp_pred <-
        catch_and_log(
          predict_model(split, workflow, all_param, metrics),
          control,
          split,
          paste(mod_msg, "(predictions)"),
          bad_only = TRUE,
          notes = .notes
        )

      # check for prediction level failure
      if (is_failure(tmp_pred)) {
        next
      }

      metric_est <- append_metrics(metric_est, tmp_pred, workflow, metrics, split, mod_id)
      pred_vals <- append_predictions(pred_vals, tmp_pred, split, control, mod_id)
    } # end model loop

  } # end recipe loop

  list(.metrics = metric_est, .extracts = extracted, .predictions = pred_vals, .notes = .notes)
}

tune_rec_and_mod <- function(resamples, grid, workflow, metrics, control) {
  B <- nrow(resamples)

  `%op%` <- get_operator(control$allow_par, workflow)

  lab_names <- names(labels(resamples$splits[[1]]))

  safely_iter_rec_and_mod <- super_safely_iterate(iter_rec_and_mod)

  load_pkgs <- c(control$pkgs, "recipes", "parsnip", "tune")

  results <-
    foreach::foreach(rs_iter = 1:B, .packages = load_pkgs, .errorhandling = "pass") %op%
    safely_iter_rec_and_mod(rs_iter, resamples, grid, workflow, metrics, control)

  resamples <- pull_metrics(resamples, results, control)
  resamples <- pull_notes(resamples, results, control)
  resamples <- pull_extracts(resamples, results, control)
  resamples <- pull_predictions(resamples, results, control)

  resamples
}

# ------------------------------------------------------------------------------

iter_rec <- function(rs_iter, resamples, grid, workflow, metrics, control) {
  load_pkgs(workflow)
  load_namespace(control$pkgs)

  control_parsnip <- parsnip::control_parsnip(verbosity = 0, catch = TRUE)
  control_workflow <- control_workflow(control_parsnip = control_parsnip)

  split <- resamples$splits[[rs_iter]]
  metric_est <- NULL
  extracted <- NULL
  pred_vals <- NULL
  .notes <- NULL

  num_rec <- nrow(grid)
  original_workflow <- workflow

  for (param_iter in 1:num_rec) {
    workflow <- original_workflow

    param_vals <- grid[param_iter, ]
    rec_msg <- paste0("recipe ", format(1:num_rec)[param_iter], "/", num_rec)
    mod_msg <- paste0(rec_msg, ", model 1/1")
    rec_id <- vec_slice(recipes::names0(num_rec, "Recipe"), param_iter)

    workflow <- catch_and_log(
      train_recipe(split, workflow, param_vals),
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
      train_model(workflow, NULL, control = control_workflow),
      control,
      split,
      mod_msg,
      notes = .notes
    )

    # check for parsnip level and model level failure
    if (is_failure(workflow) || is_failure(workflow$fit$fit$fit)) {
      next
    }

    extracted <-
      append_extracts(
        extracted,
        workflow,
        grid[param_iter, ],
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

    metric_est <- append_metrics(metric_est, tmp_pred, workflow, metrics, split, rec_id)
    pred_vals <- append_predictions(pred_vals, tmp_pred, split, control, rec_id)
  } # recipe parameters

  list(.metrics = metric_est, .extracts = extracted, .predictions = pred_vals, .notes = .notes)

}

tune_rec <- function(resamples, grid, workflow, metrics, control) {
  B <- nrow(resamples)

  `%op%` <- get_operator(control$allow_par, workflow)

  safely_iter_rec <- super_safely_iterate(iter_rec)

  load_pkgs <- c(control$pkgs, "recipes", "parsnip", "tune")

  results <-
    foreach::foreach(rs_iter = 1:B, .packages = load_pkgs, .errorhandling = "pass") %op%
    safely_iter_rec(rs_iter, resamples, grid, workflow, metrics, control)

  resamples <- pull_metrics(resamples, results, control)
  resamples <- pull_notes(resamples, results, control)
  resamples <- pull_extracts(resamples, results, control)
  resamples <- pull_predictions(resamples, results, control)

  resamples
}


# ------------------------------------------------------------------------------

tune_mod_with_recipe <- function(resamples, grid, workflow, metrics, control) {
  B <- nrow(resamples)

  `%op%` <- get_operator(control$allow_par, workflow)

  safely_iter_mod_with_recipe <- super_safely_iterate(iter_mod_with_recipe)

  load_pkgs <- c(control$pkgs, "recipes", "parsnip", "tune")

  results <-
    foreach::foreach(rs_iter = 1:B, .packages = load_pkgs, .errorhandling = "pass") %op%
    safely_iter_mod_with_recipe(rs_iter, resamples, grid, workflow, metrics, control)

  resamples <- pull_metrics(resamples, results, control)
  resamples <- pull_notes(resamples, results, control)
  resamples <- pull_extracts(resamples, results, control)
  resamples <- pull_predictions(resamples, results, control)

  resamples
}

iter_mod_with_recipe <- function(rs_iter, resamples, grid, workflow, metrics, control) {
  load_pkgs(workflow)
  load_namespace(control$pkgs)

  control_parsnip <- parsnip::control_parsnip(verbosity = 0, catch = TRUE)
  control_workflow <- control_workflow(control_parsnip = control_parsnip)

  split <- resamples$splits[[rs_iter]]
  metric_est <- NULL
  extracted <- NULL
  pred_vals <- NULL
  .notes <- NULL

  # ----------------------------------------------------------------------------

  workflow <- catch_and_log(
    train_recipe(split, workflow, NULL),
    control,
    split,
    "recipe",
    notes = .notes
  )

  # check for recipe failure
  if (is_failure(workflow)) {
    out <- list(
      .metrics = metric_est,
      .extracts = extracted,
      .predictions = pred_vals,
      .notes = .notes
    )

    return(out)
  }

  # ----------------------------------------------------------------------------

  # Determine the _minimal_ number of models to fit in order to get
  # predictions on all models.
  mod_grid_vals <- workflows::pull_workflow_spec(workflow) %>% min_grid(grid)

  num_mod <- nrow(mod_grid_vals)
  num_submodels <- nrow(grid)
  original_workflow <- workflow

  for (mod_iter in 1:num_mod) {
    workflow <- original_workflow

    param_val <- mod_grid_vals[mod_iter, ]
    submodel_id <- num_submodels / num_mod * mod_iter
    mod_msg <- paste0("model ", format(1:num_mod)[mod_iter], "/", num_mod)
    mod_id <- vec_slice(
      recipes::names0(num_submodels, "Model"),
      (submodel_id - num_submodels / num_mod + 1):submodel_id
    )

    workflow <- catch_and_log_fit(
      train_model(workflow, mod_grid_vals[mod_iter,], control_workflow),
      control,
      split,
      mod_msg,
      notes = .notes
    )

    # check for parsnip level and model level failure
    if (is_failure(workflow) || is_failure(workflow$fit$fit$fit)) {
      next
    }

    extracted <-
      append_extracts(
        extracted,
        workflow,
        mod_grid_vals[mod_iter, ],
        split,
        control,
        mod_id
      )

    tmp_pred <- catch_and_log(
      predict_model(split, workflow, mod_grid_vals[mod_iter,], metrics),
      control,
      split,
      paste(mod_msg, "(predictions)"),
      bad_only = TRUE,
      notes = .notes
    )

    # check for prediction level failure
    if (is_failure(tmp_pred)) {
      next
    }

    metric_est  <- append_metrics(metric_est, tmp_pred, workflow, metrics, split, mod_id)
    pred_vals <- append_predictions(pred_vals, tmp_pred, split, control, mod_id)
  } # end model loop

  list(.metrics = metric_est, .extracts = extracted, .predictions = pred_vals, .notes = .notes)
}

# ------------------------------------------------------------------------------

tune_mod_with_formula <- function(resamples, grid, workflow, metrics, control) {
  B <- nrow(resamples)

  `%op%` <- get_operator(control$allow_par, workflow)

  safely_iter_mod_with_formula <- super_safely_iterate(iter_mod_with_formula)

  load_pkgs <- c(control$pkgs, "parsnip", "tune")

  results <-
    foreach::foreach(rs_iter = 1:B, .packages = load_pkgs, .errorhandling = "pass") %op%
    safely_iter_mod_with_formula(rs_iter, resamples, grid, workflow, metrics, control)

  resamples <- pull_metrics(resamples, results, control)
  resamples <- pull_notes(resamples, results, control)
  resamples <- pull_extracts(resamples, results, control)
  resamples <- pull_predictions(resamples, results, control)

  resamples
}

iter_mod_with_formula <- function(rs_iter, resamples, grid, workflow, metrics, control) {
  load_pkgs(workflow)
  load_namespace(control$pkgs)

  control_parsnip <- parsnip::control_parsnip(verbosity = 0, catch = TRUE)
  control_workflow <- control_workflow(control_parsnip = control_parsnip)

  split <- resamples$splits[[rs_iter]]
  metric_est <- NULL
  extracted <- NULL
  pred_vals <- NULL
  .notes <- NULL

  # ----------------------------------------------------------------------------

  workflow <- catch_and_log(
    train_formula(split, workflow),
    control,
    split,
    "formula",
    notes = .notes
  )

  # check for formula failure
  if (is_failure(workflow)) {
    out <- list(
      .metrics = metric_est,
      .extracts = extracted,
      .predictions = pred_vals,
      .notes = .notes
    )

    return(out)
  }

  # ----------------------------------------------------------------------------

  # Determine the _minimal_ number of models to fit in order to get
  # predictions on all models.
  mod_grid_vals <- workflows::pull_workflow_spec(workflow) %>% min_grid(grid)

  num_mod <- nrow(mod_grid_vals)
  num_submodels <- nrow(grid)
  original_workflow <- workflow

  for (mod_iter in 1:num_mod) {
    workflow <- original_workflow

    param_val <- mod_grid_vals[mod_iter, ]
    submodel_id <- num_submodels / num_mod * mod_iter
    mod_msg <- paste0("model ", format(1:num_mod)[mod_iter], "/", num_mod)
    mod_id <- vec_slice(
      recipes::names0(num_submodels, "Model"),
      (submodel_id - num_submodels / num_mod + 1):submodel_id
      )

    workflow <- catch_and_log_fit(
      train_model(workflow, param_val, control = control_workflow),
      control,
      split,
      mod_msg,
      notes = .notes
    )

    # check for parsnip level and model level failure
    if (is_failure(workflow) || is_failure(workflow$fit$fit$fit)) {
      next
    }

    extracted <-
      append_extracts(extracted,
                      workflow,
                      param_val,
                      split,
                      control,
                      mod_id)

    pred_msg <- paste(mod_msg, "(predictions)")

    tmp_pred <-
      catch_and_log(
        predict_model(split, workflow, param_val, metrics),
        control,
        split,
        mod_msg,
        notes = .notes
      )

    # check for prediction level failure
    if (is_failure(tmp_pred)) {
      next
    }

    metric_est  <- append_metrics(metric_est, tmp_pred, workflow, metrics, split, mod_id)
    pred_vals <- append_predictions(pred_vals, tmp_pred, split, control, mod_id)
  } # end model loop

  list(.metrics = metric_est, .extracts = extracted, .predictions = pred_vals, .notes = .notes)
}

# ----------------------------------------------------------------------------

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
