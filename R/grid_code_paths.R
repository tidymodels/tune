tune_nothing_with_recipe <- function(resamples, grid, object, metrics, ctrl)  {
  resample_with_recipe(resamples, object, metrics, ctrl)
}

tune_nothing_with_formula <- function(resamples, grid, object, metrics, ctrl)  {
  resample_with_formula(resamples, object, metrics, ctrl)
}

# ------------------------------------------------------------------------------

iter_rec_and_mod <- function(rs_iter, resamples, grid, object, metrics, ctrl) {
  load_pkgs(object)
  load_namespace(ctrl$pkgs)
  fit_ctrl <- parsnip::fit_control(verbosity = 0, catch = TRUE)

  metric_est <- NULL
  extracted <- NULL
  pred_vals <- NULL
  .notes <- NULL

  split <- resamples$splits[[rs_iter]]

  model_param <-
    dials::parameters(object) %>%
    dplyr::filter(source == "model_spec") %>%
    dplyr::pull(id)
  rec_param <-
    dials::parameters(object) %>%
    dplyr::filter(source == "recipe") %>%
    dplyr::pull(id)

  if (tidyr_new_interface()) {
    rec_grid <- tidyr::nest(tibble::as_tibble(grid), data = dplyr::one_of(model_param))
  } else {
    rec_grid <- tidyr::nest(grid, !!!model_param)
  }

  # --------------------------------------------------------------------------

  num_rec <- nrow(rec_grid)
  for (rec_iter in 1:num_rec) {
    rec_msg <- paste0("recipe ", format(1:num_rec)[rec_iter], "/", num_rec)

    # Current recipe parameters only
    rec_grid_vals <-
      rec_grid %>%
      dplyr::slice(rec_iter) %>%
      dplyr::select(-data)

    tmp_rec <- catch_and_log(train_recipe(split, object, rec_grid_vals), ctrl, split, rec_msg, notes = .notes)

    if (is_failure(tmp_rec)) {
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
    mod_grid_vals <- get_wflow_model(object) %>% min_grid(mod_grid_vals)

    # ------------------------------------------------------------------------

    num_mod <- nrow(mod_grid_vals)
    for (mod_iter in 1:num_mod) {
      mod_msg <- paste0(rec_msg, ", model ", format(1:num_mod)[mod_iter], "/", num_mod)

      fixed_param <- mod_grid_vals %>% dplyr::slice(mod_iter) %>% dplyr::select(-.submodels)
      submd_param <- mod_grid_vals %>% dplyr::slice(mod_iter) %>% dplyr::select(.submodels)
      submd_param <- submd_param$.submodels[[1]]

      tmp_fit <-
        catch_and_log(
          train_model_from_recipe(object, tmp_rec, fixed_param, control = fit_ctrl),
          ctrl,
          split,
          mod_msg,
          notes = .notes
        )

      # check for parsnip level and model level failure
      if (is_failure(tmp_fit) || is_failure(tmp_fit$fit)) {
        next
      }

      all_param <- dplyr::bind_cols(rec_grid_vals, mod_grid_vals[mod_iter, ])

      extracted <- append_extracts(extracted, tmp_rec, tmp_fit$fit, all_param, split, ctrl)

      tmp_pred <-
        catch_and_log(
          predict_model_from_recipe(split, tmp_fit, tmp_rec, all_param, metrics),
          ctrl,
          split,
          paste(mod_msg, "(predictions)"),
          bad_only = TRUE,
          notes = .notes
        )

      # check for prediction level failure
      if (is_failure(tmp_pred)) {
        next
      }

      metric_est <- append_metrics(metric_est, tmp_pred, object, metrics, split)
      pred_vals <- append_predictions(pred_vals, tmp_pred, split, ctrl)
    } # end model loop

  } # end recipe loop

  list(.metrics = metric_est, .extracts = extracted, .predictions = pred_vals, .notes = .notes)
}

tune_rec_and_mod <- function(resamples, grid, object, metrics, ctrl) {
  B <- nrow(resamples)

  `%op%` <- get_operator(ctrl$allow_par, object)

  lab_names <- names(labels(resamples$splits[[1]]))

  safely_iter_rec_and_mod <- super_safely_iterate(iter_rec_and_mod)

  results <-
    foreach::foreach(rs_iter = 1:B, .packages = "tune", .errorhandling = "pass") %op%
    safely_iter_rec_and_mod(rs_iter, resamples, grid, object, metrics, ctrl)

  resamples <- pull_metrics(resamples, results, ctrl)
  resamples <- pull_notes(resamples, results, ctrl)
  resamples <- pull_extracts(resamples, results, ctrl)
  resamples <- pull_predictions(resamples, results, ctrl)

  resamples
}

# ------------------------------------------------------------------------------

iter_rec <- function(rs_iter, resamples, grid, object, metrics, ctrl) {
  load_pkgs(object)
  load_namespace(ctrl$pkgs)
  fit_ctrl <- parsnip::fit_control(verbosity = 0, catch = TRUE)

  split <- resamples$splits[[rs_iter]]
  metric_est <- NULL
  extracted <- NULL
  pred_vals <- NULL
  .notes <- NULL

  num_rec <- nrow(grid)

  for (param_iter in 1:num_rec) {
    param_vals <- grid[param_iter, ]
    rec_msg <- paste0("recipe ", format(1:num_rec)[param_iter], "/", num_rec)
    mod_msg <- paste0(rec_msg, ", model 1/1")

    tmp_rec <- catch_and_log(train_recipe(split, object, param_vals), ctrl, split, rec_msg, notes = .notes)

    # check for recipe failure
    if (is_failure(tmp_rec)) {
      next
    }

    tmp_fit <-
      catch_and_log(
        train_model_from_recipe(object, tmp_rec, NULL, control = fit_ctrl),
        ctrl,
        split,
        mod_msg,
        notes = .notes
      )

    # check for parsnip level and model level failure
    if (is_failure(tmp_fit) || is_failure(tmp_fit$fit)) {
      next
    }

    extracted <- append_extracts(extracted, tmp_rec, tmp_fit$fit, grid[param_iter, ], split, ctrl)

    pred_msg <- paste(mod_msg, "(predictions)")

    tmp_pred <-
      catch_and_log(
        predict_model_from_recipe(split, tmp_fit, tmp_rec, param_vals, metrics),
        ctrl,
        split,
        pred_msg,
        bad_only = TRUE,
        notes = .notes
      )

    # check for prediction level failure
    if (is_failure(tmp_pred)) {
      next
    }

    metric_est <- append_metrics(metric_est, tmp_pred, object, metrics, split)
    pred_vals <- append_predictions(pred_vals, tmp_pred, split, ctrl)
  } # recipe parameters

  list(.metrics = metric_est, .extracts = extracted, .predictions = pred_vals, .notes = .notes)

}

tune_rec <- function(resamples, grid, object, metrics, ctrl) {
  B <- nrow(resamples)

  `%op%` <- get_operator(ctrl$allow_par, object)

  safely_iter_rec <- super_safely_iterate(iter_rec)

  results <-
    foreach::foreach(rs_iter = 1:B, .packages = "tune", .errorhandling = "pass") %op%
    safely_iter_rec(rs_iter, resamples, grid, object, metrics, ctrl)

  resamples <- pull_metrics(resamples, results, ctrl)
  resamples <- pull_notes(resamples, results, ctrl)
  resamples <- pull_extracts(resamples, results, ctrl)
  resamples <- pull_predictions(resamples, results, ctrl)

  resamples
}


# ------------------------------------------------------------------------------

tune_mod_with_recipe <- function(resamples, grid, object, metrics, ctrl) {
  B <- nrow(resamples)

  `%op%` <- get_operator(ctrl$allow_par, object)

  safely_iter_mod_with_recipe <- super_safely_iterate(iter_mod_with_recipe)

  results <-
    foreach::foreach(rs_iter = 1:B, .packages = "tune", .errorhandling = "pass") %op%
    safely_iter_mod_with_recipe(rs_iter, resamples, grid, object, metrics, ctrl)

  resamples <- pull_metrics(resamples, results, ctrl)
  resamples <- pull_notes(resamples, results, ctrl)
  resamples <- pull_extracts(resamples, results, ctrl)
  resamples <- pull_predictions(resamples, results, ctrl)

  resamples
}

iter_mod_with_recipe <- function(rs_iter, resamples, grid, object, metrics, ctrl) {
  load_pkgs(object)
  load_namespace(ctrl$pkgs)
  fit_ctrl <- parsnip::fit_control(verbosity = 0, catch = TRUE)
  split <- resamples$splits[[rs_iter]]
  metric_est <- NULL
  extracted <- NULL
  pred_vals <- NULL
  .notes <- NULL

  # ----------------------------------------------------------------------------

  tmp_rec <-
    catch_and_log(train_recipe(split, object, NULL),
                  ctrl,
                  split,
                  "recipe",
                  notes = .notes)

  # check for recipe failure
  if (is_failure(tmp_rec)) {
    out <- list(
      .metrics = metric_est,
      .extracts = extracted,
      .predictions = pred_vals,
      .notes = .notes
    )

    return(out)
  }

  # Determine the _minimal_ number of models to fit in order to get
  # predictions on all models.
  mod_grid_vals <- get_wflow_model(object) %>% min_grid(grid)

  num_mod <- nrow(mod_grid_vals)
  for (mod_iter in 1:num_mod) {
    mod_msg <- paste0("model ", format(1:num_mod)[mod_iter], "/", num_mod)

    tmp_fit <-
      catch_and_log(
        train_model_from_recipe(object, tmp_rec, mod_grid_vals[mod_iter,], control = fit_ctrl),
        ctrl,
        split,
        mod_msg,
        notes = .notes
      )

    # check for parsnip level and model level failure
    if (is_failure(tmp_fit) || is_failure(tmp_fit$fit)) {
      next
    }

    extracted <- append_extracts(extracted, tmp_rec, tmp_fit$fit, mod_grid_vals[mod_iter, ], split, ctrl)

    tmp_pred <-
      catch_and_log(
        predict_model_from_recipe(split, tmp_fit, tmp_rec, mod_grid_vals[mod_iter,], metrics),
        ctrl,
        split,
        paste(mod_msg, "(predictions)"),
        bad_only = TRUE,
        notes = .notes
      )

    # check for prediction level failure
    if (is_failure(tmp_pred)) {
      next
    }

    metric_est  <- append_metrics(metric_est, tmp_pred, object, metrics, split)
    pred_vals <- append_predictions(pred_vals, tmp_pred, split, ctrl)
  } # end model loop

  list(.metrics = metric_est, .extracts = extracted, .predictions = pred_vals, .notes = .notes)

}

# ------------------------------------------------------------------------------


tune_mod_with_formula <- function(resamples, grid, object, metrics, ctrl) {
  B <- nrow(resamples)

  `%op%` <- get_operator(ctrl$allow_par, object)

  safely_iter_mod_with_formula <- super_safely_iterate(iter_mod_with_formula)

  results <-
    foreach::foreach(rs_iter = 1:B, .packages = "tune", .errorhandling = "pass") %op%
    safely_iter_mod_with_formula(rs_iter, resamples, grid, object, metrics, ctrl)

  resamples <- pull_metrics(resamples, results, ctrl)
  resamples <- pull_notes(resamples, results, ctrl)
  resamples <- pull_extracts(resamples, results, ctrl)
  resamples <- pull_predictions(resamples, results, ctrl)

  resamples
}

iter_mod_with_formula <- function(rs_iter, resamples, grid, object, metrics, ctrl) {
  load_pkgs(object)
  load_namespace(ctrl$pkgs)
  fit_ctrl <- parsnip::fit_control(verbosity = 0, catch = TRUE)
  split <- resamples$splits[[rs_iter]]
  metric_est <- NULL
  extracted <- NULL
  pred_vals <- NULL
  .notes <- NULL

  # ----------------------------------------------------------------------------

  tmp_df <- catch_and_log(exec_formula(split, object), ctrl, split, "formula", notes = .notes)

  # check for formula failure
  if (is_failure(tmp_df)) {
    out <- list(
      .metrics = metric_est,
      .extracts = extracted,
      .predictions = pred_vals,
      .notes = .notes
    )

    return(out)
  }

  tmp_trms <- tmp_df$terms
  tmp_df <- tmp_df[c("x", "y")]

  # Determine the _minimal_ number of models to fit in order to get
  # predictions on all models.
  mod_grid_vals <- get_wflow_model(object) %>% min_grid(grid)

  num_mod <- nrow(mod_grid_vals)
  for (mod_iter in 1:num_mod) {
    param_val <- mod_grid_vals[mod_iter, ]
    mod_msg <- paste0("model ", format(1:num_mod)[mod_iter], "/", num_mod)

    tmp_fit <-
      catch_and_log(
        train_model_from_df(object, tmp_df, param_val, control = fit_ctrl),
        ctrl,
        split,
        mod_msg,
        notes = .notes
      )

    # check for parsnip level and model level failure
    if (is_failure(tmp_fit) || is_failure(tmp_fit$fit)) {
      next
    }

    extracted <- append_extracts(extracted, NULL, tmp_fit$fit, param_val, split, ctrl)

    pred_msg <- paste(mod_msg, "(predictions)")

    tmp_pred <-
      catch_and_log(
        predict_model_from_terms(split, tmp_fit, tmp_trms, param_val, metrics),
        ctrl,
        split,
        mod_msg,
        notes = .notes
      )

    # check for prediction level failure
    if (is_failure(tmp_pred)) {
      next
    }

    metric_est  <- append_metrics(metric_est, tmp_pred, object, metrics, split)
    pred_vals <- append_predictions(pred_vals, tmp_pred, split, ctrl)
  } # end model loop

  list(.metrics = metric_est, .extracts = extracted, .predictions = pred_vals, .notes = .notes)

}

# ----------------------------------------------------------------------------

super_safely_iterate <- function(fn) {
  purrr::partial(.f = super_safely_iterate_impl, fn = fn)
}

super_safely_iterate_impl <- function(fn, rs_iter, resamples, grid, object, metrics, ctrl) {
  safely_iterate <- super_safely(fn)

  result <- safely_iterate(rs_iter, resamples, grid, object, metrics, ctrl)

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

  notes <- log_problems(notes, ctrl, split, "internal", problems)

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
