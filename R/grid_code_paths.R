tune_grid_loop <- function(resamples, grid, workflow, metrics, control, rng) {
  `%op%` <- get_operator(control$allow_par, workflow)
  `%:%` <- foreach::`%:%`

  current_rng_kind <- RNGkind()[[1]]
  on.exit(
    RNGkind(kind = current_rng_kind),
    add = TRUE
  )

  tune_grid_loop_iter_safely <- super_safely_iterate(tune_grid_loop_iter)

  packages <- c(control$pkgs, required_pkgs(workflow))

  grid_info <- compute_grid_info(workflow, grid)

  n_resamples <- nrow(resamples)
  iterations <- seq_len(n_resamples)

  n_grid_info <- nrow(grid_info)
  rows <- seq_len(n_grid_info)

  parallel_over <- control$parallel_over
  parallel_over <- parallel_over_finalize(parallel_over, n_resamples)

  rlang::local_options(doFuture.rng.onMisuse = "ignore")

  if (identical(parallel_over, "resamples")) {
    seeds <- generate_seeds(rng, n_resamples)

    suppressPackageStartupMessages(
      results <- foreach::foreach(
        iteration = iterations,
        seed = seeds,
        .packages = packages,
        .errorhandling = "pass"
      ) %op% {
        tune_grid_loop_iter_safely(
          iteration = iteration,
          resamples = resamples,
          grid_info = grid_info,
          workflow = workflow,
          metrics = metrics,
          control = control,
          seed = seed
        )
      }
    )
  } else if (identical(parallel_over, "everything")) {
    seeds <- generate_seeds(rng, n_resamples * n_grid_info)

    suppressPackageStartupMessages(
      results <- foreach::foreach(
        iteration = iterations,
        .packages = packages,
        .errorhandling = "pass"
      ) %:%
        foreach::foreach(
          row = rows,
          seed = slice_seeds(seeds, iteration, n_grid_info),
          .packages = packages,
          .errorhandling = "pass",
          .combine = iter_combine
        ) %op% {
          grid_info_row <- vctrs::vec_slice(grid_info, row)

          tune_grid_loop_iter_safely(
            iteration = iteration,
            resamples = resamples,
            grid_info = grid_info_row,
            workflow = workflow,
            metrics = metrics,
            control = control,
            seed = seed
          )
        }
    )
  } else {
    rlang::abort("Internal error: Invalid `parallel_over`.")
  }

  resamples <- pull_metrics(resamples, results, control)
  resamples <- pull_notes(resamples, results, control)
  resamples <- pull_extracts(resamples, results, control)
  resamples <- pull_predictions(resamples, results, control)
  resamples <- pull_all_outcome_names(resamples, results)

  resamples
}

# ------------------------------------------------------------------------------

# Combine results from individual hyperparameter combination iterations.
# For use by `flat` parallel method to return something to the outer parallel
# loop that looks identical to the `outer` parallel method
iter_combine <- function(...) {
  results <- list(...)

  metrics <- purrr::map(results, ~.x[[".metrics"]])
  extracts <- purrr::map(results, ~.x[[".extracts"]])
  predictions <- purrr::map(results, ~.x[[".predictions"]])
  all_outcome_names <- purrr::map(results, ~.x[[".all_outcome_names"]])
  notes <- purrr::map(results, ~.x[[".notes"]])

  metrics <- vec_c(!!!metrics)
  extracts <- vec_c(!!!extracts)
  predictions <- vec_c(!!!predictions)
  all_outcome_names <- vec_c(!!!all_outcome_names)
  notes <- vec_c(!!!notes)

  list(
    .metrics = metrics,
    .extracts = extracts,
    .predictions = predictions,
    .all_outcome_names = all_outcome_names,
    .notes = notes
  )
}

# ------------------------------------------------------------------------------

tune_grid_loop_iter <- function(iteration,
                                resamples,
                                grid_info,
                                workflow,
                                metrics,
                                control,
                                seed) {
  load_pkgs(workflow)
  load_namespace(control$pkgs)

  # After package loading to avoid potential package RNG manipulation
  if (!is.null(seed)) {
    assign(".Random.seed", seed, envir = globalenv())
  }

  control_parsnip <- parsnip::control_parsnip(verbosity = 0, catch = TRUE)
  control_workflow <- control_workflow(control_parsnip = control_parsnip)

  event_level <- control$event_level

  out_metrics <- NULL
  out_extracts <- NULL
  out_predictions <- NULL
  out_all_outcome_names <- list()
  out_notes <- NULL

  params <- dials::parameters(workflow)
  model_params <- dplyr::filter(params, source == "model_spec")
  preprocessor_params <- dplyr::filter(params, source == "recipe")

  param_names <- dplyr::pull(params, "id")
  model_param_names <- dplyr::pull(model_params, "id")
  preprocessor_param_names <- dplyr::pull(preprocessor_params, "id")

  # Model related grid-info columns
  cols <- rlang::expr(
    c(
      .iter_model,
      .iter_config,
      .msg_model,
      dplyr::all_of(model_param_names),
      .submodels
    )
  )

  # Nest grid_info:
  # - Preprocessor info in the outer level
  # - Model info in the inner level
  if (tidyr_new_interface()) {
    grid_info <- tidyr::nest(grid_info, data = !!cols)
  } else {
    grid_info <- tidyr::nest(grid_info, !!cols)
  }

  split <- resamples$splits[[iteration]]
  training <- rsample::analysis(split)

  # ----------------------------------------------------------------------------
  # Preprocessor loop

  iter_preprocessors <- grid_info[[".iter_preprocessor"]]

  workflow_original <- workflow

  for (iter_preprocessor in iter_preprocessors) {
    workflow <- workflow_original

    iter_grid_info <- dplyr::filter(
      .data = grid_info,
      .iter_preprocessor == iter_preprocessor
    )

    iter_grid_preprocessor <- dplyr::select(
      .data = iter_grid_info,
      dplyr::all_of(preprocessor_param_names)
    )

    iter_msg_preprocessor <- iter_grid_info[[".msg_preprocessor"]]

    workflow <- finalize_workflow_preprocessor(
      workflow = workflow,
      grid_preprocessor = iter_grid_preprocessor
    )

    workflow <- catch_and_log(
      .expr = .fit_pre(workflow, training),
      control,
      split,
      iter_msg_preprocessor,
      notes = out_notes
    )

    if (is_failure(workflow)) {
      next
    }

    # --------------------------------------------------------------------------
    # Model loop

    iter_grid_info_models <- iter_grid_info[["data"]][[1L]]
    iter_models <- iter_grid_info_models[[".iter_model"]]

    workflow_preprocessed <- workflow

    for (iter_model in iter_models) {
      workflow <- workflow_preprocessed

      iter_grid_info_model <- dplyr::filter(
        .data = iter_grid_info_models,
        .iter_model == iter_model
      )

      iter_grid_model <- dplyr::select(
        .data = iter_grid_info_model,
        dplyr::all_of(model_param_names)
      )

      iter_submodels <- iter_grid_info_model[[".submodels"]][[1L]]
      iter_msg_model <- iter_grid_info_model[[".msg_model"]]
      iter_config <- iter_grid_info_model[[".iter_config"]][[1L]]

      workflow <- finalize_workflow_spec(workflow, iter_grid_model)

      workflow <- catch_and_log_fit(
        expr = .fit_model(workflow, control_workflow),
        control,
        split,
        iter_msg_model,
        notes = out_notes
      )

      # Check for parsnip level and model level failure
      if (is_failure(workflow) || is_failure(workflow$fit$fit$fit)) {
        next
      }

      workflow <- .fit_finalize(workflow)

      # Extract outcome names from the hardhat mold
      outcome_names <- outcome_names(workflow)

      out_all_outcome_names <- append_outcome_names(
        all_outcome_names = out_all_outcome_names,
        outcome_names = outcome_names
      )

      # FIXME: I think this might be wrong? Doesn't use submodel parameters,
      # so `extracts` column doesn't list the correct parameters.
      iter_grid <- dplyr::bind_cols(
        iter_grid_preprocessor,
        iter_grid_model
      )

      # FIXME: bind_cols() drops number of rows with zero col data frames
      # because of a bug with vec_cbind()
      # https://github.com/r-lib/vctrs/issues/1281
      if (ncol(iter_grid_preprocessor) == 0L && ncol(iter_grid_model) == 0L) {
        nrow <- nrow(iter_grid_model)
        iter_grid <- tibble::new_tibble(x = list(), nrow = nrow)
      }

      out_extracts <- append_extracts(
        collection = out_extracts,
        workflow = workflow,
        grid = iter_grid,
        split = split,
        ctrl = control,
        .config = iter_config
      )

      iter_msg_predictions <- paste(iter_msg_model, "(predictions)")

      iter_predictions <- catch_and_log(
        predict_model(split, workflow, iter_grid, metrics, iter_submodels),
        control,
        split,
        iter_msg_predictions,
        bad_only = TRUE,
        notes = out_notes
      )

      # Check for prediction level failure
      if (is_failure(iter_predictions)) {
        next
      }

      out_metrics <- append_metrics(
        collection = out_metrics,
        predictions = iter_predictions,
        metrics = metrics,
        param_names = param_names,
        outcome_name = outcome_names,
        event_level = event_level,
        split = split,
        .config = iter_config
      )

      iter_config_metrics <- extract_metrics_config(param_names, out_metrics)

      out_predictions <- append_predictions(
        collection = out_predictions,
        predictions = iter_predictions,
        split = split,
        control = control,
        .config = iter_config_metrics
      )
    } # model loop
  } # preprocessor loop

  list(
    .metrics = out_metrics,
    .extracts = out_extracts,
    .predictions = out_predictions,
    .all_outcome_names = out_all_outcome_names,
    .notes = out_notes
  )
}

# ------------------------------------------------------------------------------

super_safely_iterate <- function(fn) {
  purrr::partial(.f = super_safely_iterate_impl, fn = fn)
}

super_safely_iterate_impl <- function(fn,
                                      iteration,
                                      resamples,
                                      grid_info,
                                      workflow,
                                      metrics,
                                      control,
                                      seed) {
  safely_iterate <- super_safely(fn)

  result <- safely_iterate(
    iteration,
    resamples,
    grid_info,
    workflow,
    metrics,
    control,
    seed
  )

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

  split <- resamples$splits[[iteration]]

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

parallel_over_finalize <- function(parallel_over, n_resamples) {
  # Always use user supplied option, even if not as efficient
  if (!is.null(parallel_over)) {
    return(parallel_over)
  }

  # Generally more efficient to parallelize over just resamples,
  # but if there is only 1 resample we instead parallelize over
  # "everything" (resamples and the hyperparameter grid) to maximize
  # core utilization
  if (n_resamples == 1L) {
    "everything"
  } else {
    "resamples"
  }
}

# Note:
# We generate seeds in such a way that each call to `tune_grid_loop_iter()`
# will get its own unique seed. It sets that seed once at the top of the
# function call, then runs all the models in that loop iteration.
generate_seeds <- function(rng, n) {
  out <- vector("list", length = n)

  if (!rng) {
    # `NULL` elements are the signal to not fix the random seed
    return(out)
  }

  original_algorithms <- RNGkind(kind = "L'Ecuyer-CMRG")
  original_rng_algorithm <- original_algorithms[[1]]

  on.exit(
    RNGkind(kind = original_rng_algorithm),
    add = TRUE
  )

  seed <- .Random.seed

  for (i in seq_len(n)) {
    out[[i]] <- seed
    seed <- parallel::nextRNGStream(seed)
  }

  out
}

slice_seeds <- function(x, i, n) {
  # Slice out the current iteration worth of seeds
  x[(i - 1L) * n + seq_len(n)]
}
