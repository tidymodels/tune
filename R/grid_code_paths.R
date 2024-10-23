tune_grid_loop <- function(resamples,
                           grid,
                           workflow,
                           metrics,
                           control,
                           eval_time = NULL,
                           rng,
                           split_args) {
  fn_tune_grid_loop <- tune_grid_loop_tune

  if (workflow_uses_agua(workflow)) {
    # Special allowance for agua, which uses a custom loop implementation
    fn_tune_grid_loop <- tune_grid_loop_agua
  }

  results <- fn_tune_grid_loop(
    resamples,
    grid,
    workflow,
    metrics,
    control,
    eval_time,
    rng,
    split_args
  )

  # carry out arranging by id before extracting each element of results (#728)
  resample_ids <- grep("^id", names(resamples), value = TRUE)
  id_order <- vctrs::vec_order(resamples[resample_ids])

  resamples <- pull_metrics(resamples, results, control, order = id_order)
  resamples <- pull_notes(resamples, results, control, order = id_order)
  resamples <- pull_extracts(resamples, results, control, order = id_order)
  resamples <- pull_predictions(resamples, results, control, order = id_order)
  resamples <- pull_all_outcome_names(resamples, results)

  resamples
}

# ------------------------------------------------------------------------------

tune_grid_loop_tune <- function(resamples,
                                grid,
                                workflow,
                                metrics,
                                control,
                                eval_time = NULL,
                                rng,
                                split_args) {
  n_resamples <- nrow(resamples)

  parallel_over <- control$parallel_over
  parallel_over <- parallel_over_finalize(parallel_over, n_resamples)

  fn_tune_grid_loop_iter <- tune_grid_loop_iter

  tune_grid_loop_impl(
    fn_tune_grid_loop_iter = fn_tune_grid_loop_iter,
    resamples = resamples,
    grid = grid,
    workflow = workflow,
    metrics = metrics,
    control = control,
    eval_time = eval_time,
    rng = rng,
    parallel_over = parallel_over,
    split_args = split_args
  )
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

# ------------------------------------------------------------------------------
# Agua / h2o specific utilities

tune_grid_loop_agua <- function(resamples,
                                grid,
                                workflow,
                                metrics,
                                control,
                                eval_time = NULL,
                                rng) {
  if (!rlang::is_installed("agua")) {
    cli::cli_abort("The {.pkg agua} package must be installed to use an h2o
                   parsnip engine.")
  }

  parallel_over <- control$parallel_over
  parallel_over <- parallel_over_finalize_agua(parallel_over)

  fn_tune_grid_loop_iter <- utils::getFromNamespace(
    x = "tune_grid_loop_iter_agua",
    ns = "agua"
  )

  tune_grid_loop_impl(
    fn_tune_grid_loop_iter = fn_tune_grid_loop_iter,
    resamples = resamples,
    grid = grid,
    workflow = workflow,
    metrics = metrics,
    control = control,
    eval_time = eval_time,
    rng = rng,
    parallel_over = parallel_over
  )
}

parallel_over_finalize_agua <- function(parallel_over) {
  if (is.null(parallel_over)) {
    parallel_over <- "resamples"
  }

  if (!identical(parallel_over, "resamples")) {
    cli::cli_abort("The {.arg agua} package only supports
                   {.code parallel_over = 'resamples'}.")
  }

  parallel_over
}

workflow_uses_agua <- function(workflow) {
  model_spec <- hardhat::extract_spec_parsnip(workflow)
  identical(model_spec$engine, "h2o")
}

# ------------------------------------------------------------------------------

tune_grid_loop_impl <- function(fn_tune_grid_loop_iter,
                                resamples,
                                grid,
                                workflow,
                                metrics,
                                control,
                                eval_time = NULL,
                                rng,
                                parallel_over,
                                split_args) {
  splits <- resamples$splits
  packages <- c(control$pkgs, required_pkgs(workflow))
  grid_info <- compute_grid_info(workflow, grid)
  metrics_info <- metrics_info(metrics)
  params <- hardhat::extract_parameter_set_dials(workflow)

  if (!catalog_is_active()) {
    initialize_catalog(control = control)
  }

  n_splits <- length(splits)
  n_grid_info <- nrow(grid_info)

  # Must assign `tune:::tune_grid_loop_iter_safely()` to a function in
  # this function environment so foreach can find it. Mainly an issue with
  # doParallel and PSOCK clusters.
  fn_tune_grid_loop_iter_safely <- tune_grid_loop_iter_safely

  do_op <- get_operator(control$allow_par, workflow)
  `%op%` <- do_op[[1]]
  is_future <- do_op[[2]]
  `%:%` <- foreach::`%:%`

  rlang::local_options(doFuture.rng.onMisuse = "ignore")

  if (identical(parallel_over, "resamples")) {
    seeds <- generate_seeds(rng, n_splits)

    # Evaluate the call to foreach in a local environment using a clone of
    # the current environment so that foreach doesn't touch the exit
    # handlers attached to the execution environment of this function
    # by `initialize_catalog()` (#828, #845, #846).
    results <- rlang::with_env(rlang::env_clone(rlang::current_env()), {
      # Rather than generating them programmatically, write each `foreach()`
      # call out since `foreach()` `substitute()`s its dots. Note that
      # doFuture will error when passed `.packages`.
      if (is_future) {
        for_each <-
          foreach::foreach(
            split = splits,
            seed = seeds,
            .options.future = list(seed = NULL, packages = packages)
          )
      } else {
        for_each <-
          foreach::foreach(
            split = splits,
            seed = seeds,
            .packages = packages,
            .errorhandling = "pass"
          )
      }

      suppressPackageStartupMessages(
        for_each %op% {
          # Likely want to debug with `debugonce(tune_grid_loop_iter)`
          fn_tune_grid_loop_iter_safely(
            fn_tune_grid_loop_iter = fn_tune_grid_loop_iter,
            split = split,
            grid_info = grid_info,
            workflow = workflow,
            metrics = metrics,
            control = control,
            eval_time = eval_time,
            seed = seed,
            metrics_info = metrics_info,
            params = params,
            split_args = split_args
          )
        }
      )
    })

    return(results)
  }

  if (identical(parallel_over, "everything")) {
    iterations <- seq_len(n_splits)
    rows <- seq_len(n_grid_info)

    seeds <- generate_seeds(rng, n_splits * n_grid_info)

    # Make `slice_seeds` available in the environment that foreach will
    # search for it in; resolves issue with PSOCK/multisession parallelism
    # (#888, #889).
    slice_seeds <- slice_seeds

    # Evaluate the call to foreach in a local environment using a clone of
    # the current environment so that foreach doesn't touch the exit
    # handlers attached to the execution environment of this function
    # by `initialize_catalog()` (#828, #845, #846).
    results <- rlang::with_env(rlang::env_clone(rlang::current_env()), {
      # Rather than generating them programmatically, write each `foreach()`
      # call out since `foreach()` `substitute()`s its dots. Note that
      # doFuture will error when passed `.packages`.
      if (is_future) {
        for_each <-
          foreach::foreach(
            split = splits,
            iteration = iterations,
            .options.future = list(seed = NULL, packages = packages)
          ) %:%
          foreach::foreach(
            row = rows,
            .combine = iter_combine,
            .options.future = list(seed = NULL, packages = packages)
          )
      } else {
        for_each <-
          foreach::foreach(
            split = splits,
            iteration = iterations,
            .packages = packages,
            .errorhandling = "pass"
          ) %:%
          foreach::foreach(
            row = rows,
            .combine = iter_combine,
            .packages = packages,
            .errorhandling = "pass"
          )
      }

      suppressPackageStartupMessages(
        for_each %op% {
            grid_info_row <- vctrs::vec_slice(grid_info, row)
            seed <- slice_seeds(seeds, iteration, n_grid_info)[[1]]

            # Likely want to debug with `debugonce(tune_grid_loop_iter)`
            fn_tune_grid_loop_iter_safely(
              fn_tune_grid_loop_iter = fn_tune_grid_loop_iter,
              split = split,
              grid_info = grid_info_row,
              workflow = workflow,
              metrics = metrics,
              control = control,
              eval_time = eval_time,
              seed = seed,
              metrics_info = metrics_info,
              params = params,
              split_args = split_args
            )
          }
      )
    })

    return(results)
  }

  cli::cli_abort("Invalid {.arg parallel_over}.", .internal = TRUE)
}

# ------------------------------------------------------------------------------

# Combine results from individual hyperparameter combination iterations.
# For use by `flat` parallel method to return something to the outer parallel
# loop that looks identical to the `outer` parallel method
iter_combine <- function(...) {
  results <- list(...)

  metrics <- purrr::map(results, ~ .x[[".metrics"]])
  extracts <- purrr::map(results, ~ .x[[".extracts"]])
  predictions <- purrr::map(results, ~ .x[[".predictions"]])
  all_outcome_names <- purrr::map(results, ~ .x[[".all_outcome_names"]])
  notes <- purrr::map(results, ~ .x[[".notes"]])

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

tune_grid_loop_iter <- function(split,
                                grid_info,
                                workflow,
                                metrics,
                                control,
                                eval_time = NULL,
                                seed,
                                metrics_info = metrics_info(metrics),
                                params,
                                split_args = NULL) {
  split_labels <- labels(split)

  load_pkgs(workflow)
  .load_namespace(control$pkgs)

  # After package loading to avoid potential package RNG manipulation
  if (!is.null(seed)) {
    # `assign()`-ing the random seed alters the `kind` type to L'Ecuyer-CMRG,
    # so we have to ensure it is restored on exit
    old_kind <- RNGkind()[[1]]
    assign(".Random.seed", seed, envir = globalenv())
    on.exit(RNGkind(kind = old_kind), add = TRUE)
  }

  control_parsnip <- parsnip::control_parsnip(verbosity = 0, catch = TRUE)
  control_workflow <- control_workflow(control_parsnip = control_parsnip)

  event_level <- control$event_level

  out_metrics <- NULL
  out_extracts <- NULL
  out_predictions <- NULL
  out_all_outcome_names <- list()
  out_notes <-
    tibble::new_tibble(list(
      location = character(0),
      type = character(0),
      note = character(0),
      trace = list()
    ))

  model_params <- vctrs::vec_slice(params, params$source == "model_spec")
  preprocessor_params <- vctrs::vec_slice(params, params$source == "recipe")

  param_names <- params$id
  model_param_names <- model_params$id
  preprocessor_param_names <- preprocessor_params$id

  # inline rsample::assessment so that we can pass indices to `predict_model()`
  assessment_rows <- as.integer(split, data = "assessment")
  assessment <- vctrs::vec_slice(split$data, assessment_rows)

  if (workflows::.workflow_includes_calibration(workflow)) {
    # if the workflow has a postprocessor that needs training (i.e. calibration),
    # further split the analysis data into an "inner" analysis and
    # assessment set.
    # * the preprocessor and model (excluding the post-processor) are fitted
    #   on `analysis(inner_split(split))`, the inner analysis set (just
    #   referred to as analysis)
    # * that model generates predictions on `assessment(inner_split(split))`,
    #   the calibration set
    # * the post-processor is trained on the predictions generated from the
    #   calibration set
    # * the model (including the post-processor) generates predictions on the
    #   assessment set and those predictions are assessed with performance metrics
    split <- rsample::inner_split(split, split_args = split_args)
    analysis <- rsample::analysis(split)

    # inline rsample::assessment so that we can pass indices to `predict_model()`
    calibration_rows <- as.integer(split, data = "assessment")
    calibration <- vctrs::vec_slice(split$data, calibration_rows)
  } else {
    analysis <- rsample::analysis(split)

    calibration_rows <- NULL
    calibration <- NULL
  }

  rm(split)

  # ----------------------------------------------------------------------------
  # Preprocessor loop

  iter_preprocessors <- unique(grid_info[[".iter_preprocessor"]])

  workflow_original <- workflow

  for (iter_preprocessor in iter_preprocessors) {
    workflow <- workflow_original

    iter_grid_info <- vctrs::vec_slice(
      grid_info,
      grid_info$.iter_preprocessor == iter_preprocessor
    )

    iter_grid_preprocessor <- iter_grid_info[1L, preprocessor_param_names]

    iter_msg_preprocessor <- iter_grid_info[[".msg_preprocessor"]][1]

    workflow <- finalize_workflow_preprocessor(
      workflow = workflow,
      grid_preprocessor = iter_grid_preprocessor
    )

    workflow <- .catch_and_log(
      .expr = .fit_pre(workflow, analysis),
      control,
      split_labels,
      iter_msg_preprocessor,
      notes = out_notes
    )

    if (is_failure(workflow)) {
      next
    }

    # --------------------------------------------------------------------------
    # Model loop

    iter_models <- iter_grid_info[[".iter_model"]]

    workflow_preprocessed <- workflow

    for (iter_model in iter_models) {
      workflow <- workflow_preprocessed

      iter_grid_info_model <- vctrs::vec_slice(
        iter_grid_info,
        iter_grid_info$.iter_model == iter_model
      )

      iter_grid_model <- iter_grid_info_model[, model_param_names]

      iter_submodels <- iter_grid_info_model[[".submodels"]][[1L]]
      iter_msg_model <- iter_grid_info_model[[".msg_model"]]
      iter_config <- iter_grid_info_model[[".iter_config"]][[1L]]

      workflow <- finalize_workflow_spec(workflow, iter_grid_model)

      workflow <- .catch_and_log_fit(
        .expr = .fit_model(workflow, control_workflow),
        control,
        split_labels,
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

      iter_grid <- dplyr::bind_cols(
        iter_grid_preprocessor,
        iter_grid_model
      )

      if (!has_postprocessor(workflow)) {
        elt_extract <- .catch_and_log(
          extract_details(workflow, control$extract),
          control,
          split_labels,
          paste(iter_msg_model, "(extracts)"),
          bad_only = TRUE,
          notes = out_notes
        )
        elt_extract <- make_extracts(elt_extract, iter_grid, split_labels, .config = iter_config)
        out_extracts <- append_extracts(out_extracts, elt_extract)
      }

      iter_msg_predictions <- paste(iter_msg_model, "(predictions)")

      iter_predictions <- .catch_and_log(
        predict_model(calibration %||% assessment, calibration_rows %||% assessment_rows,
                      workflow, iter_grid, metrics, iter_submodels,
                      metrics_info = metrics_info, eval_time = eval_time),
        control,
        split_labels,
        iter_msg_predictions,
        bad_only = TRUE,
        notes = out_notes
      )

      # Check for prediction level failure
      if (is_failure(iter_predictions)) {
        next
      }

      if (has_postprocessor(workflow)) {
        # note that, since we're training a postprocessor, `iter_predictions`
        # are the predictions from the calibration set rather than the
        # assessment set

        # train the post-processor on the predictions generated from the model
        # on the calibration set
        # todo: needs a `.catch_and_log`
        #
        # if the postprocessor does not require training, then `calibration` will
        # be NULL and nothing other than the column names is learned from
        # `assessment`.
        workflow_with_post <- .fit_post(workflow, calibration %||% assessment)

        workflow_with_post <- .fit_finalize(workflow_with_post)

        # run extract function on workflow with trained postprocessor
        elt_extract <- .catch_and_log(
          extract_details(workflow_with_post, control$extract),
          control,
          split_labels,
          paste(iter_msg_model, "(extracts)"),
          bad_only = TRUE,
          notes = out_notes
        )
        elt_extract <- make_extracts(elt_extract, iter_grid, split_labels, .config = iter_config)
        out_extracts <- append_extracts(out_extracts, elt_extract)

        # generate predictions on the assessment set from the model and apply the
        # post-processor to those predictions to generate updated predictions
        iter_predictions <- .catch_and_log(
          predict_model(assessment, assessment_rows, workflow_with_post, iter_grid,
                        metrics, iter_submodels, metrics_info = metrics_info,
                        eval_time = eval_time),
          control,
          split_labels,
          paste(iter_msg_model, "(predictions with post-processor)"),
          bad_only = TRUE,
          notes = out_notes
        )

        # now, assess those predictions with performance metrics
      }

      out_metrics <- append_metrics(
        collection = out_metrics,
        predictions = iter_predictions,
        metrics = metrics,
        param_names = param_names,
        outcome_name = outcome_names,
        event_level = event_level,
        split_labels = split_labels,
        .config = iter_config,
        metrics_info = metrics_info
      )

      iter_config_metrics <- extract_metrics_config(param_names, out_metrics)

      out_predictions <- append_predictions(
        collection = out_predictions,
        predictions = iter_predictions,
        split_labels = split_labels,
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

tune_grid_loop_iter_safely <- function(fn_tune_grid_loop_iter,
                                       split,
                                       grid_info,
                                       workflow,
                                       metrics,
                                       control,
                                       eval_time = NULL,
                                       seed,
                                       metrics_info,
                                       params,
                                       split_args) {

  fn_tune_grid_loop_iter_wrapper <- super_safely(fn_tune_grid_loop_iter)

  # Likely want to debug with `debugonce(tune_grid_loop_iter)`
  result <- fn_tune_grid_loop_iter_wrapper(
    split,
    grid_info,
    workflow,
    metrics,
    control,
    eval_time,
    seed,
    metrics_info = metrics_info,
    params,
    split_args
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

  notes <- log_problems(notes, control, labels(split), "internal", problems)

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
    e <- structure(conditionMessage(e), class = "try-error", condition = e)
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
