# tune creates the schedule object. With h2o, we need to determine the model
# and post candidates (if any). The model parameters should be converted into
# a tibble along with post_stage that contains any tuning parameter info for
# the postprocessors. This has to be done for each preprocessing tuning parameter
# combination (if any). Note that the predict_stage is not needed for h2o so
# we need to handle submodel information that was in the predict_state and
# return it to the model grid.

submodel_names <- function(x) names(x)[names(x) != "post_stage"]

late_stage_grid <- function(sched_pre_iter) {

  # No tuning parameters anywhere
  if (nrow(sched_pre_iter) == 0) {
    return(tibble::tibble(post_stage = list()))
  }

  # No submodels apart from the recipe
  if (!any(names(sched_pre_iter) == "model_stage")) {
    sched_pre_iter$post_stage <- list(tibble::tibble())
    return(sched_pre_iter)
  }

  res <-
    sched_pre_iter |>
    dplyr::select(model_stage) |>
    tidyr::unnest(model_stage)

  # If there are submodels, the submodel column shows up at the model_stage
  # and the full vector of values is in the predict_stage (the latter are what
  # we want). If we leave both, the unnesting throws an error
  submodel_param <- unique(unlist(purrr::map(res$predict_stage, submodel_names)))

  if (any(names(res) == submodel_param)) {
    res[[submodel_param]] <- NULL
  }

  tidyr::unnest(res, predict_stage)
}

# ------------------------------------------------------------------------------

loop_over_all_stages_agua <- function(resamples, grid, static) {
  # Some packages may use random numbers so attach them prior to initializing
  # the RNG seed
  attach_pkgs(static$pkgs)

  # Initialize some objects
  seed_length <- length(resamples$.seeds[[1]])

  # If we are using last_fit() (= zero seed length), don't mess with the RNG
  # stream; otherwise set everything up.
  if (seed_length > 0) {
    orig_seed <- .Random.seed
    # Set seed within the worker process
    assign(".Random.seed", resamples$.seeds[[1]], envir = .GlobalEnv)
    resamples$.seeds <- NULL
    withr::defer(assign(".Random.seed", orig_seed, envir = .GlobalEnv))
  }

  split <- resamples$splits[[1]]
  split_labs <- resamples |>
    dplyr::select(dplyr::starts_with("id"))

  pred_reserve <- NULL
  pred_iter <- 0
  notes <- new_note()
  extracts <- NULL

  sched <- schedule_grid(grid, static$wflow)

  config_tbl <- static$configs

  # Append data partitions here; these are the same for the duration of this function
  data_splits <- get_data_subsets(static$wflow, split, static$split_args)
  static <- update_static(static, data_splits)

  # Now that we have data, determine the names of the outcome data. NOTE that
  # if an inline function is used (e.g. add_formula(log(mpg) ~ .)), We will
  # potentially change it later. See #1024
  static$y_name <- outcome_names(static$wflow, data = split$data)

  # ----------------------------------------------------------------------------
  # Iterate over preprocessors

  num_iterations_pre <- max(nrow(sched), 1)

  for (iter_pre in seq_len(num_iterations_pre)) {
    current_sched_pre <- sched[iter_pre, ]
    location <- glue::glue("preprocessor {iter_pre}/{num_iterations_pre}")
    current_wflow <- .catch_and_log(
      finalize_fit_pre(static$wflow, current_sched_pre, static),
      control = static$control,
      split_labels = split_labs,
      location = location,
      notes = notes
    )

    if (is_failure(current_wflow)) {
      next
    }
    # Update y_name in case the workflow had an inline function like `log(mpg) ~ .`
    static$y_name <- outcome_names(current_wflow)

    # --------------------------------------------------------------------------
    # Iterate over model parameters using h2o

    # Make a copy of the current workflow so that we can finalize it multiple
    # times, since finalize_*() functions will not update parameters whose
    # values currently are tune()
    pre_wflow <- current_wflow

    # Convert the current schedule to a tibble with the grid points for the
    # model candidates (if any) and, for each candidate, the postprocessing
    # grid in a column called post_stage.
    h2o_grid_info <- late_stage_grid(current_sched_pre)

    # Just the model candidates please. There is no special processing for
    # submodels.
    h2o_grid <- h2o_grid_info |> dplyr::select(-post_stage)

    # Preprocessing grid
    pre_grid <- remove_stage(current_sched_pre)

    num_iterations_model <- max(nrow(h2o_grid), 1)

    # Get processed versions of 2-3 data sets
    static_agua <- forge_all_from_workflow(static, pre_wflow)

    # Process all models across tuning parameters (if any) at the same time.
    # This results in a list of prediction tibbles for each candidate. The
    # h2o_grid_info object has a post_stage that, for each row's candidate, the
    # corresponding postprocessor tuning parameters (if any)
    agua_cl <- rlang::call2(
      "agua_train_predict",
      .ns = "agua",
      static = quote(static_agua),
      grid = quote(h2o_grid),
      resample_label = quote(split_labs)
    )

    # TODO catch and log; not sure what the location should be
    agua_pred <- try(rlang::eval_bare(agua_cl), silent = TRUE)

    if (is_failure(agua_pred)) {
      next
    }

    # agua_pred <- remove_log_notes(agua_pred)

    # Add preprocessing grid back in
    # TODO maybe move this to agua
    # TODO do same for cal
    agua_pred$pred <- purrr::map(agua_pred$pred, ~ vctrs::vec_cbind(.x, pre_grid))

    has_post <- has_tailor(current_wflow)

    for (iter_model in seq_len(num_iterations_model)) {
      agua_pred_iter <- agua_pred$pred[[iter_model]]
      all_sched_post <- h2o_grid_info$post_stage[[iter_model]]

      has_post <- has_tailor(current_wflow)

      num_iterations_post <- max(nrow(all_sched_post), 1)

      # TODO pull tailor object

      for (iter_post in seq_len(num_iterations_post)) {

        current_sched_post <- all_sched_post[iter_post,]

        if (has_post) {

        } else {

        }
        # TODOs:
        # everything from current_wflow <- workflows::.fit_finalize(current_wflow)
        # on down in the regular loop

        # includes
        # finalize tailor object with any tuning parameters
        # fit tailor with correct data
        # predict tailor
        # bind remaining tuning parameter columns
        # restuff predictions back in
        # update workflow
        # extract
        # unnest on list of predictions?

      } # post loop
    } # model loop
  } # pre loop

  # ----------------------------------------------------------------------------
  # Compute metrics on each config and eval_time

  if (is.null(pred_reserve)) {
    all_metrics <- NULL
  } else {
    location <- glue::glue("internal")
    all_metrics <- .catch_and_log(
      pred_reserve |>
        dplyr::group_by(!!!rlang::syms(static$param_info$id)) |>
        .estimate_metrics(
          metric = static$metrics,
          param_names = static$param_info$id,
          outcome_name = static$y_name,
          event_level = static$control$event_level,
          metrics_info = metrics_info(static$metrics)
        ) |>
        add_configs(static),
      control = static$control,
      split_labels = split_labs,
      location = location,
      notes = notes
    )
  }

  if (!is.null(extracts)) {
    extracts <- add_configs(extracts, static) |>
      dplyr::relocate(.config, .after = .extracts) |>
      dplyr::relocate(names(grid))

    # Failing rows are not in the output:
    empty_extract <- purrr::map_lgl(extracts$.extracts, is.null)
    extracts <- extracts[!empty_extract, ]
  }

  # ----------------------------------------------------------------------------
  # Return the results

  return_tbl <- tibble::tibble(
    .metrics = list(all_metrics),
    .notes = list(notes),
    outcome_names = static$y_name
  )

  if (!is.null(static$control$extract)) {
    if (is.null(extracts)) {
      # Everything failed; return NULL for each row
      return_tbl$.extracts <- purrr::map(1:nrow(return_tbl), \(x) NULL)
    } else {
      return_tbl <- dplyr::mutate(return_tbl, .extracts = list(extracts))
    }
  }

  return_tbl <- vctrs::vec_cbind(return_tbl, split_labs)

  if (static$control$save_pred) {
    if (is.null(pred_reserve)) {
      # Everything failed; return NULL for each row
      return_tbl$.predictions <- purrr::map(1:nrow(return_tbl), \(x) NULL)
    } else {
      return_tbl$.predictions <-
        list(
          add_configs(pred_reserve, static) |>
            # Filter out joined rows that corresponded to a config that failed
            dplyr::filter(!is.na(.row)) |>
            reorder_pred_cols(static$y_name, static$param_info$id)
        )
    }
  }

  return_tbl
}

forge_all_from_workflow <- function(static, wflow) {
  x <- static[c("data", "control", "wflow", "y_name")]
  x$data$fit$data <- forge_from_workflow(x$data$fit$data, wflow)
  x$data$pred$data <- forge_from_workflow(x$data$pred$data, wflow)
  if (!is.null(x$data$cal$data)) {
    x$cal$data <- forge_from_workflow(x$data$cal$data, wflow)
  }
  x
}

