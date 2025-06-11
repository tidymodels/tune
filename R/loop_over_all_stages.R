loop_over_all_stages <- function(resamples, grid, static) {
  # Initialize some objects

  split <- resamples$splits[[1]]
  split_labs <- resamples |>
    dplyr::select(dplyr::starts_with("id"))

  pred_reserve <- NULL
  pred_iter <- 0
  notes <- tibble::tibble(
    location = character(),
    type = character(),
    note = character()
  )
  extracts <- NULL

  sched <- schedule_grid(grid, static$wflow)

  config_tbl <- get_config_key(grid, static$wflow)

  # Append data partitions here; these are the same for the duration of this function
  data_splits <- get_data_subsets(static$wflow, split, static$split_args)
  static <- update_static(static, data_splits)

  # Now that we have data, determine the names of the outcome data
  static$y_name <- outcome_names(static$wflow, data = split$data)

  # ----------------------------------------------------------------------------
  # Iterate over preprocessors

  num_iterations_pre <- nrow(sched)

  for (iter_pre in seq_len(num_iterations_pre)) {
    current_sched_pre <- sched[iter_pre, ]
    current_wflow <- .catch_and_log_melodie(
      finalize_fit_pre(static$wflow, current_sched_pre, static)
    )
    if (has_log_notes(current_wflow)) {
      location <- glue::glue("preprocessor {iter_pre}/{num_iterations_pre}")
      notes <- append_log_notes(notes, current_wflow, location)
      catalog_log(notes)
      if (is_failure_melodie(current_wflow)) {
        next
      }
      current_wflow <- remove_log_notes(current_wflow)
    }

    num_iterations_model <- nrow(current_sched_pre$model_stage[[1]])

    # --------------------------------------------------------------------------
    # Iterate over model parameters

    # Make a copy of the current workflow so that we can finalize it multiple
    # times, since finalize_*() functions will not update parameters whose
    # values currently are tune()
    pre_wflow <- current_wflow

    for (iter_model in seq_len(num_iterations_model)) {
      current_sched_model <- current_sched_pre$model_stage[[1]][iter_model, ]

      # Splice in any parameters marked for tuning and fit the model
      current_wflow <- .catch_and_log_melodie(
        finalize_fit_model(pre_wflow, current_sched_model)
      )

      if (has_log_notes(current_wflow)) {
        location <- glue::glue("model {iter_model}/{num_iterations_model}")
        notes <- append_log_notes(notes, current_wflow, location)
        if (is_failure_melodie(current_wflow)) {
          next
        }
        current_wflow <- remove_log_notes(current_wflow)
      }

      current_grid <- rebind_grid(current_sched_pre, current_sched_model)

      has_submodel <- has_sub_param(current_sched_model$predict_stage[[1]])
      num_iterations_pred <- nrow(current_sched_model$predict_stage[[1]])

      # --------------------------------------------------------------------------
      # Iterate over prediction submodels

      for (iter_pred in seq_len(num_iterations_pred)) {
        # cli::cli_inform("Predicting {iter_pred} of {num_iterations_pred}")

        current_sched_pred <- current_sched_model$predict_stage[[1]][
          iter_pred,
        ]

        if (has_submodel) {
          sub_nm <- get_sub_param(current_sched_pred)
          sub_grid <- current_sched_pred[, sub_nm]

          # The assigned submodel parameter (from min_grid()) is in the
          # current grid. Remove that and add the one that we are predicting on

          current_grid <- current_grid |>
            dplyr::select(-dplyr::all_of(sub_nm)) |>
            rebind_grid(current_sched_pred)

          # Remove the submodel column since it is in the currrent grid.
          current_pred <- .catch_and_log_melodie(
            predict_all_types(current_wflow, static, sub_grid) |>
              dplyr::select(-dplyr::all_of(sub_nm))
          )
        } else {
          current_pred <- .catch_and_log_melodie(
            predict_all_types(current_wflow, static)
          )
        }

        if (has_log_notes(current_pred)) {
          location <- glue::glue("prediction {iter_pred}/{num_iterations_pred}")
          notes <- append_log_notes(notes, current_pred, location)
          if (is_failure_melodie(current_pred)) {
            next
          }
          current_pred <- remove_log_notes(current_pred)
        }

        has_post <- has_tailor(current_wflow)
        num_iterations_post <- nrow(current_sched_pred$post_stage[[1]])

        # ----------------------------------------------------------------------
        # Iterate over postprocessors

        current_predict_grid <- current_grid

        for (iter_post in seq_len(num_iterations_post)) {
          # cli::cli_inform("-- Postprocessing {iter_post} of {num_iterations_post}")

          if (has_post) {
            current_sched_post <-
              current_sched_pred$post_stage[[1]][iter_post, ]
            post_grid <- current_sched_post

            current_post_grid <- rebind_grid(
              current_predict_grid,
              current_sched_post
            )

            # make data for prediction
            if (has_tailor_estimated(current_wflow)) {
              tailor_train_data <- predict_all_types(
                current_wflow,
                static,
                predictee = "calibration"
              )
            } else {
              tailor_train_data <- current_pred[0, ]
            }

            post_fit <- .catch_and_log_melodie(
              finalize_fit_post(
                current_wflow,
                predictions = tailor_train_data,
                grid = post_grid
              )
            )

            if (has_log_notes(post_fit)) {
              location <- glue::glue(
                "postprocessing {iter_pred}/{num_iterations_pred}"
              )
              notes <- append_log_notes(notes, post_fit, location)
              if (is_failure_melodie(post_fit)) {
                next
              }
              post_fit <- remove_log_notes(post_fit)
            }

            post_pred <- .catch_and_log_melodie(
              predict(post_fit, current_pred)
            )

            if (has_log_notes(post_pred)) {
              location <- glue::glue(
                "postprocessing {iter_pred}/{num_iterations_pred}"
              )
              notes <- append_log_notes(notes, post_pred, location)
              if (is_failure_melodie(post_pred)) {
                next
              }
              post_pred <- remove_log_notes(post_pred)
            }

            current_wflow <- set_workflow_tailor(current_wflow, post_fit)
            final_pred <- dplyr::bind_cols(post_pred, current_post_grid)
          } else {
            # No postprocessor so just use what we have
            final_pred <- dplyr::bind_cols(current_pred, current_predict_grid)
          }

          # --------------------------------------------------------------------
          # Allocate predictions to an overall object

          pred_iter <- pred_iter + 1
          pred_reserve <- dplyr::bind_rows(pred_reserve, final_pred)

          # --------------------------------------------------------------------
          # Extractions
          if (!is.null(static$control$extract)) {
            elt_extract <- .catch_and_log_melodie(
              extract_details(current_wflow, static$control$extract)
            )
  
            if (has_log_notes(elt_extract)) {
              location <- glue::glue(
                "extraction"
              )
              notes <- append_log_notes(notes, elt_extract, location)
              if (is_failure_melodie(elt_extract)) {
                next
              }
            }
            elt_extract <- remove_log_notes(elt_extract)
            extracts <- c(extracts, list(elt_extract))
          }
        } # post loop
      } # predict loop
    } # model loop
  } # pre loop

  # ----------------------------------------------------------------------------
  # Compute metrics on each config and eval_time

  if (is.null(pred_reserve)) {
    all_metrics <- NULL
  } else {
    all_metrics <- pred_reserve |>
      dplyr::group_by(!!!rlang::syms(static$param_info$id)) |>
      .estimate_metrics(
        metric = static$metrics,
        param_names = static$param_info$id,
        outcome_name = static$y_name,
        event_level = static$control$event_level,
        metrics_info = metrics_info(static$metrics)
      ) |>
      dplyr::full_join(config_tbl, by = static$param_info$id) |>
      dplyr::arrange(.config)
  }

  if (!is.null(extracts)) {
    extracts <- config_tbl |>
      dplyr::mutate(.extracts = extracts) |>
      dplyr::relocate(.config, .after = .extracts)
  }

  # ----------------------------------------------------------------------------
  # Return the results

  return_list <- tibble::tibble(
    .metrics = list(all_metrics),
    .notes = list(notes)
  )
  
  if (!is.null(extracts)) {
    return_list <- dplyr::mutate(return_list, .extracts = list(extracts))
  }

  return_list <- vctrs::vec_cbind(return_list, split_labs)

  if (static$control$save_pred) {
    return_list$.predictions <- list(
      pred_reserve |>
        dplyr::full_join(config_tbl, by = static$param_info$id) |>
        dplyr::arrange(.config)
    )
  }

  return_list
}

loop_over_all_stages2 <- function(index, resamples, grid, static) {
  loop_over_all_stages(resamples[[index$b]], grid[[index$s]], static)
}

# ------------------------------------------------------------------------------

# This will take a grid and make a list of subgrids that should be used when
# we parallel process over grid candidates. The function will make 1-row grids
# except when there is a submodel parameter. In that case, it will create a
# subgrid that has fixed values for non-submodel parameters and the associated
# values of the submodel.
get_row_wise_grid <- function(wflow, grid) {
  param_tuned <- tune_args(wflow)$id
  submodel <- wflow |>
    hardhat::extract_spec_parsnip() |>
    get_submodel_info() |>
    dplyr::filter(has_submodel) |>
    purrr::pluck("id")

  const_param <- setdiff(param_tuned, submodel)
  const_param <- rlang::syms(const_param)

  if (length(submodel) == 0) {
    inds <- seq_len(nrow(grid))
  } else {
    grid_inds <- grid |>
      parsnip::add_rowindex() |>
      dplyr::group_nest(!!!const_param) |>
      dplyr::mutate(inds = dplyr::row_number()) |>
      tidyr::unnest(c(data)) |>
      dplyr::select(-.row)
    grid <- grid_inds[, param_tuned]
    inds <- grid_inds$inds
  }
  vctrs::vec_split(grid, inds)$val
}
