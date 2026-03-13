# Notes for easier reading are in `inst/loop.qmd`.
# Notes on debugging:
# 1. You can set `options(future.debug = TRUE)` to help
# 2. If you are debugging .loop_over_all_stages, use the control option
#    `allow_par = FALSE`; that will use `lapply()` so that you can see output.

#' @export
#' @keywords internal
#' @rdname empty_ellipses
.loop_over_all_stages <- function(resamples, grid, static) {
  # Some packages may use random numbers so attach them prior to initializing
  # the RNG seed
  attach_pkgs(static$pkgs, strategy = static$strategy)

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
  notes <- new_note()
  extracts <- NULL

  sched <- schedule_grid(grid, static$wflow)

  config_tbl <- static$configs

  # Append data partitions here; these are the same for the duration of this function
  data_splits <- .get_data_subsets(static$wflow, split, static$split_args)
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
    current_grid <- remove_stage(current_sched_pre)

    location <- glue::glue("preprocessor {iter_pre}/{num_iterations_pre}")

    # Note: finalize_fit_pre() will process the data used for modeling. We'll
    # also need to process the data used for prediction in the same way. That
    # will happen below (via process_prediction_data()).

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

    # Now we can process the data being predicted. These do not change
    # over the next two loops, so compute them here, once.
    location <- glue::glue(
      "preprocessor {iter_pre}/{num_iterations_pre} (prediction data)"
    )
    pred_data <-
      .catch_and_log(
        process_prediction_data(current_wflow, static),
        control = static$control,
        split_labels = split_labs,
        location = location,
        notes = notes
      )

    # Also process the calibration data (if needed for postprocessor fitting).
    cal_pred_data <- NULL
    if (!is.null(static$data$cal)) {
      cal_pred_data <- .catch_and_log(
        process_prediction_data(current_wflow, static, source = "cal"),
        control = static$control,
        split_labels = split_labs,
        location = location,
        notes = notes
      )
    }

    # Update y_name in case the workflow had an inline function like `log(mpg) ~ .`
    static$y_name <- outcome_names(current_wflow)

    num_iterations_model <- max(nrow(current_sched_pre$model_stage[[1]]), 1)

    # --------------------------------------------------------------------------
    # Iterate over model parameters

    # Make a copy of the current workflow so that we can finalize it multiple
    # times, since finalize_*() functions will only update parameters whose
    # values currently are tune()
    wflow_with_fitted_pre <- current_wflow

    grid_with_pre <- current_grid

    if (is_failure(pred_data)) {
      next
    }

    for (iter_model in seq_len(num_iterations_model)) {
      current_sched_model <- current_sched_pre$model_stage[[1]][iter_model, ]
      current_grid <- extend_grid(grid_with_pre, current_sched_model)

      # Splice in any parameters marked for tuning and fit the model
      location <- glue::glue(
        "preprocessor {iter_pre}/{num_iterations_pre}, model {iter_model}/{num_iterations_model}"
      )

      current_wflow <- .catch_and_log(
        finalize_fit_model(wflow_with_fitted_pre, current_sched_model),
        control = static$control,
        split_labels = split_labs,
        location = location,
        notes = notes
      )

      if (is_failure(current_wflow)) {
        next
      }

      has_submodel <- has_sub_param(current_sched_model$predict_stage[[1]])
      num_iterations_pred <- max(
        nrow(current_sched_model$predict_stage[[1]]),
        1
      )

      # --------------------------------------------------------------------------
      # Predict all submodels at once (if applicable), then iterate over them
      # for postprocessing

      if (has_submodel) {
        # Collect all submodel values and predict once
        sched_pred_all_submodels <- current_sched_model$predict_stage[[1]]
        sub_nm <- get_sub_param(sched_pred_all_submodels)
        grid_pred_all_submodels <- sched_pred_all_submodels[,
          sub_nm,
          drop = FALSE
        ]

        # Submodel parameters will be added in the predict stage
        grid_with_pre_model <- current_grid |>
          dplyr::select(-dplyr::all_of(sub_nm))

        location <- glue::glue(
          "preprocessor {iter_pre}/{num_iterations_pre}, model {iter_model}/{num_iterations_model} (predictions)"
        )
        pred_all_submodels <- .catch_and_log(
          predict_all_types(
            current_wflow,
            pred_data,
            static,
            grid_pred_all_submodels
          ),
          control = static$control,
          split_labels = split_labs,
          location = location,
          notes = notes
        )

        if (is_failure(pred_all_submodels)) {
          next
        }
        pred_all_submodels <- remove_log_notes(pred_all_submodels)

        # Also predict on calibration data for all submodels at once
        cal_pred_all_submodels <- NULL
        if (!is.null(cal_pred_data) && !is_failure(cal_pred_data)) {
          cal_pred_all_submodels <- .catch_and_log(
            predict_all_types(
              current_wflow,
              cal_pred_data,
              static,
              grid_pred_all_submodels,
              source = "cal"
            ),
            control = static$control,
            split_labels = split_labs,
            location = location,
            notes = notes
          )

          if (!is_failure(cal_pred_all_submodels)) {
            cal_pred_all_submodels <- remove_log_notes(cal_pred_all_submodels)
          }
        }
      }

      for (iter_pred in seq_len(num_iterations_pred)) {
        current_sched_pred <- current_sched_model$predict_stage[[1]][
          iter_pred,
        ]

        if (has_submodel) {
          sub_nm <- get_sub_param(current_sched_pred)
          sub_val <- current_sched_pred[[sub_nm]]

          # Add submodel param to grid
          current_grid <- extend_grid(grid_with_pre_model, current_sched_pred)

          # Filter to this submodel's predictions (already computed above)
          current_pred <- pred_all_submodels |>
            dplyr::filter(.data[[sub_nm]] == sub_val) |>
            dplyr::select(-dplyr::all_of(sub_nm))
        } else {
          location <- glue::glue(
            "preprocessor {iter_pre}/{num_iterations_pre}, model {iter_model}/{num_iterations_model} (predictions)"
          )
          current_pred <- .catch_and_log(
            predict_all_types(current_wflow, pred_data, static),
            control = static$control,
            split_labels = split_labs,
            location = location,
            notes = notes
          )

          if (is_failure(current_pred)) {
            next
          }
          current_pred <- remove_log_notes(current_pred)
        }

        has_post <- has_tailor(current_wflow)
        num_iterations_post <- max(nrow(current_sched_pred$post_stage[[1]]), 1)

        # Compute calibration predictions for this predict iteration.
        # For submodels these were pre-computed above; for non-submodels we
        # predict here (once per model, shared across post iterations).
        current_cal_pred <- NULL
        if (static$post_estimation) {
          if (has_submodel) {
            if (
              !is.null(cal_pred_all_submodels) &&
                !is_failure(cal_pred_all_submodels)
            ) {
              current_cal_pred <- cal_pred_all_submodels |>
                dplyr::filter(.data[[sub_nm]] == sub_val) |>
                dplyr::select(-dplyr::all_of(sub_nm))
            }
          } else if (!is.null(cal_pred_data) && !is_failure(cal_pred_data)) {
            current_cal_pred <- .catch_and_log(
              predict_all_types(
                current_wflow,
                cal_pred_data,
                static,
                source = "cal"
              ),
              control = static$control,
              split_labels = split_labs,
              location = glue::glue(
                "preprocessor {iter_pre}/{num_iterations_pre}, model {iter_model}/{num_iterations_model} (calibration predictions)"
              ),
              notes = notes
            )
            if (!is_failure(current_cal_pred)) {
              current_cal_pred <- remove_log_notes(current_cal_pred)
            }
          }
        }

        # ----------------------------------------------------------------------
        # Iterate over postprocessors

        # Make a copy of the current workflow so that we can finalize it multiple
        # times, since finalize_*() functions will only update parameters whose
        # values currently are tune()
        wflow_with_fitted_pre_and_model <- current_wflow

        grid_with_pre_model_pred <- current_grid

        for (iter_post in seq_len(num_iterations_post)) {
          if (has_post) {
            current_sched_post <-
              current_sched_pred$post_stage[[1]][iter_post, ]

            current_grid <- extend_grid(
              grid_with_pre_model_pred,
              current_sched_post
            )

            location <- glue::glue(
              "preprocessor {iter_pre}/{num_iterations_pre}, model {iter_model}/{num_iterations_model}, postprocessing {iter_pred}/{num_iterations_pred}"
            )

            # If the postprocessor requires fitting (e.g. calibration), use
            # pre-computed calibration predictions. Otherwise, use the raw
            # training data (no data leakage since no fitting occurs).
            current_wflow <- .catch_and_log(
              if (static$post_estimation) {
                finalize_fit_post(
                  wflow_with_fitted_pre_and_model,
                  grid = current_sched_post,
                  cal_predictions = current_cal_pred
                )
              } else {
                finalize_fit_post(
                  wflow_with_fitted_pre_and_model,
                  data_calibration = static$data$fit$data,
                  grid = current_sched_post
                )
              },
              control = static$control,
              split_labels = split_labs,
              location = location,
              notes = notes
            )
            if (is_failure(current_wflow)) {
              next
            }

            # to predict, use the post-processor directly rather than the
            # workflow so that we don't have to generate the model predictions
            # a second time
            post_fit <- extract_postprocessor(current_wflow, estimated = TRUE)
            post_pred <- .catch_and_log(
              predict(post_fit, current_pred),
              control = static$control,
              split_labels = split_labs,
              location = location,
              notes = notes
            )
            if (is_failure(post_pred)) {
              next
            }

            final_pred <- dplyr::bind_cols(post_pred, current_grid)
          } else {
            # No postprocessor so just use what we have
            final_pred <- dplyr::bind_cols(current_pred, current_grid)
          }

          current_wflow <- workflows::.fit_finalize(current_wflow)

          # --------------------------------------------------------------------
          # Allocate predictions to an overall object

          pred_reserve <- dplyr::bind_rows(pred_reserve, final_pred)

          # --------------------------------------------------------------------
          # Extractions

          # TODO modularize this:
          if (!is.null(static$control$extract)) {
            location <- glue::glue(
              "preprocessor {iter_pre}/{num_iterations_pre}, model {iter_model}/{num_iterations_model} (extracts)"
            )
            elt_extract <- .catch_and_log(
              extract_details(current_wflow, static$control$extract),
              control = static$control,
              split_labels = split_labs,
              location = location,
              notes = notes
            )

            if (is.null(extracts)) {
              extracts <- tibble::tibble(.extracts = list(1))
              if (nrow(static$param_info) > 0) {
                extracts <- tibble::add_column(
                  current_grid,
                  .extracts = list(1)
                )
              }
              extracts <- extracts[integer(), ]
            }

            if (nrow(static$param_info) > 0) {
              extracts <- tibble::add_row(
                extracts,
                tibble::add_column(
                  current_grid,
                  .extracts = list(elt_extract)
                )
              )
            } else {
              extracts <- tibble::add_row(
                extracts,
                tibble::tibble(.extracts = list(elt_extract))
              )
            }
            if (is_failure(elt_extract)) {
              next
            }
          }

          # Output for these loops:
          # - pred_reserve (probably not null)
          # - extracts (may be null)
          # - notes
        } # post loop
      } # predict loop
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

loop_over_all_stages2 <- function(index, resamples, grid, static) {
  .loop_over_all_stages(resamples[[index$b]], grid[[index$s]], static)
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

# ------------------------------------------------------------------------------

add_configs <- function(x, static) {
  config_tbl <- static$configs
  if (length(static$param_info$id) > 0) {
    x <- dplyr::left_join(x, config_tbl, by = static$param_info$id)
  } else {
    x <- dplyr::bind_cols(x, config_tbl)
  }

  dplyr::arrange(x, .config)
}
