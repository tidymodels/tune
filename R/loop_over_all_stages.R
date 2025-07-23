# Notes on debugging:
# 1. You can set `options(future.debug = TRUE)` to help
# 2. If you are debugging loop_over_all_stages, use the control option
#    `allow_par = FALSE`; that will use `lapply()` so that you can see output.

loop_over_all_stages <- function(resamples, grid, static) {
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
    current_wflow <- .catch_and_log_melodie(
      finalize_fit_pre(static$wflow, current_sched_pre, static)
    )
    if (has_log_notes(current_wflow)) {
      location <- glue::glue("preprocessor {iter_pre}/{num_iterations_pre}")
      notes <- append_log_notes(notes, current_wflow, location)
      catalog_log(notes)
      if (is_failure(current_wflow)) {
        next
      }
      current_wflow <- remove_log_notes(current_wflow)
    }
    # Update y_name in case the workflow had an inline function like `log(mpg) ~ .`
    static$y_name <- outcome_names(current_wflow)

    num_iterations_model <- max(nrow(current_sched_pre$model_stage[[1]]), 1)

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
        catalog_log(notes)
        if (is_failure(current_wflow)) {
          next
        }
        current_wflow <- remove_log_notes(current_wflow)
      }

      current_grid <- rebind_grid(current_sched_pre, current_sched_model)

      has_submodel <- has_sub_param(current_sched_model$predict_stage[[1]])
      num_iterations_pred <- max(
        nrow(current_sched_model$predict_stage[[1]]),
        1
      )

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
          catalog_log(notes)
          if (is_failure(current_pred)) {
            next
          }
        }
        current_pred <- remove_log_notes(current_pred)

        has_post <- has_tailor(current_wflow)
        num_iterations_post <- max(nrow(current_sched_pred$post_stage[[1]]), 1)

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
              catalog_log(notes)
              if (is_failure(post_fit)) {
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
              catalog_log(notes)
              if (is_failure(post_pred)) {
                next
              }
              post_pred <- remove_log_notes(post_pred)
            }

            current_wflow <- set_workflow_tailor(current_wflow, post_fit)

            final_pred <- dplyr::bind_cols(post_pred, current_post_grid)
            current_extract_grid <- current_post_grid
          } else {
            # No postprocessor so just use what we have
            final_pred <- dplyr::bind_cols(current_pred, current_predict_grid)
            current_extract_grid <- current_predict_grid
          }

          current_wflow <- workflows::.fit_finalize(current_wflow)

          # --------------------------------------------------------------------
          # Allocate predictions to an overall object

          pred_iter <- pred_iter + 1
          pred_reserve <- dplyr::bind_rows(pred_reserve, final_pred)

          # --------------------------------------------------------------------
          # Extractions

          # TODO modularize this:
          if (!is.null(static$control$extract)) {
            elt_extract <- .catch_and_log_melodie(
              extract_details(current_wflow, static$control$extract)
            )

            if (is.null(extracts)) {
              extracts <- tibble::tibble(.extracts = list(1))
              if (nrow(static$param_info) > 0) {
                extracts <- tibble::add_column(current_extract_grid, .extracts = list(1))
              }
              extracts <- extracts[integer(), ]
            }

            if (has_log_notes(elt_extract)) {
              location <- glue::glue(
                "preprocessor {iter_pre}/{num_iterations_pre}, model {iter_model}/{num_iterations_model} (extracts)"
              )
              empty_notes <- new_note()
              new_notes <- append_log_notes(empty_notes, elt_extract, location)

              catalog_log(new_notes)
              notes <- dplyr::bind_rows(notes, new_notes)
            }
            elt_extract <- remove_log_notes(elt_extract)
            if (nrow(static$param_info) > 0) {
              extracts <- tibble::add_row(
                extracts,
                tibble::add_column(current_extract_grid, .extracts = list(elt_extract))
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
    all_metrics <- .catch_and_log_melodie(
      pred_reserve |>
      dplyr::group_by(!!!rlang::syms(static$param_info$id)) |>
      .estimate_metrics(
        metric = static$metrics,
        param_names = static$param_info$id,
        outcome_name = static$y_name,
        event_level = static$control$event_level,
        metrics_info = metrics_info(static$metrics)
      ) |>
      add_configs(static)
    )

    if (has_log_notes(all_metrics)) {
      location <- glue::glue(
        "internal"
      )
      empty_notes <- new_note()
      new_notes <- append_log_notes(empty_notes, all_metrics, location)

      catalog_log(new_notes)
      notes <- dplyr::bind_rows(notes, new_notes)
    }
    all_metrics <- remove_log_notes(all_metrics)
  }

  if (!is.null(extracts)) {

    extracts <- add_configs(extracts, static) |>
      dplyr::relocate(.config, .after = .extracts) |>
      dplyr::relocate(names(grid))

    # Failing rows are not in the output:
    empty_extract <- purrr::map_lgl(extracts$.extracts, is.null)
    extracts <- extracts[!empty_extract,]
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
      return_tbl$.extracts <- purrr::map(1:nrow(return_tbl), ~ NULL)
    } else {
      return_tbl <- dplyr::mutate(return_tbl, .extracts = list(extracts))
    }
  }

  return_tbl <- vctrs::vec_cbind(return_tbl, split_labs)

  if (static$control$save_pred) {
    if (is.null(pred_reserve)) {
      # Everything failed; return NULL for each row
      return_tbl$.predictions <- purrr::map(1:nrow(return_tbl), ~ NULL)
    } else {
      return_tbl$.predictions <-
        list(
          add_configs(pred_reserve, static) |>
            # Filter out joined rows that corresponded to a config that failed
            dplyr::filter(!is.na(.row))
        )
    }
  }

  return_tbl
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
