# ------------------------------------------------------------------------------
# Helpers for loop_over_all_stages()

# Note: in loop(), we add more elements for the outcome name(s), and the
# data partitions
make_static <- function(
  workflow,
  param_info,
  metrics,
  eval_time,
  split_args,
  control,
  data = list(fit = NULL, pred = NULL, cal = NULL)
) {
  # check inputs
  if (!inherits(workflow, "workflow")) {
    cli::cli_abort("{.arg workflow} should be a {.cls workflow} object.")
  }
  if (!inherits(param_info, "parameters")) {
    cli::cli_abort("{.arg param_info} should be a {.cls parameters} object.")
  }
  if (!inherits(metrics, "metric_set")) {
    cli::cli_abort("{.arg metrics} should be a {.cls metric_set} object.")
  }
  if (!check_class_or_null(eval_time, "numeric")) {
    cli::cli_abort("{.arg eval_time} should be a numeric vector.")
  }

  list(
    wflow = workflow,
    param_info = param_info,
    post_estimation = workflows::.workflow_includes_calibration(workflow),
    metrics = metrics,
    metric_info = tibble::as_tibble(metrics),
    pred_types = determine_pred_types(workflow, metrics),
    eval_time = eval_time,
    split_args = split_args,
    control = control,
    data = data
  )
}

check_static_data <- function(x, elem = "fit") {
  if (is.null(x)) {
    return(x)
  }

  nms <- sort(names(x))
  if (!identical(nms, c("data", "ind"))) {
    cli::cli_abort(
      "{.arg data_*} arguments should have names {.val data} and
                   {.val ind}, not {.val {nms}} in the {.field {elem}} slot."
    )
  }

  if (!is.integer(x[["ind"]])) {
    cli::cli_abort(
      "Element {.arg ind} should be an integer in the
                   {.field {elem}} slot."
    )
  }

  if (!is.data.frame(x[["data"]])) {
    cli::cli_abort(
      "Element {.arg data} should be a tibble in the {.field {elem}} slot."
    )
    if (!tibble::is_tibble(x[["data"]])) {
      x[["data"]] <- tibble::as_tibble(x[["data"]])
    }
  }

  x
}

get_data_subsets <- function(wflow, split, split_args = NULL) {
  fit_lst <- pred_lst <- cal_lst <- list(data = NULL, ind = NULL)
  pred_lst$data <- rsample::assessment(split)
  pred_lst$ind <- as.integer(split, data = "assessment")
  if (workflows::.workflow_includes_calibration(wflow)) {
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

    cal_lst$ind <- as.integer(split, data = "assessment")
    cal_lst$data <- vctrs::vec_slice(split$data, cal_lst$ind)
  } else {
    cal_lst <- NULL
  }

  fit_lst$data <- rsample::analysis(split)
  fit_lst$ind <- as.integer(split, data = "analysis")
  list(fit = fit_lst, pred = pred_lst, cal = cal_lst)
}

update_static <- function(x, data) {
  check_static_data(data$fit)
  check_static_data(data$pred)
  check_static_data(data$cal)
  x$data <- data
  x
}

# ------------------------------------------------------------------------------
# For stages

remove_stage <- function(x) {
  stages <- c("model_stage", "predict_stage", "post_stage")
  x[, !(names(x) %in% stages)]
}

has_pre_param <- function(x) {
  any(names(x) != "model_stage")
}

has_mod_param <- function(x) {
  any(names(x) != "predict_stage")
}

# ------------------------------------------------------------------------------

# This is run on a `predict_stage` column:
has_sub_param <- function(x) {
  not_post_list <- names(x) != "post_stage"
  has_param_col <- any(not_post_list)
  if (!has_param_col) {
    return(FALSE)
  }
  param_col_nm <- names(x)[not_post_list]
  param_col <- x[[param_col_nm]]
  two_plus_vals <- length(param_col) > 1
  two_plus_vals
}

# This is run on a `predict_stage` column:
get_sub_param <- function(x) {
  not_post_list <- names(x) != "post_stage"
  names(x)[not_post_list]
}

# ------------------------------------------------------------------------------
# from workflows
# nocov start
has_tailor <- function(x) {
  "tailor" %in% names(x$post$actions)
}

has_tailor_tuned <- function(x) {
  if (!has_tailor(x)) {
    res <- FALSE
  } else {
    res <- any(tune_args(x)$source == "tailor")
  }
  res
}
has_tailor_estimated <- function(x) {
  if (!has_tailor(x)) {
    res <- FALSE
  } else {
    post <- hardhat::extract_postprocessor(x)
    res <- tailor::tailor_requires_fit(post)
  }
  res
}
# nocov end

# ------------------------------------------------------------------------------
# Prediction and postprocessing

finalize_fit_post <- function(wflow_current, predictions, grid = NULL) {
  if (is.null(grid)) {
    grid <- dplyr::tibble()
  }

  post_obj <- hardhat::extract_postprocessor(wflow_current) |>
    finalize_tailor(grid)

  outputs <- get_output_columns(wflow_current)

  post_obj <- post_obj |>
    fit(
      .data = predictions,
      outcome = !!outputs$outcome[[1]],
      estimate = !!outputs$estimate[[1]],
      probabilities = c(!!!outputs$probabilities)
    )

  post_obj
}

# ------------------------------------------------------------------------------

predict_all_types <- function(
  wflow_fit,
  static,
  submodel_grid = NULL,
  predictee = "assessment"
) {
  predictee <- rlang::arg_match(predictee, c("assessment", "calibration"))
  outputs <- get_output_columns(wflow_fit)

  if (predictee == "calibration" && static$post_estimation) {
    if (is.null(static$data$cal)) {
      cli::cli_abort(
        "Calibration data were requested but not reserved.",
        call = NULL
      )
    }
    .data <- static$data$cal$data
    .ind <- static$data$cal$ind
  } else {
    .data <- static$data$pred$data
    .ind <- static$data$pred$ind
  }

  processed_data_pred <- forge_from_workflow(.data, wflow_fit)
  processed_data_pred$outcomes <- processed_data_pred$outcomes |>
    dplyr::mutate(.row = .ind)

  sub_param <- names(submodel_grid)

  # Convert argument names to parsnip format see #1011
  submodel_grid <- engine_to_parsnip(static$wflow, submodel_grid)

  pred <- NULL
  for (type_iter in static$pred_types) {
    tmp_res <- predict_wrapper(
      model = wflow_fit |> hardhat::extract_fit_parsnip(),
      new_data = processed_data_pred$predictors,
      type = type_iter,
      eval_time = static$eval_time,
      subgrid = submodel_grid
    )
    tmp_res$.row <- .ind

    # predict_wrapper() is designed to predict all submodels at once; we get a
    # list column back called .pred with a single row. Collapse that and remove
    # the submodel column since it is in the current grid.
    if (length(sub_param) > 0) {
      tmp_res <- tidyr::unnest(tmp_res, cols = c(.pred))
    }

    # Now go back to engine names
    tmp_res <- parsnip_to_engine(static$wflow, tmp_res)

    if (is.null(pred)) {
      pred <- tmp_res
    } else {
      pred <- dplyr::full_join(pred, tmp_res, by = c(sub_param, ".row"))
    }
  }

  pred <- pred |>
    dplyr::full_join(processed_data_pred$outcomes, by = ".row")

  pred
}

# ------------------------------------------------------------------------------
# Fitting/training functions

finalize_fit_pre <- function(wflow_current, grid, static) {
  pre_proc <- hardhat::extract_preprocessor(wflow_current)

  if (inherits(pre_proc, "recipe")) {
    grid <- remove_stage(grid)
    pre_proc_param <- hardhat::extract_parameter_set_dials(pre_proc)
    pre_proc_id <- pre_proc_param$id

    if (length(pre_proc_id) > 0) {
      grid <- grid[, pre_proc_id]
      pre_proc <- finalize_recipe(pre_proc, grid)
      wflow_current <- set_workflow_recipe(wflow_current, pre_proc)
    }
  }
  workflows::.fit_pre(wflow_current, static$data$fit$data)
}

finalize_fit_model <- function(wflow_current, grid) {
  mod_spec <- hardhat::extract_spec_parsnip(wflow_current)

  grid <- remove_stage(grid)
  mod_param <- hardhat::extract_parameter_set_dials(mod_spec)
  mod_id <- mod_param$id

  if (length(mod_id) > 0) {
    grid <- grid[, mod_id]
    mod_spec <- finalize_model(mod_spec, grid)
    wflow_current <- set_workflow_spec(wflow_current, mod_spec)
  }

  # .catch_and_log_melodie_fit()
  .fit_model(wflow_current, workflows::control_workflow())
}

# ------------------------------------------------------------------------------
# Misc functions

rebind_grid <- function(...) {
  list(...) |> purrr::map(remove_stage) |> purrr::list_cbind()
}

get_output_columns <- function(x) {
  # This needs a fitted model or workflow
  pred_cols <- parsnip::.get_prediction_column_names(x, syms = TRUE)
  res <- c(list(outcome = rlang::syms(outcome_names(x))), pred_cols)
  res
}

# ------------------------------------------------------------------------------
# pre-allocating predictions

initialize_pred_reserve <- function(predictions, grid_size) {
  if (!tibble::is_tibble(predictions)) {
    predictions <- dplyr::as_tibble(predictions)
  }
  grid_size <- max(1, grid_size)
  ptype <- predictions[0, ]
  size <- nrow(predictions) * grid_size
  res <- ptype[1:size, ]
  dplyr::as_tibble(res)
}

replace_reserve_rows <- function(iter, chunk) {
  start_loc <- (iter - 1) * chunk + 1
  end_loc <- iter * chunk
  start_loc:end_loc
}

update_reserve <- function(reserve, iter, predictions, grid_size) {
  grid_size <- min(1, grid_size)
  pred_size <- nrow(predictions)

  if (is.null(reserve)) {
    reserve <- initialize_pred_reserve(predictions, grid_size)
  } else {
    if (tibble::is_tibble(predictions)) {
      predictions <- dplyr::as_tibble(predictions)
    }
  }
  reserve[replace_reserve_rows(iter, pred_size), ] <- predictions
  reserve
}

# ------------------------------------------------------------------------------
# Add .config to grid

get_config_key <- function(grid, wflow) {
  info <- tune_args(wflow)
  key <- grid

  only_param <- setdiff(info$id, names(grid))
  if (length(only_param) > 0) {
    cli::cli_abort(
      "Some parameters are tagged for tuning but are not in the grid:
      {.arg {only_param}}",
      call = NULL
    )
  }

  only_grid <- setdiff(names(grid), info$id)
  if (length(only_grid) > 0) {
    cli::cli_abort(
      "Some parameters are in the grid but are not tagged for tuning:
      {.arg {only_grid}}",
      call = NULL
    )
  }

  pre_param <- info$id[info$source == "recipe"]
  if (length(pre_param) > 0) {
    key <- make_config_labs(grid, pre_param) |>
      dplyr::full_join(key, by = pre_param)
  } else {
    key <- key |>
      dplyr::mutate(pre = "pre0")
  }

  mod_param <- info$id[info$source == "model_spec"]
  if (length(mod_param) > 0) {
    key <- make_config_labs(grid, mod_param, "mod") |>
      dplyr::full_join(key, by = mod_param)
  } else {
    key <- key |>
      dplyr::mutate(mod = "mod0")
  }

  post_param <- info$id[info$source == "tailor"]
  if (length(post_param) > 0) {
    key <- make_config_labs(grid, post_param, "post") |>
      dplyr::full_join(key, by = post_param)
  } else {
    key <- key |>
      dplyr::mutate(post = "post0")
  }

  key$.config <- paste(key$pre, key$mod, key$post, sep = "_")
  key$.config <- gsub("_$", "", key$.config)
  key |>
    dplyr::arrange(.config) |>
    dplyr::select(dplyr::all_of(info$id), .config)
}

make_config_labs <- function(grid, param, val = "pre") {
  res <- grid |>
    dplyr::select(dplyr::all_of(param)) |>
    dplyr::distinct() |>
    dplyr::arrange(!!!rlang::syms(param)) |>
    dplyr::mutate(
      num = format(dplyr::row_number()),
      num = gsub(" ", "0", num),
      {{ val }} := paste0(val, num)
    ) |>
    dplyr::select(-num)

  res
}

determine_pred_types <- function(wflow, metrics) {
  model_mode <- extract_spec_parsnip(wflow)$mode

  pred_types <- unique(metrics_info(metrics)$type)
  if (has_tailor(wflow)) {
    post <- extract_postprocessor(wflow)
    post_out <- purrr::map(post$adjustments, ~ .x$outputs)
    post_in <- purrr::map(post$adjustments, ~ .x$inputs)
    post_types <- unlist(c(post_out, post_in))
    post_types[grepl("probability", post_types)] <- "prob"
    post_cls <- purrr::map(post$adjustments, class)
    post_cls <- unlist(post_cls)
    if (any(post_cls == "probability_calibration")) {
      post_types <- c(post_types, "class", "prob")
    }
    post_cls <- unique(post_cls)
    pred_types <- unique(c(pred_types, post_types))
  }

  if (any(pred_types == "everything")) {
    if (model_mode == "regression") {
      pred_types <- c(pred_types, "numeric")
    } else if (model_mode == "classification") {
      pred_types <- c(pred_types, "class", "prob")
    } else if (model_mode == "censored regression") {
      pred_types <- c(
        pred_types,
        "static_survival_metric",
        "dynamic_survival_metric"
      )
    } else {
      cli::cli_abort(
        "No prediction types are known for mode {.val model_mode}."
      )
    }

    pred_types <- pred_types[pred_types != "everything"]
  }

  sort(unique(pred_types))
}

reorder_pred_cols <- function(x, y_name) {
  # x |>
  #   dplyr::relocate(dplyr::any_of(".row"), .before = dplyr::everything()) |>
  #   dplyr::relocate(dplyr::any_of(".eval_time"), .before = dplyr::everything()) |>
  #   dplyr::relocate(dplyr::matches(".pred_[A-Za-z]"), .before = dplyr::everything()) |>
  #   dplyr::relocate(dplyr::matches("^\\.pred_class$"), .before = dplyr::everything()) |>
  #   dplyr::relocate(dplyr::matches("^\\.pred_time$"), .before = dplyr::everything()) |>
  #   dplyr::relocate(dplyr::matches("^\\.pred$"), .before = dplyr::everything()) |>
  #   dplyr::relocate(dplyr::all_of(y_name), .before = dplyr::everything())

  x |>
    dplyr::relocate(
      dplyr::all_of(y_name),
      dplyr::matches("^\\.pred_time$"),
      dplyr::matches("^\\.pred$"),
      dplyr::matches("^\\.pred_class$"),
      dplyr::matches(".pred_[A-Za-z]"),
      dplyr::any_of(".eval_time"),
      dplyr::any_of(".row"),
      .before = dplyr::everything()
    )
}

engine_to_parsnip <- function(wflow, grid) {
  grid_nm <- names(grid)
  key <- parsnip::.model_param_name_key(wflow) |>
    dplyr::filter(user != parsnip & user %in% grid_nm) |>
    dplyr::select(-engine)

  if (nrow(key) == 0) {
    return(grid)
  }
  nm_lst <- key$user
  names(nm_lst) <- key$parsnip
  dplyr::rename(grid, dplyr::all_of(nm_lst))
}

parsnip_to_engine <- function(wflow, grid) {
  grid_nm <- names(grid)
  key <- parsnip::.model_param_name_key(wflow) |>
    dplyr::filter(user != parsnip & parsnip %in% grid_nm) |>
    dplyr::select(-engine)

  if (nrow(key) == 0) {
    return(grid)
  }
  nm_lst <- key$parsnip
  names(nm_lst) <- key$user
  dplyr::rename(grid, dplyr::all_of(nm_lst))
}
