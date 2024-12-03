
predict_model <- function(new_data, orig_rows, workflow, grid, metrics,
                          submodels = NULL, metrics_info, eval_time = NULL) {

  model <- extract_fit_parsnip(workflow)

  forged <- forge_from_workflow(new_data, workflow)
  x_vals <- forged$predictors
  y_vals <- forged$outcomes

  # TODO patch since parsnip does not record the column names when Surv objects
  # are used with fit_xy()
  if (model$spec$mode == "censored regression") {
    model$preproc$y_var <- names(y_vals)
  }

  if (length(orig_rows) != nrow(x_vals)) {
    msg <- "Some assessment set rows are not available at prediction time."

    if (has_preprocessor_recipe(workflow)) {
      msg <-
        c(
          msg,
          i =
            "Consider using {.code skip = TRUE} on any recipe steps that
             remove rows to avoid calling them on the assessment set."

        )
    } else {
      msg <- c(msg, i = "Did your preprocessing steps filter or remove rows?")
    }

    cli::cli_abort(msg)
  }

  # Determine the type of prediction that is required
  types <- unique(metrics_info$type)

  res <- NULL
  merge_vars <- c(".row", names(grid))

  for (type_iter in types) {
    # Regular predictions

    tmp_res <- predict_wrapper(model, x_vals, type_iter, eval_time)
    tmp_res$.row <- orig_rows
    tmp_res <- vctrs::vec_cbind(tmp_res, grid)


    if (!is.null(submodels)) {
      submod_length <- lengths(submodels)
      has_submodels <- any(submod_length > 0)

      if (has_submodels) {
        submod_param <- names(submodels)
        subgrid <- make_submod_arg(grid, model, submodels)

        tmp_sub <- predict_wrapper(model, x_vals, type_iter, eval_time, subgrid)
        tmp_sub$.row <- orig_rows
        tmp_sub <- unnest(tmp_sub, cols = dplyr::starts_with(".pred"))

        grid_bind <- grid
        grid_bind[, submod_param] <- NULL

        tmp_sub <- vctrs::vec_cbind(tmp_sub, grid_bind)
        rownames(tmp_sub) <- NULL
        tmp_sub <- dplyr::rename(tmp_sub, !!!make_rename_arg(grid, model, submodels))
        tmp_sub <- tmp_sub[, names(tmp_res)]

        tmp_res <- vec_rbind(tmp_sub, tmp_res)
      }
    }

    if (!is.null(res)) {
      res <- dplyr::full_join(res, tmp_res, by = merge_vars)
    } else {
      res <- tmp_res
    }

    rm(tmp_res)
  } # end type loop

  # Add outcome data
  y_vals$.row <- orig_rows
  res <- dplyr::full_join(res, y_vals, by = ".row")

  if (has_postprocessor(workflow)) {
    post <- extract_postprocessor(workflow)

    if (tailor::tailor_fully_trained(post)) {
      res <- predict(post, res)
    }
  }

  # Add implicitly grouped metric data, if applicable
  metrics_by <- get_metrics_by(metrics)
  if (has_metrics_by(metrics_by)) {
    new_data$.row <- orig_rows
    res <- dplyr::full_join(res, new_data[c(metrics_by, ".row")], by = ".row")
  }

  # Add case weights (if needed)
  if (has_case_weights(workflow)) {
    case_weights <- extract_case_weights(new_data, workflow)

    if (.use_case_weights_with_yardstick(case_weights)) {
      case_weights <- rlang::list2(!!case_weights_column_name() := case_weights)
      case_weights <- vctrs::new_data_frame(case_weights)
      case_weights <- dplyr::mutate(case_weights, .row = orig_rows)
      res <- dplyr::full_join(res, case_weights, by = ".row")
    }
  }


  res <- maybe_add_ipcw(res, model, types)

  if (!tibble::is_tibble(res)) {
    res <- tibble::as_tibble(res)
  }
  res
}

trim_ipcw <- function(x) {
  x$.weight_time <- NULL
  x$.pred_censored <- NULL
  x
}

maybe_add_ipcw <- function(.data, model, types) {
  if (!any(types == "survival")) {
    return(.data)
  }
  res <- parsnip::.censoring_weights_graf(model, .data)
  res$.pred <- purrr::map(res$.pred, trim_ipcw)
  res
}

#' Get time for analysis of dynamic survival metrics
#' @param metrics A metric set.
#' @param eval_time A vector of evaluation times.
#' @export
#' @keywords internal
get_metric_time <- function(metrics, eval_time) {
  info <- tibble::as_tibble(metrics)
  if (any(info$class == "dynamic_survival_metric")) {
    eval_time <- eval_time[1]
  } else {
    eval_time <- NULL
  }
  eval_time
}

predict_wrapper <- function(model, new_data, type, eval_time, subgrid = NULL) {
  if (is.null(subgrid)) {
    fn <- "predict.model_fit"
  } else {
    fn <- "multi_predict"
  }

  cl <-
    rlang::call2(
      fn,
      .ns = "parsnip",
      object = rlang::expr(model),
      new_data = rlang::expr(new_data),
      type = type)

  # Add in censored regression evaluation times (if needed)
  has_type <- type %in% c("survival", "hazard")
  if (model$spec$mode == "censored regression" & !is.null(eval_time) & has_type) {
    cl <- rlang::call_modify(cl, eval_time = eval_time)
  }

  # When there are sub-models:
  if (!is.null(subgrid)) {
    cl <- rlang::call_modify(cl, !!!subgrid)
  }
  res <- rlang::eval_tidy(cl)

  res
}

# ------------------------------------------------------------------------------

#' @export
#' @rdname tune-internal-functions
forge_from_workflow <- function(new_data, workflow) {
  blueprint <- workflow$pre$mold$blueprint

  forged <- hardhat::forge(new_data, blueprint, outcomes = TRUE)

  forged
}

make_submod_arg <- function(grid, model, submodels) {
  # Assumes only one submodel parameter per model
  real_name <-
    parsnip::get_from_env(paste(class(model$spec)[1], "args", sep = "_")) %>%
    dplyr::filter(has_submodel & engine == model$spec$engine) %>%
    dplyr::pull(parsnip)
  names(submodels) <- real_name
  submodels
}

make_rename_arg <- function(grid, model, submodels) {
  # Assumes only one submodel parameter per model
  real_name <-
    parsnip::get_from_env(paste(class(model$spec)[1], "args", sep = "_")) %>%
    dplyr::filter(has_submodel & engine == model$spec$engine) %>%
    dplyr::pull(parsnip)
  res <- list(real_name)
  names(res) <- names(submodels)
  res
}

get_metrics_by <- function(metric_set) {
  metrics <- attr(metric_set, "metrics")
  metrics_by <- purrr::map(metrics, attr, "by")
  unique(unlist(metrics_by, use.names = FALSE))
}

# metrics_by is the output of `get_metrics_by()`---it's assumed that wherever
# `has_metrics_by()` is needed, `get_metrics_by()` output will be needed too.
has_metrics_by <- function(metrics_by) {
  length(metrics_by) > 0
}

# ------------------------------------------------------------------------------

finalize_workflow_spec <- function(workflow, grid_model) {
  # Already finalized, nothing to tune
  if (ncol(grid_model) == 0L) {
    return(workflow)
  }

  spec <- extract_spec_parsnip(workflow)
  spec <- merge(spec, grid_model)$x[[1]]

  workflow <- set_workflow_spec(workflow, spec)

  workflow
}

#' @export
#' @rdname tune-internal-functions
finalize_workflow_preprocessor <- function(workflow, grid_preprocessor) {
  # Already finalized, nothing to tune
  if (ncol(grid_preprocessor) == 0L) {
    return(workflow)
  }

  recipe <- extract_preprocessor(workflow)
  recipe <- merge(recipe, grid_preprocessor)$x[[1]]

  workflow <- set_workflow_recipe(workflow, recipe)

  workflow
}

# ------------------------------------------------------------------------------

# For any type of tuning, and for fit-resamples, we generate a unified
# grid-info object which is a tibble with two layers of information:
#
# - The outer level has to do with preprocessor iteration. Really this only
#   applies to recipes, as they are the only preprocessor type that can be
#   tuned. These correspond to columns starting at `.iter_preprocessor` and
#   going through the last preprocessor tuning parameter.
# - The inner level has to do with the models that get fit per preprocessor.
#   It corresponds to the columns starting at `.iter_model` and going through
#   `.submodels`. This has been "minified" by `min_grid()`. The `$.submodels`
#   column contains all of the submodels that this parameter combination can
#   predict on.
#
# A single row of this tibble corresponds to a unique hyperparameter combination
# across both the preprocessor and model that has to be fit.
#
# `compute_grid_info()` returns a tibble with the following columns:
# .iter_preprocessor:
#   An integer vector of the current preprocessor iteration.
# .msg_preprocessor:
#   The message that is printed as we fit this preprocessor iteration.
# <preprocessor-tuning-columns>:
#   Zero or more columns outlining the recipes tuning parameter combinations.
# .iter_model:
#   An integer vector of the current model iteration within the current
#   `.iter_preprocessor` iteration.
# .iter_config:
#   A list column of character vectors containing `"Preprocessor<i>_Model<j>"`
#   to describe exactly which iteration we are on. Each submodel is treated
#   as its own unique model here, and has its own id.
# .msg_model:
#   The message that is printed as we fit this model iteration.
# <model-tuning-columns>:
#   Zero or more columns outlining the model tuning parameter combinations.
# .submodels:
#   A list column of lists. Each element contains zero of more submodels that
#   this particular parameter combination can predict for.
#
compute_grid_info <- function(workflow, grid) {
  # For `fit_resamples()`
  if (is.null(grid)) {
    out <- new_grid_info_resamples()
    return(out)
  }

  grid <- tibble::as_tibble(grid)

  parameters <- hardhat::extract_parameter_set_dials(workflow)
  parameters_model <- dplyr::filter(parameters, source == "model_spec")
  parameters_preprocessor <- dplyr::filter(parameters, source == "recipe")

  any_parameters_model <- nrow(parameters_model) > 0
  any_parameters_preprocessor <- nrow(parameters_preprocessor) > 0

  res <- min_grid(extract_spec_parsnip(workflow), grid)

  syms_pre <- rlang::syms(parameters_preprocessor$id)
  syms_mod <- rlang::syms(parameters_model$id)

  # ----------------------------------------------------------------------------
  # Create an order of execution to train the preprocessor (if any). This will
  # define a loop over any preprocessing tuning parameter combinations.
  if (any_parameters_preprocessor) {
    pp_df <-
      dplyr::distinct(res, !!!syms_pre) %>%
      dplyr::arrange(!!!syms_pre) %>%
      dplyr::mutate(
        .iter_preprocessor = dplyr::row_number(),
        .lab_pre = recipes::names0(max(dplyr::n()), "Preprocessor")
      )
    res <-
      dplyr::full_join(res, pp_df, by = parameters_preprocessor$id) %>%
      dplyr::arrange(.iter_preprocessor)
  } else {
    res$.iter_preprocessor <- 1L
    res$.lab_pre <- "Preprocessor1"
  }

  # Make the label shown in the grid and in loggining
  res$.msg_preprocessor <-
    new_msgs_preprocessor(
      res$.iter_preprocessor,
      max(res$.iter_preprocessor)
    )

  # ----------------------------------------------------------------------------
  # Now make a similar iterator across models. Conditioning on each unique
  # preprocessing candidate set, make an iterator for the model candidate sets
  # (if any)

  res <-
    res %>%
    dplyr::group_nest(.iter_preprocessor, keep = TRUE) %>%
    dplyr::mutate(
      .iter_config = purrr::map(data, make_iter_config),
      .model = purrr::map(data, ~ tibble::tibble(.iter_model = seq_len(nrow(.x)))),
      .num_models = purrr::map_int(.model, nrow)
    ) %>%
    dplyr::select(-.iter_preprocessor) %>%
    tidyr::unnest(cols = c(data, .model, .iter_config)) %>%
    dplyr::select(-.lab_pre) %>%
    dplyr::relocate(dplyr::starts_with(".iter"))

  res$.msg_model <-
    new_msgs_model(i = res$.iter_model,
                   n = res$.num_models,
                   res$.msg_preprocessor)

  res %>%
    dplyr::select(-.num_models) %>%
    dplyr::relocate(dplyr::starts_with(".msg"))
}

make_iter_config <- function(dat) {
  # Compute labels for the models *within* each preprocessing loop.
  num_submodels <- purrr::map_int(dat$.submodels, ~ length(unlist(.x)))
  num_models <- sum(num_submodels + 1) # +1 for the model being trained
  .mod_label <- recipes::names0(num_models, "Model")
  .iter_config <- paste(dat$.lab_pre[1], .mod_label, sep = "_")
  .iter_config <- vctrs::vec_chop(.iter_config, sizes = num_submodels + 1)
  tibble::tibble(.iter_config  = .iter_config)
}

# This generates a "dummy" grid_info object that has the same
# structure as a grid-info object with no tunable recipe parameters
# and no tunable model parameters.
new_grid_info_resamples <- function() {
  msgs_preprocessor <- new_msgs_preprocessor(
    i = 1L,
    n = 1L
  )

  msgs_model <- new_msgs_model(
    i = 1L,
    n = 1L,
    msgs_preprocessor = msgs_preprocessor
  )

  iter_config <- list("Preprocessor1_Model1")

  out <- tibble::new_tibble(list(
    .iter_preprocessor = 1L,
    .msg_preprocessor = msgs_preprocessor,
    .iter_model = 1L,
    .iter_config = iter_config,
    .msg_model = msgs_model,
    .submodels = list(list())
  ), nrow = length(msgs_model))

  out
}

new_msgs_preprocessor <- function(i, n) {
  paste0("preprocessor ", i, "/", n)
}
new_msgs_model <- function(i, n, msgs_preprocessor) {
  paste0(msgs_preprocessor, ", model ", i, "/", n)
}

# c(1, 10) -> c("01", "10")
format_with_padding <- function(x) {
  gsub(" ", "0", format(x))
}

# ------------------------------------------------------------------------------

has_preprocessor <- function(workflow) {
  has_preprocessor_recipe(workflow) ||
    has_preprocessor_formula(workflow) ||
    has_preprocessor_variables(workflow)
}

has_preprocessor_recipe <- function(workflow) {
  "recipe" %in% names(workflow$pre$actions)
}

has_preprocessor_formula <- function(workflow) {
  "formula" %in% names(workflow$pre$actions)
}

has_preprocessor_variables <- function(workflow) {
  "variables" %in% names(workflow$pre$actions)
}

has_postprocessor <- function(workflow) {
  "tailor" %in% names(workflow$post$actions)
}

has_case_weights <- function(workflow) {
  "case_weights" %in% names(workflow$pre$actions)
}

has_spec <- function(workflow) {
  "model" %in% names(workflow$fit$actions)
}

set_workflow_spec <- function(workflow, spec) {
  workflow$fit$actions$model$spec <- spec
  workflow
}

set_workflow_recipe <- function(workflow, recipe) {
  workflow$pre$actions$recipe$recipe <- recipe
  workflow
}

set_workflow_tailor <- function(workflow, tailor) {
  workflow$post$actions$tailor$tailor <- tailor
  workflow
}
