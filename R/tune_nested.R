#' Model tuning assessments via nested resampling
#'
#' [tune_nested()] computes
#'
#' @inheritParams tune_grid
#' @param control An object used to modify the tuning process.
#' @param ... Not currently used.
#' @return An updated version of `resamples` with extra list columns for `.metrics` and
#' `.notes` (optional columns are `.predictions` and `.extracts`). `.notes`
#' contains warnings and errors that occur during execution.
#' @seealso [control_grid()],
#' [autoplot.tune_results()], [show_best()], [select_best()],
#' [collect_predictions()], [collect_metrics()]

#' @export
tune_nested <- function(object, ...) {
  UseMethod("tune_nested")
}

#' @export
tune_nested.default <- function(object, ...) {
  msg <- paste0(
    "The first argument to [tune_nested()] should be either ",
    "a model or workflow."
  )
  rlang::abort(msg)
}

#' @export
#' @rdname tune_nested
tune_nested.model_spec <- function(object, preprocessor, resamples,
                                   fn = "tune_grid",
                                   param_info, metrics,
                                   .control = control_nested(), ...) {
  if (rlang::is_missing(preprocessor) || !is_preprocessor(preprocessor)) {
    rlang::abort(paste(
      "To tune a model spec, you must preprocess",
      "with a formula or recipe"
    ))
  }

  control <- parsnip::condense_control(control, control_grid())

  wflow <- add_model(workflow(), object)

  if (is_recipe(preprocessor)) {
    wflow <- add_recipe(wflow, preprocessor)
  } else if (rlang::is_formula(preprocessor)) {
    wflow <- add_formula(wflow, preprocessor)
  }

  tune_nested(
    wflow,
    resamples = resamples,
    param_info,
    metrics,
    .control = control,
    ...
  )
}

#' @export
#' @rdname tune_nested
tune_nested.workflow <- function(object, resamples, fn = "tune_grid",
                                 param_info, metrics,
                                 .control = control_nested(), ...) {


  control <- parsnip::condense_control(control, control_grid())

  res <-
    tune_nested_workflow(
      object,
      resamples = resamples,
      .control = control,
      ...
    )
  .stash_last_result(res)
  res
}

# ------------------------------------------------------------------------------

tune_nested_workflow <- function(workflow, resamples, param_info, metrics,
                                 .control = control_nested(), ...) {
  check_rset(resamples)
  opts <- list(...)

  metrics <- check_metrics(metrics, workflow)

  pset <- check_parameters(
    workflow,
    pset = pset,
    data = resamples$splits[[1]]$data,
    grid_names = names(grid)
  )

  check_workflow(workflow, pset = pset)
  check_backend_options(control$backend_options)

  # Save rset attributes, then fall back to a bare tibble
  rset_info <- pull_rset_attributes(resamples)
  resamples <- new_bare_tibble(resamples)

  # resamples <- tune_nested_loop(
  #   resamples = resamples,
  #   grid = grid,
  #   workflow = workflow,
  #   metrics = metrics,
  #   control = control,
  #   rng = rng
  # )

  if (is_cataclysmic(resamples)) {
    rlang::warn("All models failed. Run `show_notes(.Last.tune.result)` for more information.")
  }

  outcomes <- reduce_all_outcome_names(resamples)
  resamples[[".all_outcome_names"]] <- NULL

  workflow <- set_workflow(workflow, control)

  # new_tune_results(
  #   x = resamples,
  #   parameters = pset,
  #   metrics = metrics,
  #   outcomes = outcomes,
  #   rset_info = rset_info,
  #   workflow = workflow
  # )
}

# ------------------------------------------------------------------------------

tune_inner <- function(workflow, resamples, param_info, metrics,
                                    fn, opts, seed = 1) {
  set.seed(seed)
  .cl <-
    rlang::call2(
      .fn = fn,
      object = expr(workflow),
      resamples = expr(resamples),
      metrics = expr(metrics),
      param_info = expr(param_info),
      !!!opts
    )
  res <- rlang::eval_tidy(.cl)
  prime_metric <- tune:::metrics_info(metrics)$.metric[1]
  res_metrics <- collect_metrics(res) %>% dplyr::mutate(level = "inner")
  inner_param <- select_best(res, metric = prime_metric) %>% dplyr::mutate(best = TRUE)
  # dplyr::full_join(
  #   res_metrics,
  #   inner_param,
  #   by = c(".config", .get_tune_parameter_names(res))
  # ) %>%
  #   dplyr::mutate(best = ifelse(is.na(best), FALSE, best))

  res_metrics$best <- res_metrics$.config == inner_param$.config
  res_metrics
}

# TODO what about seeds?
fit_outer_resamples <- function(config, split, workflow, metrics) {
  prm_names <-
    extract_parameter_set_dials(workflow) %>%
    pluck("id")
  config <-
    dplyr::filter(config, best) %>%
    dplyr::select(dplyr::all_of(prm_names)) %>%
    dplyr::distinct()

  workflow <- finalize_workflow(workflow, config)
  workflow_fit <- fit(workflow, data = analysis(split))
  y_name <- outcome_names(workflow_fit)
  predictions <- augment(workflow_fit, new_data = assessment(split))
  res <- metrics(predictions, truth = !!y_name, estimate = .pred_class, .pred_Class1)
  # do extracts here, same predictions?
  res
}


# TODO control_nested, separate control for base tuner?
# TODO api for selection?


