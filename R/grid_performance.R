# Functions related to computing predicted performance

# will add further options later for surv_prob etc.
pred_type <- function(x) {
  cls <- class(x)[class(x) != "function"][1]
  res <- dplyr::case_when(
    cls == "class_metric" ~ "class",
    cls == "prob_metric" ~ "prob",
    cls == "numeric_metric" ~ "numeric",
    cls == "dynamic_survival_metric" ~ "survival",
    cls == "integrated_survival_metric" ~ "survival",
    cls == "static_survival_metric" ~ "time",
    TRUE ~ "unknown"
  )
  res
}

#' @export
#' @keywords internal
#' @rdname empty_ellipses
metrics_info <- function(x) {
  metric_list <- rlang::env_get(environment(x), "fns")
  metric_dir <- purrr::map_chr(metric_list, attr, "direction")
  res <- tibble::new_tibble(
    list(
      .metric = names(metric_dir),
      direction = unname(metric_dir),
      type = unname(purrr::map_chr(metric_list, pred_type))
    ),
    nrow = length(metric_dir)
  )
  res
}

#' Internal functions used by other tidymodels packages
#'
#' These are not to be meant to be invoked directly by users.
#' @param dat A data set.
#' @param metric A metric set.
#' @param param_names A character vector of tuning parameter names.
#' @param outcome_name A character string for the column of `dat` that is the
#' outcome.
#' @param event_level A logical passed from the control function.
#' @param x A character vector of package names.
#' @param .expr Code to execute.
#' @param ... Object to pass to the internal `tune_log()` function.
#' @param bad_only A logical for whether warnings and errors should be caught.
#' @param notes Character data to add to the logging.
#' @param workflow A workflow.
#' @param grid_preprocessor A tibble with parameter information.
#' @param new_data A data frame or matrix of predictors to process.
#' @param metrics_info The output of `tune:::metrics_info(metrics)`---only
#' included as an argument to allow for pre-computing.
#' @param catalog A logical passed to `tune_log()` giving whether the message
#' is compatible with the issue cataloger. Defaults to `TRUE`. Updates that are
#' always unique and do not represent a tuning "issue" can bypass the cataloger
#' by setting `catalog = FALSE`.
#' @keywords internal
#' @name tune-internal-functions
#' @export
.estimate_metrics <- function(dat, metric, param_names, outcome_name, event_level,
                              metrics_info = metrics_info(metrics)) {
  # The call stack is:
  #
  # tune_grid_loop_iter()
  #   append_metrics(). <many times>
  #    .estimate_metrics()

  # predictions made in predict_model()

  if (inherits(dat, "try-error")) {
    return(NULL)
  }

  # Determine the type of prediction that is required
  types <- unique(metrics_info$type)

  if (length(outcome_name) > 1L) {
    cli::cli_abort(
      "Multiple outcomes are not supported in {.fn .estimate_metrics}.",
      .internal = TRUE
    )
  }

  if (case_weights_column_name() %in% names(dat)) {
    case_weights <- sym(case_weights_column_name())
  } else {
    case_weights <- NULL
  }

  if (all(types == "numeric")) {
    estimate_reg(dat, metric, param_names, outcome_name, case_weights)
  } else if (all(types == "class" | types == "prob")) {
    estimate_class_prob(dat, metric, param_names, outcome_name, case_weights, types, event_level)
  } else if (all(types == "time" | types == "survival")) {
    estimate_surv(dat, metric, param_names, outcome_name, case_weights, types)
  } else {
    cli::cli_abort("Metric type not yet supported by {.pkg tune}.")
  }
}

estimate_reg <- function(dat, metric, param_names, outcome_name, case_weights) {
  dat %>%
    dplyr::group_by(!!!rlang::syms(param_names)) %>%
    metric(estimate = .pred, truth = !!sym(outcome_name), case_weights = !!case_weights)
}

estimate_class_prob <- function(dat, metric, param_names, outcome_name,
                                case_weights, types, event_level) {
  truth <- sym(outcome_name)

  estimate <- NULL
  if (any(types == "class")) {
    estimate <- sym(".pred_class")
  }

  probs <- NULL
  if (any(types == "prob")) {
    levels <- levels(dat[[outcome_name]])
    probs <- paste0(".pred_", levels)

    # Special case binary class prob metrics,
    # as yardstick requires only 1 column passed through
    if (length(probs) == 2) {
      if (identical(event_level, "first")) {
        probs <- probs[[1]]
      } else if (identical(event_level, "second")) {
        probs <- probs[[2]]
      } else {
        cli::cli_abort("{.arg event_level} must be either {.val first} or
                        {.val second}.")
      }
    }
  }

  dat %>%
    dplyr::group_by(!!!rlang::syms(param_names)) %>%
    metric(
      truth = !!truth,
      estimate = !!estimate,
      !!!probs,
      case_weights = !!case_weights,
      event_level = event_level
    )
}

# ------------------------------------------------------------------------------

estimate_surv <- function(dat, metric, param_names, outcome_name, case_weights, types) {
  #  potentially need to work around submodel parameters since those are within .pred
  dat <- unnest_parameters(dat, param_names)
  dat %>%
    dplyr::group_by(!!!rlang::syms(param_names)) %>%
    metric(
      truth = !!rlang::sym(outcome_name),
      estimate = !!maybe_estimate(metric),
      case_weights = !!case_weights,
      !!maybe_surv_prob(metric)
    )
}

unnest_parameters <- function(x, params = NULL) {
   if (is.null(params)) {
    return(x)
   }

  # When multi_predict() is used, .pred will have the tuning parameter values.
  # Other (non-submodel) parameters will be outside of 'x'.
  # If this happens, pull the submodel parameters out of .pred and put them at
  # the out level ('x') with the rest (if any)

  outer_nms <- names(x)
  has_pred <- any(outer_nms == ".pred")
   if (!has_pred) {
    return(x)
  }

  inner_nms <- names(x$.pred[[1]])
  has_inner_params <- any(params %in% inner_nms)
  if (!has_inner_params) {
    return(x)
  }

  x <- x %>% parsnip::add_rowindex()

  others <- x %>% dplyr::select(-.pred)
  rm_cols <- c(".pred_censored", ".weight_time")
  nest_cols <- c(".eval_time", ".pred_survival", ".weight_censored")
  x <-
    x %>%
    dplyr::select(.pred, .row) %>%
    tidyr::unnest(cols = c(.pred)) %>%
    dplyr::select(-dplyr::any_of(rm_cols)) %>%
    tidyr::nest(.pred = c(all_of(nest_cols)), .by = c(.row, dplyr::all_of(params))) %>%
    dplyr::full_join(others, by = ".row") %>%
    dplyr::select(-.row)
  x
}

maybe_estimate <- function(x) {
  info <- tibble::as_tibble(x)
  if (any(info$class == "static_survival_metric")) {
    res <- rlang::sym(".pred_time")
  } else {
    res <- NULL
  }
  res
}

maybe_surv_prob <- function(x) {
  info <- tibble::as_tibble(x)
  # dyn_inputs defined in checks.R
  if (any(info$class %in% dyn_inputs)) {
    res <- rlang::sym(".pred")
  } else {
    res <- NULL
  }
  res
}


