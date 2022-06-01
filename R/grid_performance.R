# Functions related to computing predicted performance

# will add further options later for surv_prob etc.
pred_type <- function(x) {
  cls <- class(x)[class(x) != "function"][1]
  res <- dplyr::case_when(
    cls == "class_metric" ~ "class",
    cls == "prob_metric" ~ "prob",
    cls == "numeric_metric" ~ "numeric",
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
  res <- tibble::tibble(
    .metric = names(metric_dir),
    direction = unname(metric_dir),
    type = unname(purrr::map_chr(metric_list, pred_type))
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
#' @keywords internal
#' @name tune-internal-functions
#' @export
.estimate_metrics <- function(dat, metric, param_names, outcome_name, event_level) {
  if (inherits(dat, "try-error")) {
    return(NULL)
  }

  # Determine the type of prediction that is required
  type_info <- metrics_info(metric)
  types <- unique(type_info$type)

  if (length(outcome_name) > 1L) {
    rlang::abort(paste0(
      "Internal error: Multiple outcomes are not ",
      "supported in `.estimate_metrics()`."
    ))
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
  } else {
    rlang::abort("Metric type not yet supported by tune.")
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
        rlang::abort("`event_level` must be either 'first' or 'second'.")
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
