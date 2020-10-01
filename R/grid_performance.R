# Functions related to computing predicted performance

# will add further options later for surv_prob etc.
pred_type <- function(x) {
  cls <- class(x)[class(x) != "function"][1]
  res <- dplyr::case_when(
    cls == "class_metric"   ~ "class",
    cls == "prob_metric"    ~ "prob",
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
  res <- tibble(
    .metric = names(metric_dir),
    direction = unname(metric_dir),
    type = unname(purrr::map_chr(metric_list, pred_type))
  )
  res
}

estimate_metrics <- function(dat, metric, workflow, event_level) {
  if (inherits(dat, "try-error")) {
    return(NULL)
  }

  # Determine the type of prediction that is required
  type_info <- metrics_info(metric)
  types <- unique(type_info$type)

  outcome <- outcome_names(workflow)

  if (length(outcome) > 1L) {
    rlang::abort(paste0(
      "Internal error: Multiple outcomes are not ",
      "supported in `estimate_metrics()`."
    ))
  }

  param_names <- dials::parameters(workflow)$id

  if (all(types == "numeric")) {
    estimate_reg(dat, metric, param_names, outcome)
  } else if (all(types == "class" | types == "prob")) {
    estimate_class_prob(dat, metric, param_names, outcome, types, event_level)
  } else {
    rlang::abort("Metric type not yet supported by tune.")
  }
}

estimate_reg <- function(dat, metric, params, outcome) {
  dat %>%
    dplyr::group_by(!!!rlang::syms(params)) %>%
    metric(estimate = .pred, truth = !!sym(outcome))
}

estimate_class_prob <- function(dat, metric, params, outcome, types, event_level) {
  truth <- sym(outcome)

  estimate <- NULL
  if (any(types == "class")) {
    estimate <- sym(".pred_class")
  }

  probs <- NULL
  if (any(types == "prob")) {
    levels <- levels(dat[[outcome]])
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
    dplyr::group_by(!!!rlang::syms(params)) %>%
    metric(
      truth = !!truth,
      estimate = !!estimate,
      !!!probs,
      event_level = event_level
    )
}
