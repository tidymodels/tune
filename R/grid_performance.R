estimate_perf <- function(dat, metric, object, other_names = NULL) {
  # other_names will take additional columns from the recipe (if any)

  if (inherits(dat, "try-error")) {
    return(NULL)
  }

  # Determine the type of prediction that is required
  type_info <- perf_info(metric)
  types <- unique(type_info$type)

  # This will use attributes in metric sets in future yardstick versions to
  # determine .pred vs .pred_class etc.
  y_names <- outcome_names(object)
  param_names <- param_set(object)$id
  if (all(types == "numeric")) {
    res <- estimate_reg(dat, metric, param_names, y_names)
  } else {
    if (all(types == "class")) {
      res <- estimate_class(dat, metric, param_names, y_names)
    } else {
      stop("Not implmented yet")
    }
  }
  res
}

estimate_reg <- function(dat, metric, params, outcomes) {
  dat %>%
    dplyr::group_by(!!!rlang::syms(params)) %>%
    metric(estimate = .pred, truth = !!sym(outcomes))
}

estimate_class <- function(dat, metric, params, outcomes, lvl) {
  dat %>%
    dplyr::group_by(!!!rlang::syms(params)) %>%
    metric(estimate = .pred_class, truth = !!sym(outcomes))
}

estimate_class_probs <- function(dat, metric, params, outcomes, lvl) {
  # Davis TODO
  # do we still need the `.pred_class` column
}

estimate_class_mixed <- function(dat, metric, params, outcomes, lvl) {
  # Davis TODO
}

