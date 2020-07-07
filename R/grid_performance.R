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

estimate_metrics <- function(dat, metric, workflow, other_names = NULL) {
  # other_names will take additional columns from the recipe (if any)

  if (inherits(dat, "try-error")) {
    return(NULL)
  }

  # Determine the type of prediction that is required
  type_info <- metrics_info(metric)
  types <- unique(type_info$type)

  y_names <- outcome_names(workflow)
  param_names <- dials::parameters(workflow)$id

  if (all(types == "numeric")) {
    res <- estimate_reg(dat, metric, param_names, y_names)
  } else {
    levels <- levels(dat[[y_names]])
    if (any(types %in% c("class", "prob"))) {
      res <- estimate_class(dat, metric, param_names, y_names, lvl = levels, types)
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

estimate_class <- function(dat, metric, params, outcomes, lvl, types) {
  if (all(types == "class")) {
    res <-
      dat %>%
      dplyr::group_by(!!!rlang::syms(params)) %>%
      metric(estimate = .pred_class, truth = !!sym(outcomes))
  } else {
    prob_cols <- paste0(".pred_", lvl)
    if (length(prob_cols) == 2) {
      prob_cols <- prob_cols[1]
    }
    if (all(types == "prob")) {
      res <-
        dat %>%
        dplyr::group_by(!!!rlang::syms(params)) %>%
        metric(truth = !!sym(outcomes), !!!prob_cols)
    } else {
      res <-
        dat %>%
        dplyr::group_by(!!!rlang::syms(params)) %>%
        metric(truth = !!sym(outcomes), !!!prob_cols, estimate = .pred_class)
    }
  }
  res
}
