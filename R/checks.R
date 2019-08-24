check_rset <- function(x) {
  if (!inherits(x, "rset")) {
    stop("The `rs` argument should be an 'rset' object, such as the type ",
         "produced by `vfold_cv()` or other 'rsample' functions.",
         call. = FALSE)
  }
  if (inherits(x, "loo_cv")) {
    stop("Leave-one-out cross-validation is not currently supported with tune.",
         call. = FALSE)
  }
  if (inherits(x, "nested_cv")) {
    stop("Nested resampling is not currently supported with tune.",
         call. = FALSE)
  }
  invisible(NULL)
}


check_grid <- function(x, object) {
  tune_param <- tune_args(object)
  if (!is.null(x)) {
    if (!is.data.frame(x) & !inherits(x, "param_grid")) {
      stop("The `grid` argument should be either a data frame or a 'param_grid' ",
           "object", call. = FALSE)
    }
    if (!isTRUE(all.equal(sort(names(x)), sort(tune_param$id)))) {
      stop("Based on the workflow, the grid object should have columns: ",
           paste0("'", tune_param$id, "'", collapse = ", "),
           call. = FALSE)
    }
  } else {
    check_object(object, check_dials = TRUE)
    x <- dials::grid_latin_hypercube(param_set(object), size = 10)
  }
  x
}

check_object <- function(x, check_dials = FALSE) {
  if (!inherits(x, "workflow")) {
    stop("The `object` argument should be a 'workflow' object.",
         call. = FALSE)
  }
  if (check_dials) {
    y <- param_set(x)
    params <- purrr::map_lgl(y$object, inherits, "param")
    if (!all(params)) {
      stop("The workflow has arguments to be tuned that are missing some ",
           "parameter objects: ", paste0("'", y$id, "'", collapse = ", "),
           call. = FALSE)
    }
    quant_param <- purrr::map_lgl(y$object, inherits, "quant_param")
    quant_name <- y$id[quant_param]
    compl <- map_lgl(y$object[quant_param],
                     ~ !dials::is_unknown(.x$range$lower) &
                       !dials::is_unknown(.x$range$upper))
    if (any(!compl)) {
      stop("The workflow has arguments whose ranges are not finalized: ",
           paste0("'", quant_name[!compl], "'", collapse = ", "),
           call. = FALSE)
    }
  }

  invisible(NULL)
}

check_perf <- function(x, object) {
  if (!is.null(x)) {
    cls <- c("numeric_metric_set", "class_prob_metric_set")
    if (!inherits(x, cls)) {
      stop("The `perf` argument should be the results of `yardstick::metric_set()`.",
           call. = FALSE)
    }
  } else {
    if (object$fit$model$model$mode == "regression") {
      x <- yardstick::metric_set(rmse, rsq)
    } else {
      x <- yardstick::metric_set(mn_log_loss, accuracy)
    }
  }

  x
}

perf_info <- function(x) {
  metric_list <- rlang::env_get(environment(x), "fns")
  metric_dir <- map_chr(metric_list, attr, "direction")
  res <- tibble(
    .metric = names(metric_dir),
    direction = unname(metric_dir),
    type = unname(purrr::map_chr(metric_list, pred_type))
    )
  res
}

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


check_grid_control <- function(x) {
  exp_names <- names(grid_control())
  if (!isTRUE(all(all.equal(names(x), exp_names)))) {
    miss_names <- exp_names[!(exp_names %in% names(x))]
    stop("The grid control object is missing element(s): ",
         paste0(miss_names, collapse = ", "))
  }
  invisible(x)
}

check_Bayes_control <- function(x) {
  exp_names <- names(Bayes_control())
  if (!isTRUE(all(all.equal(names(x), exp_names)))) {
    miss_names <- exp_names[!(exp_names %in% names(x))]
    stop("The Baysian optimization control object is missing element(s): ",
         paste0(miss_names, collapse = ", "))
  }
  invisible(x)
}

check_initial <- function(x, pset) {
  if (is.null(x) || is.numeric(x)) {
    if (is.null(x)) {
      x <- 3
    }
    x <- create_initial_set(pset, n = x)
  } else {
    if (inherits(x, "grid_results")) {
      x <- estimate(x)
    } else {
      x <- x
    }
  }
  x
}

get_objective_name <- function(x, perf) {
  if (is.null(x)) {
    metric_data <- perf_info(perf)
    x <- metric_data$.metric[1]
  } else {
    # check for a name or acquisition funciton
  }
  x
}


# ------------------------------------------------------------------------------
# acq functions

check_direction <- function(x) {
  if (!is.logical(x) || length(x) != 1) {
    stop("`maximize` should be a single logical.", call. = FALSE)
  }
  invisible(NULL)
}


check_best <- function(x) {
  if (!is.numeric(x) || length(x) != 1 || is.na(x)) {
    stop("`best` should be a single, non-missing numeric", call. = FALSE)
  }
  invisible(NULL)
}
