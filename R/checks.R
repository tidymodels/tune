check_rset <- function(x) {
  if (!inherits(x, "rset")) {
    stop("The `resamples` argument should be an 'rset' object, such as the type ",
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
  parameters <- dials::parameters(object)

  if (nrow(parameters) == 0L) {
    msg <- paste0(
      "The workflow has no tuning parameters, ",
      "performance will be evaluated using the resamples with no tuning. ",
      "Did you want `resample()`?"
    )
    rlang::warn(msg)
    return(x)
  }

  if (is.null(x)) {
    x <- 10
  }

  if (is.numeric(x)) {
    x <- as.integer(x[1])
    check_object(object, check_dials = TRUE)
    x <- dials::grid_latin_hypercube(parameters, size = x)
    x <- dplyr::distinct(x)
    return(x)
  }

  if (!is.data.frame(x) & !inherits(x, "param_grid")) {
    msg <- paste0(
      "The `grid` argument should be either a data frame, a 'param_grid' ",
      "object, or an integer."
    )
    rlang::abort(msg)
  }

  tune_param <- tune_args(object)

  if (!isTRUE(all.equal(sort(names(x)), sort(tune_param$id)))) {
    msg <- paste0(
      "Based on the workflow, the grid object should have columns: ",
      paste0("'", tune_param$id, "'", collapse = ", ")
    )
    rlang::abort(msg)
  }

  if (!tibble::is_tibble(x)) {
    x <- tibble::as_tibble(x)
  }

  x
}

shhhh <- function(x) {
  suppressPackageStartupMessages(requireNamespace(x, quietly = TRUE))
}

is_installed <- function(pkg) {
  res <- try(shhhh(pkg), silent = TRUE)
  res
}

check_installs <- function(x) {

  if (x$engine == "unknown") {
    rlang::abort("Please declare an engine for the model")
  } else {
    m_type <- class(x)[1]
    deps <- parsnip::get_dependency(m_type)
    deps <- deps$pkg[deps$engine == x$engine]
    deps <- unlist(deps)
  }

  if (length(deps) > 0) {
    is_inst <- purrr::map_lgl(deps, is_installed)
    if (any(!is_inst)) {
      stop("Some package installs are required: ",
        paste0("'", deps[!is_inst], "'", collapse = ", "),
        call. = FALSE
      )
    }
  }
}

check_object <- function(x, check_dials = FALSE) {
  if (!inherits(x, "workflow")) {
    stop("The `object` argument should be a 'workflow' object.",
         call. = FALSE)
  }
  if (length(x$pre) == 0) {
    stop("A model formula or recipe are required.", call. = FALSE)
  }
  if (length(x$fit) == 0) {
    stop("A parsnip model is required.", ll. = FALSE)
  }
  if (check_dials) {
    y <- dials::parameters(x)
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

  mod <- get_wflow_model(x)
  check_installs(mod)

  invisible(NULL)
}

check_metrics <- function(x, object) {
  if (!is.null(x)) {
    cls <- c("numeric_metric_set", "class_prob_metric_set")
    if (!inherits(x, cls)) {
      stop("The `metrics` argument should be the results of `yardstick::metric_set()`.",
           call. = FALSE)
    }
  } else {
    if (get_wflow_model(object)$mode == "regression") {
      x <- yardstick::metric_set(rmse, rsq)
    } else {
      x <- yardstick::metric_set(accuracy, kap)
    }
  }

  x
}

check_initial <- function(x, pset, wflow, resamples, metrics, ctrl) {
  if (is.null(x) || is.numeric(x)) {
    if (is.null(x)) {
      x <- 3
    }
    x <- create_initial_set(pset, n = x)
    if (ctrl$verbose) {
      message()
      msg <- paste0(" Generating a set of ", nrow(x), " initial parameter results")
      tune_log(ctrl, split = NULL, msg, type = "go")
    }
    x <- tune_grid(wflow, resamples = resamples, grid = x, metrics = metrics,
                   control = control_grid(extract = ctrl$extract,
                                          save_pred = ctrl$save_pred))
    if (ctrl$verbose) {
      tune_log(ctrl, split = NULL, "Initialization complete", type = "success")
      message()
    }
  }
  if (!any(names(x) == ".iter")) {
    x <- x %>% dplyr::mutate(.iter = 0)
  }
  x
}

get_objective_name <- function(x, metrics) {
  if (is.null(x)) {
    metric_data <- metrics_info(metrics)
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


# ------------------------------------------------------------------------------

check_class_or_null <- function(x, cls = "numeric") {
  inherits(x, cls) | is.null(x)
}

val_class_or_null <- function(x, cls = "numeric", where = NULL) {
  cl <- match.call()
  fine <- check_class_or_null(x, cls)
  cls <- paste(cls, collapse = " or ")
  if (!fine) {
    msg <- glue::glue("Argument '{deparse(cl$x)}' should be a {cls} or NULL")
    if (!is.null(where)) {
      msg <- glue::glue(msg, " in `{where}`")
    }
    rlang::abort(msg)
  }
  invisible(NULL)
}

check_class_and_single <- function(x, cls = "numeric") {
  isTRUE(inherits(x, cls) & length(x) == 1)
}

val_class_and_single <- function(x, cls = "numeric", where = NULL) {
  cl <- match.call()
  fine <- check_class_and_single(x, cls)
  cls <- paste(cls, collapse = " or ")
  if (!fine) {
    msg <- glue::glue("Argument '{deparse(cl$x)}' should be a single {cls} value")
    if (!is.null(where)) {
      msg <- glue::glue(msg, " in `{where}`")
    }
    rlang::abort(msg)
  }
  invisible(NULL)
}
