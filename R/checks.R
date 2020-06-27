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


grid_msg <- "`grid` should be a positive integer or a data frame."

check_grid <- function(x, object, pset = NULL) {
  if (is.null(pset)) {
    pset <- dials::parameters(object)
  }

  if (nrow(pset) == 0L) {
    msg <- paste0(
      "No tuning parameters have been detected, ",
      "performance will be evaluated using the resamples with no tuning. ",
      "Did you want [fit_resamples()]?"
    )
    rlang::warn(msg)
    return(x)
  }

  if (is.null(x)) {
    rlang::abort(grid_msg)
  }

  if (!is.numeric(x)) {
    if (!is.data.frame(x)) {
      rlang::abort(grid_msg)
    }

    tune_tbl <- tune_args(object)
    tune_params <- tune_tbl$id

    # when called from [tune_bayes()]
    tune_params <- tune_params[tune_params != ".iter"]

    grid_params <- names(x)

    extra_grid_params <- setdiff(grid_params, tune_params)
    extra_tune_params <- setdiff(tune_params, grid_params)

    if (length(extra_grid_params) != 0L) {
      extra_grid_params <- glue::single_quote(extra_grid_params)
      extra_grid_params <- glue::glue_collapse(extra_grid_params, sep = ", ")

      msg <- glue::glue(
        "The provided `grid` has the following parameter columns that have ",
        "not been marked for tuning by `tune()`: {extra_grid_params}."
      )

      rlang::abort(msg)
    }

    if (length(extra_tune_params) != 0L) {
      extra_tune_params <- glue::single_quote(extra_tune_params)
      extra_tune_params <- glue::glue_collapse(extra_tune_params, sep = ", ")

      msg <- glue::glue(
        "The provided `grid` is missing the following parameter columns that ",
        "have been marked for tuning by `tune()`: {extra_tune_params}."
      )

      rlang::abort(msg)
    }
  } else {
    x <- as.integer(x[1])
    if (x < 1) {
      rlang::abort(grid_msg)
    }
    check_workflow(object, pset = pset, check_dials = TRUE)

    x <- dials::grid_latin_hypercube(pset, size = x)
    x <- dplyr::distinct(x)
  }

  if (!tibble::is_tibble(x)) {
    x <- tibble::as_tibble(x)
  }

  x
}

needs_finalization <- function(x, nms = character(0)) {
  # If an unknown engine-specific parameter, the object column is missing and
  # no need for finalization
  x <- x[!is.na(x$object), ]
  # If the parameter is in a pre-defined grid, then no need to finalize
  x <- x[!(x$id %in% nms),]
  if (length(x) == 0) {
    return(FALSE)
  }
  any(dials::has_unknowns(x$object))
}

check_parameters <- function(object, pset = NULL, data, grid_names = character(0)) {
  if (is.null(pset)) {
    pset <- parameters(object)
  }
  unk <- purrr::map_lgl(pset$object, dials::has_unknowns)
  if (!any(unk)) {
    return(pset)
  }
  tune_param <- tune_args(object)
  tune_recipe <- tune_param$id[tune_param$source == "recipe"]
  tune_recipe <- length(tune_recipe) > 0
  if (tune_recipe) {
    rlang::abort(
      paste(
        "Some tuning parameters require finalization but there are recipe",
        "parameters that require tuning. Please use `parameters()` to",
        "finalize the parameter ranges."
      )
    )
  }

  if (needs_finalization(pset, grid_names)) {
    msg <- "Creating pre-processing data to finalize unknown parameter"
    unk_names <- pset$id[unk]
    if (length(unk_names) == 1) {
      msg <- paste0(msg, ": ", unk_names)
    } else {
      msg <- paste0(msg, "s: ", paste0("'", unk_names, "'", collapse = ", "))
    }

    tune_log(list(verbose = TRUE), split = NULL, msg, type = "info")

    x <- workflows::.fit_pre(object, data)$pre$mold$predictors
    pset$object <- purrr::map(pset$object, dials::finalize, x = x)
  }
  pset
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

check_param_objects <- function(pset) {
  params <- purrr::map_lgl(pset$object, inherits, "param")

  if (!all(params)) {
    rlang::abort(paste0(
      "The workflow has arguments to be tuned that are missing some ",
      "parameter objects: ",
      paste0("'", pset$id[!params], "'", collapse = ", ")
    ))
  }
  invisible(pset)
}

check_workflow <- function(x, pset = NULL, check_dials = FALSE) {
  if (!inherits(x, "workflow")) {
    rlang::abort("The `object` argument should be a 'workflow' object.")
  }

  has_preprocessor <- has_preprocessor_formula(x) || has_preprocessor_recipe(x)

  if (!has_preprocessor) {
    rlang::abort("A model formula or recipe are required.")
  }

  has_spec <- has_spec(x)

  if (!has_spec) {
    rlang::abort("A parsnip model is required.")
  }

  if (check_dials) {
    if (is.null(pset)) {
      pset <- dials::parameters(x)
    }

    check_param_objects(pset)

    incompl <- dials::has_unknowns(pset$object)

    if (any(incompl)) {
      rlang::abort(paste0(
        "The workflow has arguments whose ranges are not finalized: ",
        paste0("'", pset$id[incompl], "'", collapse = ", ")
      ))
    }
  }

  mod <- workflows::pull_workflow_spec(x)
  check_installs(mod)

  invisible(NULL)
}

check_metrics <- function(x, object) {
  mode <- workflows::pull_workflow_spec(object)$mode

  if (is.null(x)) {
    switch(
      mode,
      regression = {
        x <- yardstick::metric_set(rmse, rsq)
      },
      classification = {
        x <- yardstick::metric_set(roc_auc, accuracy)
      },
      unknown = {
        rlang::abort("Internal error: `check_installs()` should have caught an `unknown` mode.")
      },
      rlang::abort("Unknown `mode` for parsnip model.")
    )

    return(x)
  }

  is_numeric_metric_set <- inherits(x, "numeric_metric_set")
  is_class_prob_metric_set <- inherits(x, "class_prob_metric_set")

  if (!is_numeric_metric_set && !is_class_prob_metric_set) {
    rlang::abort("The `metrics` argument should be the results of [yardstick::metric_set()].")
  }

  if (mode == "regression" && is_class_prob_metric_set) {
    msg <- paste0(
      "The parsnip model has `mode = 'regression'`, ",
      "but `metrics` is a metric set for class / probability metrics."
    )
    rlang::abort(msg)
  }

  if (mode == "classification" && is_numeric_metric_set) {
    msg <- paste0(
      "The parsnip model has `mode = 'classification'`, ",
      "but `metrics` is a metric set for regression metrics."
    )
    rlang::abort(msg)
  }

  x
}

bayes_msg <- "`initial` should be a positive integer or the results of [tune_grid()]"

check_initial <- function(x, pset, wflow, resamples, metrics, ctrl) {
  if (is.null(x)) {
    rlang::abort(bayes_msg)
  }
  if (is.numeric(x)) {
    x <- create_initial_set(pset, n = x)
    if (ctrl$verbose) {
      message()
      msg <- paste0(" Generating a set of ", nrow(x), " initial parameter results")
      tune_log(ctrl, split = NULL, msg, type = "go")
    }

    x <- tune_grid(
      wflow,
      resamples = resamples,
      grid = x,
      metrics = metrics,
      param_info = pset,
      control = control_grid(extract = ctrl$extract, save_pred = ctrl$save_pred)
    )

    # Strip off `tune_results` class since we add on an `iteration_results`
    # class later.
    x <- new_bare_tibble(x)

    if (ctrl$verbose) {
      tune_log(ctrl, split = NULL, "Initialization complete", type = "success")
      message()
    }
  } else {
    if (!inherits(x, "tune_results")) {
      rlang::abort(bayes_msg)
    }

    if (ctrl$save_pred & !any(names(x) == ".predictions")) {
      rlang::abort("`save_pred` can only be used if the initial results saved predictions.")
    }
    if (!is.null(ctrl$extract) & !any(names(x) == ".extracts")) {
      rlang::abort("`extract` can only be used if the initial results has extractions.")
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
