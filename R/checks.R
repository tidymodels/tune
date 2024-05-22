#' @export
#' @keywords internal
#' @rdname empty_ellipses
check_rset <- function(x) {
  if (!inherits(x, "rset")) {
    rlang::abort(paste0(
      "The `resamples` argument should be an 'rset' object, such as the type ",
      "produced by `vfold_cv()` or other 'rsample' functions."
    ))
  }
  if (inherits(x, "loo_cv")) {
    rlang::abort(
      "Leave-one-out cross-validation is not currently supported with tune."
    )
  }
  if (inherits(x, "nested_cv")) {
    rlang::abort("Nested resampling is not currently supported with tune.")
  }
  if (inherits(x, "permutations")) {
    rlang::abort("Permutation samples are not suitable for tuning.")
  }
  invisible(NULL)
}

backend_options_msg <- "`backend_options` should be created by `tune::new_backend_options()`."

check_backend_options <- function(backend_options) {
  if (!is.null(backend_options) &&
      !inherits(backend_options, "tune_backend_options")) {
    rlang::abort(backend_options_msg)
  }

  invisible(NULL)
}


grid_msg <- "`grid` should be a positive integer or a data frame."

check_grid <- function(grid, workflow, pset = NULL, call = caller_env()) {
  # `NULL` grid is the signal that we are using `fit_resamples()`
  if (is.null(grid)) {
    return(grid)
  }

  if (is.null(pset)) {
    pset <- hardhat::extract_parameter_set_dials(workflow)
  }

  if (nrow(pset) == 0L) {
    msg <- paste0(
      "No tuning parameters have been detected, ",
      "performance will be evaluated using the resamples with no tuning. ",
      "Did you want to [tune()] parameters?"
    )
    rlang::warn(msg)

    # Return `NULL` as the new `grid`, like what is used in `fit_resamples()`
    return(NULL)
  }

  if (!is.numeric(grid)) {
    if (!is.data.frame(grid)) {
      rlang::abort(grid_msg)
    }

    grid_distinct <- distinct(grid)

    # remove attributes, particularly those tacked on by `expand.grid()`
    grid_distinct <- vctrs::new_data_frame(grid_distinct, n = nrow(grid_distinct))

    if (!identical(nrow(grid_distinct), nrow(grid))) {
      rlang::warn(
        "Duplicate rows in grid of tuning combinations found and removed."
      )
    }
    grid <- grid_distinct

    tune_tbl <- tune_args(workflow)
    tune_params <- tune_tbl$id

    # when called from [tune_bayes()]
    tune_params <- tune_params[tune_params != ".iter"]

    grid_params <- names(grid)

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
    grid <- as.integer(grid[1])
    if (grid < 1) {
      rlang::abort(grid_msg)
    }
    check_workflow(workflow, pset = pset, check_dials = TRUE, call = call)

    grid <- dials::grid_latin_hypercube(pset, size = grid)
    grid <- dplyr::distinct(grid)
  }

  if (!tibble::is_tibble(grid)) {
    grid <- tibble::as_tibble(grid)
  }

  grid
}

needs_finalization <- function(x, nms = character(0)) {
  # If an unknown engine-specific parameter, the object column is missing and
  # no need for finalization
  x <- x[!is.na(x$object), ]
  # If the parameter is in a pre-defined grid, then no need to finalize
  x <- x[!(x$id %in% nms), ]
  if (length(x) == 0) {
    return(FALSE)
  }
  any(dials::has_unknowns(x$object))
}

#' @export
#' @param data The training data.
#' @param grid_names A character vector of column names from the grid.
#' @keywords internal
#' @rdname empty_ellipses
check_parameters <- function(wflow, pset = NULL, data, grid_names = character(0)) {
  if (is.null(pset)) {
    pset <- hardhat::extract_parameter_set_dials(wflow)
  }
  unk <- purrr::map_lgl(pset$object, dials::has_unknowns)
  if (!any(unk)) {
    return(pset)
  }
  tune_param <- tune_args(wflow)
  tune_recipe <- tune_param$id[tune_param$source == "recipe"]
  tune_recipe <- length(tune_recipe) > 0

  if (needs_finalization(pset, grid_names)) {
    if (tune_recipe) {
      rlang::abort(
        paste(
          "Some model parameters require finalization but there are recipe",
          "parameters that require tuning. Please use ",
          "`extract_parameter_set_dials()` to set parameter ranges ",
          "manually and supply the output to the `param_info` argument."
        )
      )
    }
    msg <- "Creating pre-processing data to finalize unknown parameter"
    unk_names <- pset$id[unk]
    if (length(unk_names) == 1) {
      msg <- paste0(msg, ": ", unk_names)
    } else {
      msg <- paste0(msg, "s: ", paste0("'", unk_names, "'", collapse = ", "))
    }

    tune_log(list(verbose = TRUE), split = NULL, msg, type = "info")

    x <- workflows::.fit_pre(wflow, data)$pre$mold$predictors
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

check_installs <- function(x, call = caller_env()) {
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
      needs_installed <- unique(deps[!is_inst])
      cli::cli_abort(
        "{cli::qty(needs_installed)} Package install{?s} {?is/are} \\
         required for {.pkg {needs_installed}}.",
        call = call
      )
    }
  }
}

check_bayes_initial_size <- function(num_param, num_grid, race = FALSE) {
  msg <-
    cli::pluralize(
      "There {cli::qty(num_param)}{?is/are} {num_param} tuning parameter{?s} \\
      and {num_grid} grid point{?s} {?was/were} requested."
    )


  if (race) {
    race_msg <- "With racing, only completely resampled parameters are used."
  } else {
    race_msg <- NULL
  }

  if (num_grid == 1) {
    rlang::abort(
      c(
        msg,
        glue::glue(
          "The GP model requires 2+ initial points. For best performance, \\
          supply more initial points than there are tuning parameters."
        ),
        race_msg
      ),
      call = NULL
    )
  }

  if (num_grid < num_param + 1) {
    diff <- num_param - num_grid + 1
    cli::cli_bullets(
      c(
        `!` = msg,
        `*` = cli::pluralize(
          "There are {cli::qty(diff)}{?as many/more} tuning parameters \\
          {cli::qty(diff)}{?as/than} there are initial points. \\
          This is likely to cause numerical issues in the first few \\
          search iterations."),
        `*` = race_msg
      )
    )
  }

  invisible(NULL)
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

#' @export
#' @keywords internal
#' @rdname empty_ellipses
#' @param check_dials A logical for check for a NULL parameter object.
check_workflow <- function(x, ..., pset = NULL, check_dials = FALSE, call = caller_env()) {
  if (!inherits(x, "workflow")) {
    rlang::abort("The `object` argument should be a 'workflow' object.")
  }

  if (!has_preprocessor(x)) {
    rlang::abort("A formula, recipe, or variables preprocessor is required.")
  }

  if (!has_spec(x)) {
    rlang::abort("A parsnip model is required.")
  }

  rlang::check_dots_empty(call = call)

  if (check_dials) {
    if (is.null(pset)) {
      pset <- hardhat::extract_parameter_set_dials(x)
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

  check_extra_tune_parameters(x)

  check_installs(hardhat::extract_spec_parsnip(x), call = call)

  invisible(NULL)
}

check_extra_tune_parameters <- function(x) {
  mod <- hardhat::extract_spec_parsnip(x)

  to_be_tuned <- hardhat::extract_parameter_set_dials(mod)
  marked_for_tuning <- generics::tune_args(mod)

  if (nrow(marked_for_tuning) > nrow(to_be_tuned)) {
    not_tunable <- marked_for_tuning$name[!marked_for_tuning$name %in% to_be_tuned$name]
    msg <-
      c("!" = "{cli::qty(not_tunable)}The parameter{?s} {.var {not_tunable}}
               {?was/were} marked with `tune()`, though will not be tuned.",
        "i" = "This usually means that the current modeling engine
               {.var {extract_spec_parsnip(x)$engine}}
               does not support tuning {.var {not_tunable}}."
      )


    cli::cli_abort(msg, call = rlang::caller_env(3), class = "not_tunable_error")
  }
  invisible(NULL)
}


#' @export
#' @keywords internal
#' @rdname empty_ellipses
#' @param object A `workflow` object.
check_metrics <- function(x, object) {
  mode <- extract_spec_parsnip(object)$mode

  if (is.null(x)) {
    switch(mode,
           regression = {
             x <- yardstick::metric_set(rmse, rsq)
           },
           classification = {
             x <- yardstick::metric_set(roc_auc, accuracy, brier_class)
           },
           'censored regression' = {
             x <- yardstick::metric_set(brier_survival)
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
  is_surv_metric_set <- inherits(x, c("survival_metric_set"))

  if (!is_numeric_metric_set && !is_class_prob_metric_set && !is_surv_metric_set) {
    rlang::abort("The `metrics` argument should be the results of [yardstick::metric_set()].")
  }

  if (mode == "regression" && !is_numeric_metric_set) {
    msg <- paste0(
      "The parsnip model has `mode = 'regression'`, ",
      "but `metrics` is a metric set for a different model mode."
    )
    rlang::abort(msg)
  }

  if (mode == "classification" && !is_class_prob_metric_set) {
    msg <- paste0(
      "The parsnip model has `mode = 'classification'`, ",
      "but `metrics` is a metric set for a different model mode."
    )
    rlang::abort(msg)
  }

  if (mode == "censored regression" && !is_surv_metric_set) {
    msg <- paste0(
      "The parsnip model has `mode = 'censored regression'`, ",
      "but `metrics` is a metric set for a different model mode."
    )
    rlang::abort(msg)
  }
  x
}

bayes_msg <- "`initial` should be a positive integer or the results of [tune_grid()]"

#' @export
#' @keywords internal
#' @rdname empty_ellipses
#' @param wflow A `workflow` object.
#' @param resamples An `rset` object.
#' @param ctrl A `control_grid` object.
check_initial <- function(x, 
                          pset, 
                          wflow, 
                          resamples,
                          metrics, 
                          eval_time,
                          ctrl,
                          checks = "grid") {
  if (is.null(x)) {
    rlang::abort(bayes_msg)
  }
  if (is.numeric(x)) {
    x <- create_initial_set(pset, n = x, checks = checks)
    if (ctrl$verbose) {
      message()
      msg <- paste0(" Generating a set of ", nrow(x), " initial parameter results")
      tune_log(ctrl, split = NULL, msg, type = "go")
    }

    grid_ctrl <- ctrl
    grid_ctrl$verbose <- FALSE
    x <- tune_grid(
      wflow,
      resamples = resamples,
      grid = x,
      metrics = metrics,
      eval_time = eval_time,
      param_info = pset,
      control = parsnip::condense_control(grid_ctrl, control_grid())
    )

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
    param_nms <- .get_tune_parameter_names(x)
    if (inherits(x, "tune_race")) {
      num_resamples <-
        x %>%
        collect_metrics(summarize = FALSE) %>%
        dplyr::count(.config)
      max_resamples <- max(num_resamples$n)
      configs <- num_resamples$.config[num_resamples$n == max_resamples]
      x <- filter_parameters(x, .config %in% configs)
      num_grid <- length(configs)
      x$.order <- NULL
    } else {
      num_grid <-
        collect_metrics(x) %>%
        dplyr::distinct(!!!rlang::syms(param_nms)) %>%
        nrow()
    }
    if (any(checks == "bayes")) {
      check_bayes_initial_size(length(param_nms), num_grid,
                               race = inherits(x, "tune_race")
      )
    }
  }
  if (!any(names(x) == ".iter")) {
    x <- x %>% dplyr::mutate(.iter = 0L)
  }
  x
}

get_objective_name <- function(x, metrics) {
  if (is.null(x)) {
    metric_data <- metrics_info(metrics)
    x <- metric_data$.metric[1]
  } else {
    # check for a name or acquisition function
  }
  x
}


# ------------------------------------------------------------------------------
# acq functions

check_direction <- function(x) {
  if (!is.logical(x) || length(x) != 1) {
    rlang::abort("`maximize` should be a single logical.")
  }
  invisible(NULL)
}


check_best <- function(x) {
  if (!is.numeric(x) || length(x) != 1 || is.na(x)) {
    rlang::abort("`best` should be a single, non-missing numeric.")
  }
  invisible(NULL)
}


# ------------------------------------------------------------------------------

check_class_or_null <- function(x, cls = "numeric") {
  inherits(x, cls) | is.null(x)
}

#' @export
#' @keywords internal
#' @rdname empty_ellipses
#' @param cls A character vector of possible classes
#' @param where A character string for the calling function.
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

#' @export
#' @keywords internal
#' @rdname empty_ellipses
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


# Check the data going into the GP. If there are all missing values, fail. If some
# are missing, remove them and send a warning. If all metrics are the same, fail.
check_gp_data <- function(x) {
  met <- x$.metric[[1]]

  miss_y <- sum(is.nan(x$mean))
  if (miss_y > 0) {
    if (miss_y == nrow(x)) {
      msg <- cli::pluralize(
        "All of the {met} estimates were missing. The Gaussian process model cannot be fit to the data."
      )
      message_wrap(msg, prefix = "!", color_text = get_tune_colors()$message$danger)
    } else {
      msg <- cli::pluralize(
        "For the {met} estimates, {miss_y} missing {?value was/values were} found and removed before fitting the Gaussian process model."
      )
      message_wrap(msg, prefix = "!", color_text = get_tune_colors()$message$warning)
    }

    x <- x[!is.na(x$mean), ]
  }

  n_uni <- length(unique(x$mean))
  if (n_uni == 1) {
    msg <- glue::glue(
      "All of the {met} values were identical. The Gaussian process model cannot
     be fit to the data. Try expanding the range of the tuning parameters."
    )
    message_wrap(msg, prefix = "!", color_text = get_tune_colors()$message$danger)
  }

  x
}

# If the current GP failed, use a previous one if it exists
check_gp_failure <- function(current, prev) {
  if (inherits(current, "GP")) {
    return(current)
  }

  # first model failed or all previous models failed
  if (is.null(prev) || inherits(prev, "try-error")) {
    rlang::abort("Gaussian process model was not fit.")
  }

  # return prev model
  prev
}

check_no_tuning <- function(x) {
  tune_param <- tune_args(x)
  num_param <- nrow(tune_param)
  if (num_param == 0) {
    return(invisible(FALSE))
  }
  srcs <- unique(tune_param$source)
  num_srcs <- length(srcs)
  srcs <- paste0(srcs, collapse = " and ")
  msg_1 <- cli::pluralize("{num_param} argument{?s} {?has/have} been tagged for tuning in {?this component/these components}: {srcs}. ")
  msg_2 <- "Please use one of the tuning functions (e.g. `tune_grid()`) to optimize them."
  msg <- paste(msg_1, msg_2, sep = "\n")
  rlang::abort(msg, call = NULL)
}

dyn_inputs <- c("integrated_survival_metric", "dynamic_survival_metric")

check_eval_time <- function(eval_time, metrics) {
  metric_types <- tibble::as_tibble(metrics)$class
  needs_eval_time <- any(metric_types %in% dyn_inputs)
  if (!is.null(eval_time) & !needs_eval_time) {
    cli::cli_abort(
      "{.arg eval_time} is only used for dynamic and integrated survival metrics.",
      call = NULL
    )
  }
  if (is.null(eval_time) & needs_eval_time) {
    rlang::abort(
      "One or more metric requires the specification of time points in the `eval_time` argument.",
      call = NULL
    )
  }
  invisible(NULL)

}
