#' @export
#' @keywords internal
#' @rdname empty_ellipses
check_rset <- function(x) {
  if (!inherits(x, "rset")) {
    cli::cli_abort(
      c(
        "The {.arg resamples} argument should be an {.cls rset} object,",
        "i" = "Such objects are produced by {.fn vfold_cv} or other
           {.pkg rsample} functions."
      )
    )
  }
  if (inherits(x, "loo_cv")) {
    cli::cli_abort("Leave-one-out cross-validation is not currently supported with {.pkg tune}.")
  }
  if (inherits(x, "nested_cv")) {
    cli::cli_abort("Nested resampling is not currently supported with tune.")
  }
  if (inherits(x, "permutations")) {
    cli::cli_abort("Permutation samples are not suitable for tuning.")
  }
  invisible(NULL)
}

backend_options_msg <- "{.arg backend_options} should be created by {.fn tune::new_backend_options}."

check_backend_options <- function(backend_options) {
  if (!is.null(backend_options) &&
      !inherits(backend_options, "tune_backend_options")) {
    cli::cli_abort(backend_options_msg)
  }

  invisible(NULL)
}


grid_msg <- "{.arg grid} should be a positive integer or a data frame."

check_grid <- function(grid, workflow, pset = NULL, call = caller_env()) {
  # `NULL` grid is the signal that we are using `fit_resamples()`
  if (is.null(grid)) {
    return(grid)
  }

  if (is.null(pset)) {
    pset <- hardhat::extract_parameter_set_dials(workflow)
  }

  if (nrow(pset) == 0L) {
    cli::cli_warn(c(
      "No tuning parameters have been detected, performance will be
       evaluated using the resamples with no tuning.",
      "Did you want to assign any parameters with a value of {.fn tune}?"
    ))

    # Return `NULL` as the new `grid`, like what is used in `fit_resamples()`
    return(NULL)
  }

  if (!is.numeric(grid)) {
    if (!is.data.frame(grid)) {
      cli::cli_abort(grid_msg)
    }

    grid_distinct <- distinct(grid)

    # remove attributes, particularly those tacked on by `expand.grid()`
    grid_distinct <- vctrs::new_data_frame(grid_distinct, n = nrow(grid_distinct))

    if (!identical(nrow(grid_distinct), nrow(grid))) {
      cli::cli_warn(
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
      n_extra <- length(extra_grid_params)

      cli::cli_abort(
        "The provided grid has {n_extra} parameter column{?s}
         ({.var {extra_grid_params}}) that {?has/have} not been marked for tuning
         by {.fn tune}."
      )
    }

    if (length(extra_tune_params) != 0L) {
      n_extra <- length(extra_tune_params)

      cli::cli_abort(
        "The provided grid is missing the following {n_extra} parameter
         column{?s} that {?has/have} been marked for tuning by {.fn tune}:
         {.val {extra_tune_params}}."
      )
    }
  } else {
    grid <- as.integer(grid[1])
    if (grid < 1) {
      cli::cli_abort(grid_msg)
    }
    check_workflow(workflow, pset = pset, check_dials = TRUE, call = call)

    grid <- dials::grid_space_filling(pset, size = grid)
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
      cli::cli_abort(
        c(
          "Some model parameters require finalization but there are recipe
           parameters that require tuning.",
          "i" = "Please use {.fn extract_parameter_set_dials} to set parameter
           ranges manually and supply the output to the {.arg param_info}
           argument."
        )
      )
    }
    unk_names <- pset$id[unk]
    num_unk <- length(unk_names)
    msg <-
      cli::format_inline(
        "Creating pre-processing data to finalize {num_unk} unknown parameter{?s}: {.val {unk_names}}")

    tune_log(list(verbose = TRUE), split_labels = NULL, msg, type = "info")

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
    cli::cli_abort("Please declare an engine for the model")
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
        "{cli::qty(needs_installed)} Package install{?s} {?is/are}
         required for {.pkg {needs_installed}}.",
        call = call
      )
    }
  }
}

check_bayes_initial_size <- function(num_param, num_grid, race = FALSE) {
  msg <-
    cli::pluralize(
      "There {cli::qty(num_param)}{?is/are} {num_param} tuning parameter{?s}
      and {num_grid} grid point{?s} {?was/were} requested."
    )

  msg_list <- c(
    "{msg}",
    "i" = "The GP model requires 2+ initial points. For best performance,
               supply more initial points than there are tuning parameters."
  )
  bullet_msg <-
    c(
      `!` = "{msg}",
      `*` = cli::pluralize(
        "There are {cli::qty(diff)}{?as many/more} tuning parameters
          {cli::qty(diff)}{?as/than} there are initial points.
          This is likely to cause numerical issues in the first few
          search iterations.")
    )


  if (race) {
    race_msg <- "With racing, only completely resampled parameters are used."
    msg_list <- c(msg_list, "i" = race_msg)
    bullet_msg <- c(bullet_msg, `*` = race_msg)
  }

  if (num_grid == 1) {
    cli::cli_abort(msg_list)
  }

  if (num_grid < num_param + 1) {
    diff <- num_param - num_grid + 1
    cli::cli_bullets(bullet_msg)
  }

  invisible(NULL)
}

check_param_objects <- function(pset) {
  params <- purrr::map_lgl(pset$object, inherits, "param")

  if (!all(params)) {
    cli::cli_abort(
      "The workflow has arguments to be tuned that are missing some parameter
       objects: {.val {pset$id[!params]}}"
    )
  }
  invisible(pset)
}

#' @export
#' @keywords internal
#' @rdname empty_ellipses
#' @param check_dials A logical for check for a NULL parameter object.
check_workflow <- function(x, ..., pset = NULL, check_dials = FALSE, call = caller_env()) {
  if (!inherits(x, "workflow")) {
    cli::cli_abort("The {.arg object} argument should be a {.cls workflow} object.")
  }

  if (!has_preprocessor(x)) {
    cli::cli_abort("A formula, recipe, or variables preprocessor is required.")
  }

  if (!has_spec(x)) {
    cli::cli_abort("A parsnip model is required.")
  }

  rlang::check_dots_empty(call = call)

  if (check_dials) {
    if (is.null(pset)) {
      pset <- hardhat::extract_parameter_set_dials(x)
    }

    check_param_objects(pset)

    incompl <- dials::has_unknowns(pset$object)

    if (any(incompl)) {
      cli::cli_abort(
        "The workflow has arguments whose ranges are not finalized:
         {.val {pset$id[incompl]}}."
      )
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
             cli::cli_abort("Internal error: {.fn check_installs} should have
                            caught an {.val unknown} mode.")
           },
           cli::cli_abort("Unknown {.val mode} for parsnip model.")
    )

    return(x)
  }

  is_numeric_metric_set <- inherits(x, "numeric_metric_set")
  is_class_prob_metric_set <- inherits(x, "class_prob_metric_set")
  is_surv_metric_set <- inherits(x, c("survival_metric_set"))

  if (!is_numeric_metric_set && !is_class_prob_metric_set && !is_surv_metric_set) {
    cli::cli_abort("The {.arg metrics} argument should be the results of
                   {.fn yardstick::metric_set}.")
  }

  if (mode == "regression" && !is_numeric_metric_set) {
    cli::cli_abort(
      c(
        "The parsnip model has {.code mode = 'regression'}, but {.arg metrics}
        is a metric set for a different model mode."
      )
    )
  }

  if (mode == "classification" && !is_class_prob_metric_set) {
    cli::cli_abort(
      c(
        "The parsnip model has {.code mode = 'classification'}, but {.arg metrics}
         is a metric set for a different model mode."
      )
    )
  }

  if (mode == "censored regression" && !is_surv_metric_set) {
    cli::cli_abort(c(
      "The parsnip model has {.code mode = 'censored regression'},
       but {.arg metrics} is a metric set for a different model mode."
    ))
  }
  x
}

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
    cli::cli_abort("{.arg initial} should be a positive integer or the results
                   of {.fn tune_grid}")
  }
  if (is.numeric(x)) {
    x <- create_initial_set(pset, n = x, checks = checks)
    if (ctrl$verbose) {
      message()
      msg <- cli::format_inline(" Generating a set of {nrow(x)} initial parameter results")
      tune_log(ctrl, split_labels = NULL, msg, type = "go")
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
      tune_log(ctrl, split_labels = NULL, "Initialization complete", type = "success")
      message()
    }
  } else {
    if (!inherits(x, "tune_results")) {
      cli::cli_abort("{.arg initial} should be a positive integer or the results
                      of {.fn tune_grid}")
    }
    if (ctrl$save_pred & !any(names(x) == ".predictions")) {
      cli::cli_abort("{.arg save_pred} can only be used if the initial results
                      saved predictions.")
    }
    if (!is.null(ctrl$extract) & !any(names(x) == ".extracts")) {
      cli::cli_abort("The {.fn extract} function can only be used if the initial
                     results have extractions.")
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
# TODO remove this once finetune is updated

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
# TODO remove this once finetune is updated

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
    msg <- cli::pluralize(
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
    cli::cli_abort("Gaussian process model was not fit.")
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
  cli::cli_abort(c(
    "{num_param} argument{?s} {?has/have} been tagged for tuning in
     {?this component/these components}: {srcs}.",
    "i" = "Please use one of the tuning functions (e.g. {.fn tune_grid})
           to optimize them."
  ))
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
    cli::cli_abort(
      "One or more metric requires the specification of time points in the
       {.arg eval_time} argument.",
      call = NULL
    )
  }
  invisible(NULL)

}

check_time_limit_arg <- function(x, call = rlang::caller_env()) {
  if (!inherits(x, c("logical", "numeric")) || length(x) != 1L) {
    cli::cli_abort("{.arg time_limit} should be either a single numeric or
                    logical value.", call = call)
  }
  invisible(NULL)
}
