#' Plot tuning search results
#'
#' @param object A tibble of results from [tune_grid()] or [tune_bayes()].
#' @param type A single character value. Choices are `"marginals"` (for a plot
#'  of each predictor versus performance; see Details below), `"parameters"`
#'  (each parameter versus search iteration), or `"performance"` (performance
#'  versus iteration). The latter two choices are only used for [tune_bayes()].
#' @param metric A character vector or `NULL` for which metric to plot. By
#' default, all metrics will be shown via facets. Possible options are
#' the entries in `.metric` column of `collect_metrics(object)`.
#' @param width A number for the width of the confidence interval bars when
#' `type = "performance"`. A value of zero prevents them from being shown.
#' @param eval_time A numeric vector of time points where dynamic event time
#' metrics should be chosen (e.g. the time-dependent ROC curve, etc). The
#' values should be consistent with the values used to create `object`.
#' @param call The call to be displayed in warnings or errors.
#' @param ... For plots with a regular grid, this is passed to `format()` and is
#' applied to a parameter used to color points. Otherwise, it is not used.
#' @return A `ggplot2` object.
#' @details
#'
#' When the results of `tune_grid()` are used with `autoplot()`, it tries to
#'  determine whether a _regular grid_ was used.
#'
#' ## Regular grids
#'
#'   For regular grids with one or more numeric tuning parameters, the parameter
#'  with the most unique values is used on the x-axis. If there are categorical
#'  parameters, the first is used to color the geometries. All other parameters
#'  are used in column faceting.
#'
#'   The plot has the performance metric(s) on the y-axis. If there are multiple
#'  metrics, these are row-faceted.
#'
#'   If there are more than five tuning parameters, the "marginal effects" plots
#'  are used instead.
#'
#' ## Irregular grids
#'
#' For space-filling or random grids, a _marginal_ effect plot is created. A
#'  panel is made for each numeric parameter so that each parameter is on the
#'  x-axis and performance is on the y-xis. If there are multiple metrics, these
#'  are row-faceted.
#'
#' A single categorical parameter is shown as colors. If there are two or more
#'  non-numeric parameters, an error is given. A similar result occurs is only
#'  non-numeric parameters are in the grid. In these cases, we suggest using
#'  `collect_metrics()` and `ggplot()` to create a plot that is appropriate for
#'  the data.
#'
#' If a parameter has an associated transformation associated with it (as
#' determined by the parameter object used to create it), the plot shows the
#' values in the transformed units (and is labeled with the transformation type).
#'
#' Parameters are labeled using the labels found in the parameter object
#' _except_ when an identifier was used (e.g. `neighbors = tune("K")`).
#'
#' @seealso [tune_grid()], [tune_bayes()]
#' @examplesIf tune:::should_run_examples()
#' # For grid search:
#' data("example_ames_knn")
#'
#' # Plot the tuning parameter values versus performance
#' autoplot(ames_grid_search, metric = "rmse")
#'
#'
#' # For iterative search:
#' # Plot the tuning parameter values versus performance
#' autoplot(ames_iter_search, metric = "rmse", type = "marginals")
#'
#' # Plot tuning parameters versus iterations
#' autoplot(ames_iter_search, metric = "rmse", type = "parameters")
#'
#' # Plot performance over iterations
#' autoplot(ames_iter_search, metric = "rmse", type = "performance")
#' @export
autoplot.tune_results <-
  function(object,
           type = c("marginals", "parameters", "performance"),
           metric = NULL,
           eval_time = NULL,
           width = NULL,
           call = rlang::current_env(),
           ...) {
    type <- match.arg(type)
    has_iter <- any(names(object) == ".iter")
    if (!has_iter && type != "marginals") {
      cli::cli_abort("{.code type = {glue::double_quote(type)}} is only used with
                      iterative search results.")
    }
    pset <- .get_tune_parameters(object)
    if (any(is.na(pset$object))) {
      p_names <- pset$id[is.na(pset$object)]
      cli::cli_abort(
        "Some parameters do not have corresponding parameter objects and
         cannot be used with {.fn autoplot}: {.arg {p_names}}."
      )
    }

    if (type == "parameters") {
      if (!is.null(eval_time)) {
        cli::cli_warn(
          "{.arg eval_time} is not used with {.code autoplot(..., type = 'parameters')}."
        )
      }
      p <- plot_param_vs_iter(object, call)
    } else {
      if (type == "performance") {
        p <- plot_perf_vs_iter(object, metric, eval_time = eval_time, width, call)
      } else {
        if (use_regular_grid_plot(object)) {
          p <- plot_regular_grid(object, metric = metric, eval_time = eval_time, call, ...)
        } else {
          p <- plot_marginals(object, metric = metric, eval_time = eval_time, call)
        }
      }
    }
    p
  }

#' @export
autoplot.resample_results <- function(object, ...) {
  cli::cli_abort("There is no {.fn autoplot} implementation for {.cls resample_results}.")
}

# ------------------------------------------------------------------------------

get_param_object <- function(x) {
  att <- attributes(x)
  if (any(names(att) == "parameters")) {
    res <- att$parameters
  } else {
    res <- NULL
  }
  res
}

get_param_columns <- function(x) {
  prm <- get_param_object(x)
  if (!is.null(prm)) {
    res <- prm$id
  } else {
    dat <- collect_metrics(x)
    other_names <- c(
      ".metric", ".estimator", "mean", "n",
      "std_err", ".iter", ".config"
    )
    res <- names(dat)[!(names(dat) %in% other_names)]
  }
  res
}

# Use the user-given id for the parameter or the parameter label?
get_param_label <- function(x, id_val) {
  x <- tibble::as_tibble(x)
  y <- dplyr::filter(x, id == id_val) %>% dplyr::slice(1)
  num_param <- sum(x$name == y$name)
  no_special_id <- y$name == y$id
  if (no_special_id && num_param == 1) {
    res <- y$object[[1]]$label
  } else {
    res <- id_val
  }
  res
}

# When visualizing fairness metrics, collapse the
# `.metric` and `.by` columns into one string, e.g.
# `demographic_parity(gender)`. Otherwise, `.by` would
# be ignored, so the same fairness metric applied to
# different data-columns (e.g. `demographic_parity(gender)` and
# `demographic_parity(race)`) would be obscured in output.
#
# For non-fairness metrics (i.e. observations with `is.na(.by)`),
# the `.metric` column is unaffected.
paste_param_by <- function(x) {
  if (".by" %in% colnames(x)) {
    x <-
      x %>%
      dplyr::mutate(
        .metric = case_when(
          !is.na(.by) ~ paste0(.metric, "(", .by, ")"),
          .default = .metric
        )
      )
  }

  x
}

# ------------------------------------------------------------------------------

is_factorial <- function(x, cutoff = 0.95) {
  n <- nrow(x)
  p <- ncol(x)
  vals <- purrr::map(x, unique)
  full_fact <-
    tidyr::crossing(!!!vals) %>%
    dplyr::full_join(x %>% dplyr::mutate(..obs = 1), by = names(x))
  mean(!is.na(full_fact$..obs)) >= cutoff
}


is_regular_grid <- function(grid) {
  num_points <- nrow(grid)
  p <- ncol(grid)

  if (p == 1) {
    return(TRUE)
  }

  if (p <= 5) {
    ff <- is_factorial(grid)
    if (ff) {
      return(TRUE)
    }
  }

  pct_unique <- purrr::map_int(grid, ~ length(unique(.x))) / num_points
  max_pct_unique <- max(pct_unique, na.rm = TRUE)
  np_ratio <- p / num_points

  # Derived from simulation data and C5.0 tree
  if (max_pct_unique > 1 / 2) res <- FALSE
  if (max_pct_unique <= 1 / 2 & max_pct_unique <= 1 / 6) res <- TRUE
  if (max_pct_unique <= 1 / 2 & max_pct_unique > 1 / 6 & np_ratio > 0.05) res <- TRUE
  if (max_pct_unique <= 1 / 2 & max_pct_unique > 1 / 6 & np_ratio <= 0.05) res <- FALSE
  res
}

# This will eventually change and use a parameters object.
use_regular_grid_plot <- function(x) {
  dat <- collect_metrics(x)
  param_cols <- get_param_columns(x)
  grd <- dat %>%
    dplyr::select(all_of(param_cols)) %>%
    distinct()
  is_regular_grid(grd)
}

# ------------------------------------------------------------------------------

process_autoplot_metrics <- function(x, metric, eval_time) {
  met_set <- .get_tune_metrics(x)
  any_dyn <- any(purrr::map_lgl(metric, ~ is_dyn(met_set, .x)))

  x <- estimate_tune_results(x)

  # This next line updates the .metric columns to be consistent with what the
  # metric set produces. For example, in the data, there might be a value of
  # "demographic_parity" but the metric set knows about
  # "demographic_parity(cyl)". `paste_param_by()` makes sure that they both
  # have the same value (if any).
  x <- paste_param_by(x)

  x <- x %>%
    dplyr::filter(.metric %in% metric) %>%
    dplyr::filter(!is.na(mean))

  num_eval_times <- length(eval_time[!is.na(eval_time)])

  if(any_dyn & num_eval_times > 0) {
    x <- x %>%
      dplyr::filter(.eval_time %in% eval_time) %>%
      dplyr::mutate(
        .metric =
          dplyr::if_else(
            condition = !is.na(.eval_time),
            true = paste0(.metric, " @", format(.eval_time, digits = 5)),
            false = .metric
          )
      )
  }
  x
}

# ------------------------------------------------------------------------------

plot_perf_vs_iter <- function(x, metric = NULL, eval_time = NULL, width = NULL,
                              call = rlang::caller_env()) {
  if (is.null(width)) {
    width <- max(x$.iter) / 75
  }

  metric <- check_autoplot_metrics(x, metric, call)
  eval_time <- check_autoplot_eval_times(x, metric, eval_time, call)

  x <- process_autoplot_metrics(x, metric, eval_time)

  search_iter <-
    x %>%
    dplyr::filter(.iter > 0 & std_err > 0) %>%
    dplyr::mutate(const = ifelse(n > 0, qt(0.975, n), 0))

  p <-
    ggplot(x, aes(x = .iter, y = mean)) +
    geom_point() +
    xlab("Iteration") +
    scale_x_continuous(breaks = iter_breaks(x$.iter))

  if (nrow(search_iter) > 0 & width > 0) {
    p <-
      p +
      geom_errorbar(
        data = search_iter,
        aes(ymin = mean - const * std_err, ymax = mean + const * std_err),
        width = width
      )
  }

  if (length(unique(x$.metric)) > 1) {
    p <- p + facet_wrap(~.metric, scales = "free_y")
  } else {
    p <- p + ylab(unique(x$.metric))
  }
  p
}

plot_param_vs_iter <- function(x, call = rlang::caller_env()) {
  param_cols <- get_param_columns(x)
  pset <- get_param_object(x)
  if (is.null(pset)) {
    cli::cli_abort("{.fn autoplot} requires objects made with {.pkg tune}
                    version 0.1.0 or later.")
  }

  # ----------------------------------------------------------------------------
  # Collect and filter resampling results

  x <- estimate_tune_results(x)
  is_num <- purrr::map_lgl(x %>% dplyr::select(dplyr::all_of(param_cols)), is.numeric)
  num_param_cols <- param_cols[is_num]

  # ----------------------------------------------------------------------------
  # Transform and re-label when needed. Previous vectors of names are updated.

  for (prm in param_cols) {
    pobj <- pset$object[[which(pset$id == prm)]]
    lab <- get_param_label(pset, prm)
    if (!is.null(pobj$trans)) {
      x[[prm]] <- pobj$trans$transform(x[[prm]])
      new_name <- paste0(lab, " (", pobj$trans$name, ")")
    } else {
      new_name <- lab
    }
    names(x)[names(x) == prm] <- new_name
    num_param_cols[num_param_cols == prm] <- new_name
    param_cols[param_cols == prm] <- new_name
  }

  # ----------------------------------------------------------------------------
  # Stack numeric columns for filtering

  x <-
    x %>%
    dplyr::select(.iter, dplyr::all_of(num_param_cols)) %>%
    tidyr::pivot_longer(cols = dplyr::all_of(num_param_cols))

  # ------------------------------------------------------------------------------

  p <-
    ggplot(x, aes(x = .iter, y = value)) +
    geom_point() +
    xlab("Iteration") +
    scale_x_continuous(breaks = iter_breaks(x$.iter))

  if (length(param_cols) == 1) {
    p <- p  + ylab(param_cols)
  } else {
    p <- p + ylab("") + facet_wrap(~name, scales = "free_y")
  }

  p
}

plot_marginals <- function(x, metric = NULL, eval_time = NULL, call = rlang::caller_env()) {
  param_cols <- get_param_columns(x)
  pset <- get_param_object(x)
  if (is.null(pset)) {
    cli::cli_abort("{.fn autoplot} requires objects made with {.pkg tune}
                   version 0.1.0 or later.")
  }

  # ----------------------------------------------------------------------------
  # Collect and filter resampling results

  is_race <- inherits(x, "tune_race")

  metric <- check_autoplot_metrics(x, metric, call)
  eval_time <- check_autoplot_eval_times(x, metric, eval_time, call)

  x <- process_autoplot_metrics(x, metric, eval_time)

  # ----------------------------------------------------------------------------
  # Check types of parameters then sort by unique values

  is_num <- purrr::map_lgl(x %>% dplyr::select(dplyr::all_of(param_cols)), is.numeric)
  num_val <- purrr::map_int(x %>% dplyr::select(dplyr::all_of(param_cols)), ~ length(unique(.x)))

  if (any(num_val < 2)) {
    rm_param <- param_cols[num_val < 2]
    param_cols <- param_cols[num_val >= 2]
    is_num <- is_num[num_val >= 2]
    x <- x %>% dplyr::select(-dplyr::all_of(rm_param))
  }

  if (any(!is_num)) {
    num_param_cols <- param_cols[is_num]
    chr_param_cols <- param_cols[!is_num]
    if (length(chr_param_cols) > 1) {
      cli::cli_abort("Currently cannot autoplot grids with 2+ non-numeric parameters.")
    }
    if (length(num_param_cols) == 0) {
      cli::cli_abort("Currently cannot autoplot grids with only non-numeric parameters.")
    }
    num_val <- num_val[param_cols %in% chr_param_cols]
    names(num_val) <- chr_param_cols
    num_val <- sort(num_val, decreasing = TRUE)
  } else {
    num_param_cols <- param_cols
    chr_param_cols <- character(0)
  }

  # ----------------------------------------------------------------------------
  # Transform and re-label when needed. Previous vectors of names are updated.

  for (prm in param_cols) {
    pobj <- pset$object[[which(pset$id == prm)]]
    lab <- get_param_label(pset, prm)
    if (!is.null(pobj$trans)) {
      x[[prm]] <- pobj$trans$transform(x[[prm]])
      new_name <- paste0(lab, " (", pobj$trans$name, ")")
    } else {
      new_name <- lab
    }
    names(x)[names(x) == prm] <- new_name
    num_param_cols[num_param_cols == prm] <- new_name
    chr_param_cols[chr_param_cols == prm] <- new_name
    param_cols[param_cols == prm] <- new_name
  }

  # ----------------------------------------------------------------------------
  # Stack numeric parameters for faceting.

  x <-
    x %>%
    dplyr::rename(`# resamples` = n) %>%
    dplyr::select(dplyr::all_of(param_cols), mean, `# resamples`, .metric) %>%
    tidyr::pivot_longer(cols = dplyr::all_of(num_param_cols))

  # ----------------------------------------------------------------------------

  p <- ggplot(x, aes(x = value, y = mean))

  if (length(chr_param_cols) > 0) {
    if (is_race) {
      p <- p + geom_point(aes(col = !!sym(chr_param_cols), alpha = `# resamples`, size = resamples))
      p <- p + ggplot2::labs(color = chr_param_cols)
    } else {
      p <- p + geom_point(aes(col = !!sym(chr_param_cols)), alpha = .7)
      p <- p + ggplot2::labs(color = chr_param_cols)
    }
  } else {
    if (is_race) {
      p <- p + geom_point(aes(alpha = `# resamples`, size = `# resamples`))
    } else {
      p <- p + geom_point(alpha = .7)
    }
  }

  if (length(unique(x$.metric)) > 1) {
    if (length(num_param_cols) == 1) {
      p <-
        p +
        facet_wrap(~.metric, scales = "free_y") +
        xlab(num_param_cols) +
        ylab("")
    } else {
      p <-
        p +
        ggplot2::facet_grid(.metric ~ name, scales = "free") +
        xlab("") +
        ylab("")
    }
  } else {
    if (length(num_param_cols) == 1) {
      p <- p + xlab(num_param_cols) + ylab(unique(x$.metric))
    } else {
      p <-
        p +
        facet_wrap(~name, scales = "free_x") +
        xlab("") +
        ylab(unique(x$.metric))
    }
  }

  p
}


plot_regular_grid <- function(x,
                              metric = NULL,
                              eval_time = NULL,
                              call = rlang::caller_env(), ...) {
  # Collect and filter resampling results

  is_race <- inherits(x, "tune_race")

  metric <- check_autoplot_metrics(x, metric, call)
  eval_time <- check_autoplot_eval_times(x, metric, eval_time, call)

  dat <- process_autoplot_metrics(x, metric, eval_time)

  check_singular_metric(dat, call)

  multi_metrics <- length(unique(dat$.metric)) > 1

  # ----------------------------------------------------------------------------
  # Get information about parameters

  param_cols <- get_param_columns(x)
  pset <- get_param_object(x)
  if (is.null(pset)) {
    cli::cli_abort("The {.fn autoplot} function requires objects made with
                    {.pkg tune} version 0.1.0 or later.")
  }

  grd <- dat %>% dplyr::select(all_of(param_cols))

  # ----------------------------------------------------------------------------
  # Determine which parameter goes on the x-axis and their types

  is_num <- purrr::map_lgl(grd, is.numeric)
  num_param_cols <- param_cols[is_num]
  chr_param_cols <- param_cols[!is_num]

  num_values <- purrr::map_int(grd[, num_param_cols], ~ length(unique(.x)))
  num_values <- sort(num_values, decreasing = TRUE)

  if (!any(is_num)) {
    x_col <- chr_param_cols[1]
    grp_cols <- chr_param_cols[-1]
  } else {
    x_col <- names(num_values)[1]
    grp_cols <- c(chr_param_cols, names(num_values)[-1])
  }

  g <- length(grp_cols)

  # ----------------------------------------------------------------------------

  if (g > 5) {
    return(plot_marginals(x, metric, eval_time, call))
  }

  # ----------------------------------------------------------------------------
  # Transform and re-label when needed. Previous vectors of names are updated.

  x_col_prm <- pset$object[[which(pset$id == x_col)]]
  if (inherits(x_col_prm, "quant_param") && !is.null(x_col_prm$trans)) {
    trans <- x_col_prm$trans
  } else {
    trans <- NULL
  }

  for (prm in param_cols) {
    pobj <- pset$object[[which(pset$id == prm)]]
    new_name <- get_param_label(pset, prm)
    names(dat)[names(dat) == prm] <- new_name
    grp_cols[grp_cols == prm] <- new_name
    x_col[x_col == prm] <- new_name
    param_cols[param_cols == prm] <- new_name
  }

  # ----------------------------------------------------------------------------

  dat <-
    dat %>%
    dplyr::rename(`# resamples` = n) %>%
    dplyr::select(dplyr::all_of(param_cols), mean, `# resamples`, .metric) %>%
    tidyr::pivot_longer(cols = dplyr::all_of(x_col))

  # ------------------------------------------------------------------------------

  if (g >= 1) {
    # Here we know that there is at least one grouping parameter. For the first,
    # we assign it to the color aesthetic. If it is numeric, it is converted to
    # character using format().
    col_col <- grp_cols[1]
    if (is.numeric(dat[[col_col]])) {
      dat[[col_col]] <- format(dat[[col_col]], ...)
    }
    col_col <- rlang::ensym(col_col)
    p <- ggplot(dat, aes(value, y = mean,
      col = {{col_col}}, group = {{col_col}}
    ))
    # Since `col_col` has either the parameter id or the parameter label, use
    # is in the key:

    p <- p + ggplot2::labs(color = col_col, x = x_col)

    if (g >= 2) {
      # Since there at 2 - 5 grouping parameters, the others are assigned to
      # column facets. Row facets will be for performance metrics.
      facets <- grp_cols[-1]
      facets <- purrr::map(facets, sym)
      facets <- rlang::quos(!!!facets)
      # faceting variables
      if (multi_metrics) {
        p <- p + facet_grid(
          rows = vars(.metric), vars(!!!facets),
          labeller = ggplot2::labeller(.cols = ggplot2::label_both),
          scales = "free_y"
        )
      } else {
        p <-
          p + facet_wrap(vars(!!!facets),
            labeller = ggplot2::labeller(.cols = ggplot2::label_both)
          )
      }
    } else if (multi_metrics) {
      p <- p + facet_grid(rows = vars(.metric), scales = "free_y")
    }
  } else {
    # Only a single parameter and potentially multiple metrics.
    p <- ggplot(dat, aes(x = value, y = mean))
    if (multi_metrics) {
      p <- p + facet_wrap(~.metric, scales = "free_y", ncol = 1)
    }
  }

  if (is_race) {
    p <- p + geom_point(aes(alpha = `# resamples`, size = `# resamples`))
  } else {
    p <- p + geom_point(size = 1)
  }

  if (multi_metrics) {
    p <- p + ylab("")
  } else {
    dat$.metric[1]
    p <- p + ylab(dat$.metric[1])
  }
  if (nrow(pset) == 1) {
    x_lab <- pset$object[[1]]$label
    p <- p + xlab(x_lab)
  }

  if (any(is_num)) {
    p <- p + geom_line()
  }

  if (!is.null(trans)) {
    p <- p + ggplot2::scale_x_continuous(trans = trans)
  }

  p
}


# ------------------------------------------------------------------------------

iter_breaks <- function(x, num = 10) {
  un_z <- sort(unique(x))
  n <- length(un_z)
  if (n < num) {
    num <- n
  }
  brks <- pretty(un_z, n = num)
  brks <- unique(floor(brks))
  names(brks) <- attr(brks, "labels")
  brks
}

