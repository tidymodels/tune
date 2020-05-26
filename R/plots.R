#' Plot tuning search results
#'
#' @param object A tibble of results from [tune_grid()] or [tune_bayes()].
#' @param type A single character value. Choices are `"marginals"` (for a plot
#'  of each predictor versus performance; see Details below), `"parameters"`
#'  (each parameter versus search iteration), or `"performance"` (performance
#'  versus iteration). The latter two choices are only used for [tune_bayes()].
#' @param metric A character vector or `NULL` for which metric to plot. By
#' default, all metrics will be shown via facets.
#' @param width A number for the width of the confidence interval bars when
#' `type = "performance"`. A value of zero prevents them from being shown.
#' @param ... Not currently used.
#' @return A `ggplot2` object.
#' @details
#' The parameters are currently represented in their natural units.
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
#'  are row-facetted.
#'
#' A single categorical parameter is shown as colors. If there are two or more
#'  non-numeric parameters, an error is given. A similar result occurs is only
#'  non-numeric parameters are in the grid. In these cases, we suggest using
#'  `collect_metrics()` and `ggplot()` to create a plot that is appropriate for
#'  the data.
#' @seealso [tune_grid()], [tune_bayes()]
#' @examples
#' \donttest{
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
#' }
#' @export
autoplot.tune_results <-
  function(object,
           type = c("marginals", "parameters", "performance"),
           metric = NULL,
           width = NULL,
           ...) {
    type <- match.arg(type)
    has_iter <- any(names(object) == ".iter")
    if (!has_iter && type != "marginals") {
      rlang::abort(paste0("`type = ", type, "` is only used iterative search results."))
    }

    if (type == "parameters") {
      p <- plot_param_vs_iter(object)
    } else {
      if (type == "performance") {
        p <- plot_perf_vs_iter(object, metric, width)
      } else {
        if (regular_grid_plot(object)) {
          p <-  plot_regular_grid(object, metric = metric, ...)
        } else {
          p <- plot_marginals(object, metric = metric)
        }
      }
    }
    p
  }

#' @export
autoplot.resample_results <- function(object, ...) {
  rlang::abort("There is no `autoplot()` implementation for `resample_results`.")
}

# ------------------------------------------------------------------------------

is_regular_grid <- function(grid) {
  num_points <- nrow(grid)
  p <- ncol(grid)

  if (p == 1) {
    return(TRUE)
  }

  pct_unique <- purrr::map_int(grid, ~ length(unique(.x)))/num_points
  max_pct_unique <- max(pct_unique, na.rm = TRUE)
  np_ratio <- p/num_points

  # Derived from simulation data and C5.0 tree
  if (max_pct_unique >  1/2) res <- FALSE
  if (max_pct_unique <= 1/2 & max_pct_unique <= 1/6) res <- TRUE
  if (max_pct_unique <= 1/2 & max_pct_unique >  1/6 & np_ratio >  0.05) res <- TRUE
  if (max_pct_unique <= 1/2 & max_pct_unique >  1/6 & np_ratio <= 0.05) res <- FALSE
  res
}

# This will eventually change and use a parameters object.
regular_grid_plot <- function(x) {
  dat <- collect_metrics(x)
  other_names <- c(".metric", ".estimator", "mean", "n", "std_err", ".iter")
  param_cols <- names(dat)[!(names(dat) %in% other_names)]
  grd <- dat %>% dplyr::select(one_of(param_cols)) %>% distinct()
  is_regular_grid(grd)
}

# ------------------------------------------------------------------------------

plot_perf_vs_iter <- function(x, metric = NULL, width = NULL) {
  if (is.null(width)) {
    width <- max(x$.iter)/75
  }
  x <- estimate_tune_results(x)
  if (!is.null(metric)) {
    x <- x %>% dplyr::filter(.metric %in% metric)
  }
  x <- x %>% dplyr::filter(!is.na(mean))

  search_iter <-
    x %>%
    dplyr::filter(.iter > 0 & std_err > 0) %>%
    dplyr::mutate(const = ifelse(n > 0, qt(0.975, n), 0))

  p <-
    ggplot(x, aes(x = .iter, y = mean)) +
    geom_point() +
    xlab("Iteration")

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
    p <- p + facet_wrap(~ .metric, scales = "free_y")
  } else {
    p <- p + ylab(unique(x$.metric))
  }
  p
}

plot_param_vs_iter <- function(x) {
  x <- estimate_tune_results(x)
  excl_cols <- c(".metric", ".estimator", "mean", "n", "std_err", ".iter")
  param_cols <- names(x)[!((names(x) %in% excl_cols))]
  is_num <- purrr::map_lgl(x %>% dplyr::select(dplyr::one_of(param_cols)), is.numeric)
  num_param_cols <- param_cols[is_num]
  x <-
    x %>%
    dplyr::select(.iter, dplyr::one_of(num_param_cols)) %>%
    tidyr::pivot_longer(cols = dplyr::one_of(num_param_cols))

  p <-
    ggplot(x, aes(x = .iter, y = value)) +
    geom_point() +
    xlab("Iteration") +
    ylab("Parameter Value") +
    facet_wrap(~ name, scales = "free_y")

  p
}

plot_marginals <- function(x, metric = NULL) {
  x <- estimate_tune_results(x)
  if (!is.null(metric)) {
    x <- x %>% dplyr::filter(.metric %in% metric)
  }
  x <- x %>% dplyr::filter(!is.na(mean))

  excl_cols <- c(".metric", ".estimator", "mean", "n", "std_err", ".iter")
  param_cols <- names(x)[!((names(x) %in% excl_cols))]

  is_num <- purrr::map_lgl(x %>% dplyr::select(param_cols), is.numeric)
  num_val <- purrr::map_int(x %>% dplyr::select(param_cols), ~ length(unique(.x)))

  if (any(num_val < 2)) {
    rm_param <- param_cols[num_val < 2]
    param_cols <- param_cols[num_val >= 2]
    is_num <- is_num[num_val >= 2]
    x <- x %>% dplyr::select(-dplyr::one_of(rm_param))
  }

  if (any(!is_num)) {
    num_param_cols <- param_cols[ is_num]
    chr_param_cols <- param_cols[!is_num]
    if (length(chr_param_cols) > 1) {
      rlang::abort("Currently cannot autoplot grids with 2+ non-numeric parameters.")
    }
    if (length(num_param_cols) == 0) {
      rlang::abort("Currently cannot autoplot grids with only non-numeric parameters.")
    }
    num_val <- num_val[param_cols %in% chr_param_cols]
    names(num_val) <- chr_param_cols
    num_val <- sort(num_val, decreasing = TRUE)
  } else {
    num_param_cols <- param_cols
    chr_param_cols <- character(0)
  }

  x <-
    x %>%
    dplyr::select(dplyr::one_of(param_cols), mean, .metric) %>%
    tidyr::pivot_longer(cols = dplyr::one_of(num_param_cols))

  p <- ggplot(x, aes(x = value, y = mean))

  if (length(chr_param_cols) > 0) {
    p <- p + geom_point(aes(col = !!sym(chr_param_cols)), alpha = .7)
  } else {
    p <- p + geom_point(alpha = .7)
  }

  if (length(unique(x$.metric)) > 1) {
    if (length(num_param_cols) == 1) {
      p <-
        p +
        facet_wrap(~ .metric, scales = "free_y") +
        xlab(num_param_cols)  +
        ylab("Performance")
    } else {
      p <-
        p +
        ggplot2::facet_grid(.metric ~ name, scales = "free") +
        xlab("Parameter Value") +
        ylab("Performance")
    }
  } else {
    if (length(num_param_cols) == 1) {
      p <- p + xlab(num_param_cols) +  ylab(unique(x$.metric))
    } else {
      p <-
        p +
        facet_wrap(~ name, scales = "free_x") +
        xlab("Parameter Value") +
        ylab(unique(x$.metric))
    }
  }

  p
}


plot_regular_grid <- function(x, metric = NULL, ...) {
  # eventually pull a parameter set from x attributes
  dat <- collect_metrics(x)
  if (!is.null(metric)) {
    dat <- dat %>% dplyr::filter(.metric %in% metric)
  }
  dat <- dat %>% dplyr::filter(!is.na(mean))
  multi_metrics <- length(unique(dat$.metric)) > 1

  # ----------------------------------------------------------------------------

  excl_cols <- c(".metric", ".estimator", "mean", "n", "std_err", ".iter")
  param_cols <- names(dat)[!((names(dat) %in% excl_cols))]

  # ----------------------------------------------------------------------------

  grd <- dat %>% dplyr::select(one_of(param_cols))

  # ----------------------------------------------------------------------------
  # get parameter labels (also for colored parameters)
  # transform any predictors
  # relabel with transformation (if needed)
  # ----------------------------------------------------------------------------


  is_num <- purrr::map_lgl(grd, is.numeric)
  num_param_cols <- param_cols[ is_num]
  chr_param_cols <- param_cols[!is_num]

  num_values <- purrr::map_int(grd[, num_param_cols], ~ length(unique(.x)))
  num_values <- sort(num_values, decreasing = TRUE)

  if (!any(is_num)) {
    x_col <-  chr_param_cols[1]
    grp_cols <- chr_param_cols[-1]
  } else {
    x_col <-  names(num_values)[1]
    grp_cols <- c(chr_param_cols, names(num_values)[-1])
  }

  g <- length(grp_cols)

  if (g > 5) {
    return(autoplot(x, metric = metric))
  }


  dat <-
    dat %>%
    dplyr::select(dplyr::one_of(param_cols), mean, .metric) %>%
    tidyr::pivot_longer(cols = dplyr::one_of(x_col))

  # ------------------------------------------------------------------------------

  if (g >= 1) {
    # col aes and possibly faceting variables
    if (is.numeric(dat[[ grp_cols[1] ]])) {
      dat[[ grp_cols[1] ]] <- format(dat[[ grp_cols[1] ]], ...)
    }
    p <- ggplot(dat, aes_string(x = "value", y = "mean", col = grp_cols[[1]], group = grp_cols[[1]]))

    if (g >= 2) {
      facets <- grp_cols[-1]
      facets <- purrr::map(facets, sym)
      facets <- rlang::quos(!!!facets)
      # faceting variables
      if (multi_metrics) {
        # TODO fix labels to be columns only
        p <- p + facet_grid(rows = vars(.metric), vars(!!!facets), labeller = "label_both", scales = "free_y")
      } else {
        p <- p + facet_wrap(vars(!!!facets), labeller = "label_both")
      }
    } else if (multi_metrics) {
      p <- p + facet_grid(rows = vars(.metric), scales = "free_y")
    }

  } else {
    p <- ggplot(dat, aes(x = value, y = mean))
    if (multi_metrics) {
      p <- p + facet_wrap(~ .metric, scales = "free_y", ncol = 1)
    }
  }

  p <- p + geom_point() + xlab(x_col)
  if (multi_metrics) {
    p <- p + ylab("Metric")
  } else {
    dat$.metric[1]
    p <- p + ylab(dat$.metric[1])
  }


  if (any(is_num)) {
    p <- p + geom_line()
  }

  p
}
