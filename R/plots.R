#' Plot sequential search results
#'
#' @param x A tibble of results for `tune_Bayes()`.
#' @param metric A character vector or `NULL` for which outcome to plot.
#' @param width A number for the width of the confidence interval bars. A value
#' of zero prevents them from being shown.
#' @return A `ggplot2` object.
#'
#' @export
plot_perf_vs_iter <- function(x, metric = NULL, width = max(x$.iter)/75) {
  x <- estimate(x)
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


plot_marginals <- function(x, metric = NULL) {
  # maybe add workflow to get transformations, bust up into modules
  x <- estimate(x)
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
