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
  x <- summarize(x)
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
