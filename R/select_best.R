#' Obtain the best score with parameters produced by tuning
#'
#' Get the best estimator that was chosen by the search, i.e. estimator which gave the highest performance score
#' (or Parameter setting that gave the best results) on the assessment data.
#'
#' @param x The results of `tune_grid()` or `tune_Bayes()`.
#' @param metric A character value. The metric depend on the results and the mode of the
#' model. e.g. for `mode = regression` the metrics could be `rmse, rsq, mase` etc.
#' and for `mode = classification` it could be: `accuracy, kap, recall, precision, sens, spec, roc_auc, pr_auc, average_precision`
#' etc. (See
#'   \url{https://tidymodels.github.io/yardstick/articles/metric-types.html} for more details.)
#' OR a user defined metric for performance during tuning.
#' @param n_top number of top results/rows to return.
#' @param performance A logical value (TRUE/FALSE) to indicate if columns for the corresponding
#' performance estimates should also be returned.
#' @param maximize Direction of "asc" for ascending or "desc" for descending scores. e.g. `rmse` might require ascending whilst `rsq` might require desc sort. Will be automated later.
#' @return A tibble. The column names depend on the results and the mode of the
#' model as well as the specified metric type.
#' @examples
#' \dontrun{
#' grid_knn <- tune_grid(wflow_obj, rs = cv_splits, control = grid_control(verbose = TRUE))
#' select_best(grid_knn, metric = "rmse", n_top = 2)
#' select_best(grid_knn, metric = "rmse")
#' select_best(grid_knn, metric = "rsq", maximize = "desc")
#' }
#' @export
select_best <- function(x, metric = NA, n_top = 1, performance = TRUE, maximize = "asc") {
  if (is.na(metric) | length(metric) > 1) {
    rlang::abort("Please specify a single character value for metric to get the best score/params...")
  }

  # get estimates/summarise
  summary_res <- estimate(x) %>% dplyr::filter(.metric %in% c(metric))

  if (nrow(summary_res %>% dplyr::filter(.metric == metric)) == 0) {
    rlang::abort("No results are available. Please check trained performance metrics returned by the model tunning")
  }

  if (performance == TRUE) {
    if (maximize == "asc") {
      res <- summary_res %>%
        dplyr::arrange(mean) %>%
        dplyr::slice(1:n_top)
      return(res)
    } else if (maximize == "desc") {
      res <- summary_res %>%
        dplyr::arrange(desc(mean)) %>%
        dplyr::slice(1:n_top)
      return(res)
    } else {
      rlang::abort("Results sorting can be ascending or descing only")
    }
  }
  # if performance cols are not required
  else {
    if (maximize == "asc") {
      res <- summary_res %>%
        dplyr::arrange(mean) %>%
        dplyr::select(mean) %>%
        dplyr::rename(!!paste0("best_", metric) := mean) %>%
        dplyr::slice(1:n_top)
      return(res)
    }
    else if (maximize == "desc") {
      res <- summary_res %>%
        dplyr::arrange(desc(mean)) %>%
        dplyr::select(mean) %>%
        dplyr::rename(!!paste0("best_", metric) := mean) %>%
        dplyr::slice(1:n_top)
      return(res)
    }
    else {
      rlang::abort("Results sorting can be ascending or descing only")
    }
  }
}
