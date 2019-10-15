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
#' OR a user defined metric for performance.
#' @param n_top number of top results to return.
#' @param performance A logical value (TRUE/FALSE) to indicate if columns for the corresponding
#' performance estimates should also be returned.
#' @param maximize automate this later but will need an argument in the short term. Not currently in use.
#' @return A tibble. The column names depend on the results and the mode of the
#' model as well as the specified metric type.
#' @examples
#' \dontrun{
#' initial_grid_knn <- tune_grid(ames_wflow,rs = cv_splits,grid = grid, control = grid_control(verbose = TRUE))
#' select_best(initial_grid_knn,metric = "rsq", n_top = 2)
#' select_best(initial_grid_knn,metric = "rmse")
#' select_best(initial_grid_knn,metric = "rmse", performance = FALSE)
#' }
#' @export
select_best <- function(x, metric=NA,n_top = 1,performance = TRUE,maximize = NULL) {

  if (is.na(metric)) {
    stop("Please specify a character value for metric to get the best score/params...")
  }

  metric = tolower(metric)

  #metrics that require ascending/desc sorting
  valid_asc_metrics <- c("rmse" , "mase" , "rpd")
  valid_desc_metrics <- c("rsq" , "accuracy" , "kap" , "sens" , "spec", "recall", "precision")

  #check if invalid metrics are provided
  if (!(metric %in% valid_asc_metrics | metric %in% valid_desc_metrics)){
    stop("Invalid Performance metric: Please check performance metrics names returned by the model tunning")
  }

  #get estimates/summarise
  summary_res <- tune:::estimate(x) %>% dplyr::filter(.metric %in% c(metric))

  if (performance == TRUE) {

    if (metric %in% valid_asc_metrics & nrow(summary_res %>% dplyr::filter(.metric == metric)) != 0) {
      summary_res%>%
        dplyr::arrange(mean) %>%
        dplyr::slice(1:n_top)
    }
    else if (metric %in% valid_desc_metrics & nrow(summary_res %>% dplyr::filter(.metric == metric)) !=0) {
      summary_res%>%
        dplyr::arrange(desc(mean)) %>%
        dplyr::slice(1:n_top)
    } else stop("No results are available. Please check trained performance metrics returned by the model tunning")
  }
  # if performance cols are not required
  else {
    if (metric %in% valid_asc_metrics & nrow(summary_res %>% dplyr::filter(.metric == metric)) != 0) {
      summary_res%>%
        dplyr::arrange(mean) %>%
        dplyr::select(mean) %>% dplyr::rename(!!paste0("best_",metric) := mean) %>%
        dplyr::slice(1:n_top)
    }
    else if (metric %in% valid_desc_metrics & nrow(summary_res %>% dplyr::filter(.metric == metric)) !=0) {
      summary_res %>%
        dplyr::arrange(desc(mean)) %>%
        dplyr::select(mean) %>% dplyr::rename(!!paste0("best_",metric) := mean) %>%
        dplyr::slice(1:n_top)
    } else stop("No results are available. Please check trained performance metrics returned by the model tunning")
  }
}
