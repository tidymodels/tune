#' Estimate and correct optimization bias
#'
#' Using the same model tuning results to find optimal settings _and_ to measure
#' optimal performance might lead to optimistic performance estimates.
#' [debias_estimate()] uses the individual resampling statistics to estimate
#' the bias and adjust the overall resampling estimates.
#'
#' @param x An object with class `"tune_results"`. These are produced by
#' functions such as [tune_grid()], [tune_bayes()], and so on. There should be
#' at least two resamples (e.g., validation sets cannot be used).
#' @return A tibble that mimics the output of [show_best()]. There is a row for
#' each metric and additional columns `mean_bias`, `std_err_bias`, and
#' `adjusted`. The latter is a corrected value of the `mean` column.
#' @references R J Tibshirani, R Tibshirani "A bias correction for the
#' minimum error rate in cross-validation," _The Annals of Applied Statistics_,
#' 3(2), 822-829, (June 2009)
#' @examples
#' debias_estimate(ames_grid_search)
#' debias_estimate(ames_iter_search)
#' @export
debias_estimate <- function(x) {
  if ( !inherits(x, "tune_results") ) {
    cli::cli_abort("{.arg x} should have class {.val tune_results}")
  }

  met_nms <- tune::.get_tune_metric_names(x)
  met_info <- tune::metrics_info(.get_tune_metrics(x))
  prm_nms <- tune::.get_tune_parameter_names(x)
  id_cols <- grep("^id", names(x), value = TRUE)

  num_resamples <-
    x %>%
    dplyr::select(dplyr::starts_with("id")) %>%
    dplyr::distinct() %>%
    nrow()

  if ( num_resamples < 2 ) {
    cli::cli_abort("{.arg x} should have at least two resamples.")
  }

  met_sign <-
    met_info %>%
    dplyr::mutate(sign = dplyr::if_else(direction == "minimize", 1, -1)) %>%
    dplyr::select(.metric, sign)

  rs_res <- tune::collect_metrics(x, summarize = FALSE)

  best_indiv_configs <-
    rs_res %>%
    dplyr::select(dplyr::starts_with("id"), .metric, .estimate) %>%
    dplyr::full_join(met_sign, by = ".metric") %>%
    dplyr::mutate(signed = .estimate * sign) %>%
    dplyr::slice_min(signed, n = 1, with_ties = FALSE, by = c(.metric, starts_with("id"))) %>%
    dplyr::select(dplyr::starts_with("id"), .metric, sign, indiv = .estimate)

  best_overeall <- purrr::map_dfr(met_nms, ~ tune::show_best(x, metric = .x, n = 1))

  best_overeall_configs <-
    best_overeall %>%
    dplyr::select(.metric, all_of(prm_nms)) %>%
    dplyr::inner_join(rs_res, by = c(".metric", prm_nms)) %>%
    dplyr::select(dplyr::starts_with("id"), .metric, overall = .estimate)

  bias <-
    best_indiv_configs %>%
    dplyr::full_join(best_overeall_configs, by = c(".metric", id_cols)) %>%
    dplyr::mutate(bias = (overall - indiv)) %>%
    dplyr::filter(!is.na(bias)) %>%
    dplyr::group_by(.metric) %>%
    dplyr::summarize(
      mean_bias = mean(bias),
      std_err_bias = sd(bias) / sqrt(length(bias))
    )

  dplyr::full_join(best_overeall, bias, by = ".metric") %>%
    dplyr::mutate(adjusted = mean + mean_bias) %>%
    dplyr::relocate(adjusted, .before = mean) %>%
    dplyr::relocate(.config, .after = dplyr::everything())
}
