#' Bootstrap confidence intervals for performance metrics
#'
#' Using out-of-sample predictions, the bootstrap is used to create percentile
#' confidence intervals.
#' @param .data A object with class `tune_results` where the `save_pred = TRUE`
#' option was used in the control function.
#' @param metrics A [yardstick::metric_set()]. By default, it uses the same
#' metrics as the original object.
#' @param times The number of bootstrap samples.
#' @param alpha Level of significance.
#' @param allow_par A logical to allow parallel processing (if a parallel
#' backend is registered).
#' @param event_level A single string. Either `"first"` or `"second"` to specify
#' which level of truth to consider as the "event".
#' @param ... Not currently used.
#' @return A tibble of metrics with additional columns for `.lower` and
#' `.upper`.
#' @details
#' For each model configuration (if any), this function takes bootstrap samples
#' of the out-of-sample predicted values. For each bootstrap sample, the metrics
#' are computed and these are used to compute confidence intervals.
#' See [rsample::int_pctl()] and the references therein for more details.
#'
#' Note that the `.estimate` column is likely to be different from the results
#' given by [collect_metrics()] since a different estimator is used. Since
#' random numbers are used in sampling, set the random number seed prior to
#' running this function.
#'
#' The number of bootstrap samples should be large to have reliable intervals.
#' The defaults reflect the fewest samples that should be used.
#'
#' The computations for each configuration can be extensive. To increase
#' computational efficiency parallel processing can be used. The \pkg{foreach}
#' package is used here. To execute the resampling iterations in parallel,
#' register a parallel backend function. See the documentation for
#' [foreach::foreach()] for examples. The `allow_par` argument can be used to
#' avoid parallelism.
#' @seealso [rsample::int_pctl()]
#' @examplesIf !tune:::is_cran_check() & tune:::should_run_examples("modeldata")
#' data(Sacramento, package = "modeldata")
#' library(rsample)
#' library(parsnip)
#'
#' f <- log10(price) ~ beds + baths + sqft + type + latitude + longitude
#'
#' set.seed(13)
#' sac_rs <- vfold_cv(Sacramento)
#'
#' lm_res <-
#'   linear_reg() %>%
#'   fit_resamples(
#'     log10(price) ~ beds + baths + sqft + type + latitude + longitude,
#'     resamples = sac_rs,
#'     control = control_resamples(save_pred = TRUE)
#'   )
#'
#' set.seed(31)
#' int_pctl(lm_res)
#' @export
int_pctl.tune_results <- function(.data, metrics = NULL, times = 1001,
                                  alpha = 0.05, allow_par = TRUE,
                                  event_level = "first", ...) {

  # TODO deal with eval_time
  # TODO add a parameters argument.


  y_nm <- outcome_names(.data)
  param <- .get_tune_parameter_names(.data)
  key_cols <- c(".config", param)
  if (any(names(.data) == ".iter")) {
    key_cols <- c(".config", ".iter", param)
  }
  keys <- collect_metrics(.data) %>% dplyr::distinct(dplyr::pick(dplyr::all_of(key_cols)))
  if (is.null(metrics)) {
    metrics <- .get_tune_metrics(.data)
  }

  res <-
    collect_predictions(.data, summarize = TRUE)%>%
    dplyr::select(-all_of(param)) %>%
    dplyr::group_nest(.config, .key = "results") %>%
    dplyr::mutate(
      .seed = sample.int(10000, dplyr::n()),
      results = purrr::map2(
        results,
        .seed,
        ~ int_comp(.x, .y, times = times, y_nm, metrics, allow_par, event_level)
      )
    ) %>%
    tidyr::unnest(cols = results) %>%
    dplyr::full_join(keys, by = ".config")
  res %>%
    dplyr::select(-.seed) %>%
    dplyr::relocate(!!!key_cols, .after = .upper)
}

comp_metrics <- function(split, y, metrics, event_level) {
  dat <- rsample::analysis(split)
  info <- metrics_info(metrics)
  .estimate_metrics(
    dat,
    metric = metrics,
    param_names = NULL,
    outcome_name = y,
    event_level = event_level,
    metrics_info = info
  ) %>%
    dplyr::select(term = .metric, estimate = .estimate)
}

get_int_p_operator <- function(allow = TRUE) {
  is_par <- foreach::getDoParWorkers() > 1
  if (allow && is_par) {
    res <- foreach::`%dopar%`
  } else {
    res <- foreach::`%do%`
  }
  res
}

int_comp <- function(.data, seed, times = 1000, y_name, metrics,
                     allow_par = TRUE, event_level) {
  `%op%` <- get_int_p_operator(allow_par)
  set.seed(seed)
  rs <- rsample::bootstraps(.data, times = times)

  rs$metrics <-
    foreach::foreach(
      i = 1:nrow(rs),
      .errorhandling = "pass",
      .packages = c("tune", "rsample")
    )  %op% {
      comp_metrics(rs$splits[[i]], y_name, metrics, event_level)
    }
  rsample::int_pctl(rs, metrics) %>%
    dplyr::mutate(.estimator = "bootstrap") %>%
    dplyr::select(.metric = term, .estimator, .lower, .estimate, .upper)
}


