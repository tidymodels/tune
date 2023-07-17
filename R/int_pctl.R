#' Bootstrap confidence intervals for performance metrics
#'
#' Using out-of-sample predictions, the bootstrap is used to create percentile
#' confidence intervals.
#' @inheritParams collect_predictions
#' @inheritParams rlang::args_dots_empty
#' @inheritParams rsample::int_pctl
#' @inheritParams rsample::bootstraps
#' @param .data A object with class `tune_results` where the `save_pred = TRUE`
#' option was used in the control function.
#' @param metrics A [yardstick::metric_set()]. By default, it uses the same
#' metrics as the original object.
#' @param allow_par A logical to allow parallel processing (if a parallel
#' backend is registered).
#' @param event_level A single string. Either `"first"` or `"second"` to specify
#' which level of truth to consider as the "event".
#' @param eval_time A vector of evaluation times for censored regression models.
#' `NULL` is appropriate otherwise. If `NULL` is used with censored models, a
#' evaluation time is selected, and a warning is issued.
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
#'
#' Also, if a censored regression model used numerous evaluation times, the
#' computations can take a long time unless the times are filtered with the
#' `eval_time` argument.
#' @seealso [rsample::int_pctl()]
#' @references Davison, A., & Hinkley, D. (1997). _Bootstrap Methods and their
#'  Application_. Cambridge: Cambridge University Press.
#'  doi:10.1017/CBO9780511802843
#' @examplesIf !tune:::is_cran_check() & tune:::should_run_examples("modeldata")
#' data(Sacramento, package = "modeldata")
#' library(rsample)
#' library(parsnip)
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
                                  eval_time = NULL, parameters = NULL,
                                  alpha = 0.05, allow_par = TRUE,
                                  event_level = "first", ...) {

  rlang::check_dots_empty()

  # check eval_time and set default when null
  eval_time <- default_eval_time(eval_time, .data$.metrics[[1]])
  .data$.predictions <- filter_eval_times(.data, eval_time)

  y_nm <- outcome_names(.data)

  config_keys <- get_configs(.data, parameters = parameters)
  p <- length(config_keys)

  if (is.null(metrics)) {
    metrics <- .get_tune_metrics(.data)
  }

  res <-
    purrr::map2(
      config_keys, sample.int(10000, p),
      ~ compute_by_config(.x, .y, .data, metrics, times, allow_par, event_level)
    ) %>%
    purrr::list_rbind() %>%
    dplyr::arrange(.config, .metric)
  dplyr::as_tibble(res)
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

compute_by_config <- function(config, seed, x, metrics, times, allow_par, event_level) {
  y_nm <- outcome_names(x)
  preds <- collect_predictions(x, summarize = TRUE, parameters = config)

  set.seed(seed)
  rs <- rsample::bootstraps(preds, times = times)

  `%op%` <- get_int_p_operator(allow_par)
  rs$.metrics <-
    foreach::foreach(
      i = 1:nrow(rs),
      .errorhandling = "pass",
      .packages = c("tune", "rsample")
    )  %op% {
      comp_metrics(rs$splits[[i]], y_nm, metrics, event_level)
    }

  if (any(grepl("survival", .get_tune_metric_names(x)))) {
    # compute by evaluation time
    res <- int_pctl_dyn_surv(rs, allow_par)
  } else {
    res <- rsample::int_pctl(rs, .metrics)
  }
  res %>%
    dplyr::mutate(.estimator = "bootstrap") %>%
    dplyr::rename(.metric = term) %>%
    dplyr::relocate(.estimator, .after = .metric) %>%
    dplyr::select(-.alpha, -.method) %>%
    cbind(config)
}

# We have to do the analysis separately for each evaluation time.
int_pctl_dyn_surv <- function(x, allow_par) {
  `%op%` <- get_int_p_operator(allow_par)
  times <- unique(x$.metrics[[1]]$.eval_time)
  res <-
    foreach::foreach(
      i = seq_along(times),
      .errorhandling = "pass",
      .packages = c("purrr", "rsample", "dplyr")
    )  %op% {
      by_eval_time(times[i], x)
    }
  dplyr::bind_rows(res)
}

by_eval_time <- function(time, x) {
  times <- dplyr::tibble(.eval_time = time)
  x$.metrics <- purrr::map(x$.metrics, ~ dplyr::inner_join(.x, times, by = ".eval_time"))
  rsample::int_pctl(x, .metrics)
}


# ------------------------------------------------------------------------------

get_configs <- function(x, parameters = NULL, as_list = TRUE) {
  param <- .get_tune_parameter_names(x)
  config_cols <- c(".config", ".iter", param)
  config_keys <-
    collect_metrics(x, summarize = FALSE) %>%
    dplyr::distinct(dplyr::pick(dplyr::any_of(config_cols)))
  if (!is.null(parameters)) {
    merge_cols <- intersect(names(config_keys), names(parameters))
    config_keys <- dplyr::inner_join(config_keys, parameters, by = merge_cols)
  }
  if (as_list) {
    config_keys <- vctrs::vec_chop(config_keys, as.list(1:nrow(config_keys)))
  }
  config_keys
}

# Compute metrics for a specific configuration
comp_metrics <- function(split, y, metrics, event_level) {
  dat <- rsample::analysis(split)
  info <- metrics_info(metrics)

  res <-
    .estimate_metrics(
      dat,
      metric = metrics,
      param_names = NULL,
      outcome_name = y,
      event_level = event_level,
      metrics_info = info,
      eval_time = NA  # TODO I don't think that this is used in the function
    )

  res %>%
    dplyr::rename(term = .metric, estimate = .estimate) %>%
    dplyr::select(-.estimator)
}

# ------------------------------------------------------------------------------


filter_eval_times <- function(x, eval_time = NULL) {
  if (is.null(eval_time)) {
    return(x$.predictions)
  }
  purrr::map(x$.predictions, thin_time, times = eval_time)
}

thin_time <- function(x, times) {
  x$.pred <- purrr::map(x$.pred, subset_time, times = times)
  x
}

subset_time <- function(x, times) {
  x[x$.eval_time %in% times,]
}


