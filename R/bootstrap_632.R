
# Emulates .estimate_metrics() and its output but randomizes the outcome
# multiple times to obtain the "no information rate" for each metric. For
# survival outcomes, both the time and event indicator are randomized together.


# This is designed to work on a single tuning parameter candidate/resample etc.

.randomized_metric <- function(dat, metric, param_names, outcome_name,
                              event_level, metrics_info, eval_time) {
  for (col in outcome_name) {
    dat[[col]] <- sample(dat[[col]])
  }
  .estimate_metrics(dat, metric, character(0), outcome_name,
                    event_level, metrics_info, eval_time)
}

.nir_single <-
  function(dat, metric, outcome_name, event_level, eval_time = NULL, times = 50) {
    met_info <- tune:::metrics_info(metric)
    by_vars <- dplyr::group_vars(dat)
    res <-
      purrr::map_dfr(
        1:times,
        ~ .randomized_metric(dat, metric = metric, param_names = character(0),
                             outcome_name = outcome_name, event_level = event_level,
                             metrics_info = met_info, eval_time = eval_time)
      ) %>%
      dplyr::summarize(
        .estimate = mean(.estimate, na.rm = TRUE),
        .by = c(.metric, dplyr::all_of(by_vars))
      )
    res
  }

# Now process over resamples, tuning parameters, and any other previously
# grouped-on columns
.no_information_rate <-
  function(dat, metric, param_names, outcome_name, event_level,
           metrics_info = metrics_info(metrics), eval_time = NULL, times = 25) {

    id_vars <- grep("^id", names(dat), value = TRUE)
    by_vars <- c(param_names, dplyr::group_vars(dat), id_vars)

    group_rows <- dplyr::group_rows(dat)
    group_keys <- dplyr::group_keys(dat)

    if (any(names(dat) == ".config") & length(param_names) > 0) {
      by_vars <- c(by_vars, ".config")
    }

    dat <- dplyr::group_by(dat, !!!rlang::syms(by_vars), .add = TRUE)

    res <- .nir_single(dat, metric = metric, outcome_name = outcome_name,
                       event_level = event_level, eval_time = eval_time,
                       times = times)
    res$.estimator <- "randomized"
    res <- dplyr::select(res, !!!by_vars, .metric, .estimator, .estimate)
    res <- dplyr::relocate(res, dplyr::any_of(".config"), .after = dplyr::last_col())
    res
  }

all_rates <- function(x) {


}


bootstrap_632 <- function(x) {

}
