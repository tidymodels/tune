
# .no_information_rate() is invoked after .estimate_metrics() in append.R

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
  function(dat, metric, outcome_name, event_level, eval_time = NULL, times = 20) {
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
           metrics_info = metrics_info(metrics), eval_time = NULL, times = 20) {

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

# ------------------------------------------------------------------------------

# These are invoked inside of collect_metric's summarize function

c_632 <- 1 - exp(-1)
c_368 <- 1 - c_632

bootstrap_632 <- function(x) {
  if (attr(x, "rset_info")$att$class != "bootstraps") {

  }
  mtr <- collect_metrics(x, summarize = FALSE)
  is_resub <- mtr$id == "Apparent"
  if (!any(is_resub)) {

  }
  prm_names <- .get_tune_parameter_names(x)

  resub <- mtr[is_resub, c(".metric", ".estimate", ".config")]
  names(resub)[2] <- ".resub"

  y <- dplyr::inner_join(mtr, resub, by = c(".metric", ".config"))
  y <- y[y$id != "Apparent",]
  y$.estimate <- c_632 * y$.estimate + c_368 * y$.resub
  y$.estimator <- "632 rule"
  y$.resub <- NULL

  # summarize
  extras <- intersect(names(mtr), c(".iter", ".eval_time"))
  by_vars <- c(prm_names, ".config", ".estimator", ".metric", extras)

  res <-
    dplyr::group_by(y, !!!rlang::syms(by_vars)) %>%
    dplyr::summarize(
      mean = mean(.estimate, na.rm = TRUE),
      n = sum(!is.na(.estimate)),
      std_err = sd(.estimate, na.rm = TRUE) / sqrt(n),
      .groups = "drop"
    ) %>%
    dplyr::select(!!prm_names, .metric, .estimator, mean, n, std_err, .config)

  res[order(res$.config, res$.metric),,drop = FALSE]
}

bootstrap_632_plus <- function(x)  {

}

