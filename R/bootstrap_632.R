
# .no_information_rate() is invoked after .estimate_metrics() in append.R

# Emulates .estimate_metrics() and its output but randomizes the outcome
# multiple times to obtain the "no information rate" for each metric. For
# survival outcomes, both the time and event indicator are randomized together.

# TODO
# - consolidate/refactor code to summary and compute by-groups
# - tighten up filtering across packages to exclude apparent and randomized estimators

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
    met_info <- metrics_info(metric)
    by_vars <- dplyr::group_vars(dat)
    res <-
      purrr::map_dfr(
        1:times,
        ~ .randomized_metric(dat, metric = metric, param_names = character(0),
                             outcome_name = outcome_name, event_level = event_level,
                             metrics_info = met_info, eval_time = eval_time)
      ) %>%
      dplyr::summarize(
        .estimate = median(.estimate, na.rm = TRUE),
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
c_368 <- exp(-1)

check_bootstrap <- function(x) {
  if (attr(x, "rset_info")$att$class != "bootstraps") {
    cli::cli_abort("The data do not appear to be from a bootstrap ressampling scheme.")
  }
  invisible(NULL)
}

check_apparent_present <- function(x) {
  is_resubstitution <- x$id == "Apparent"
  if (!any(is_resubstitution)) {
    cli::cli_abort("The resample does not have resubstitution estimates. \\
                   Did you use {.code bootstraps(<data>, apparent = TRUE)}?")
  }
  invisible(NULL)
}

get_randomized <- function(x) {
  mtr <- collector(x, coll_col = ".metrics", excludes = FALSE)
  is_randomized <- mtr$id != "Apparent" & mtr$.estimator == "randomized"
  randomized <- mtr[is_randomized, c(".metric", ".estimate", ".config", "id")]
  names(randomized)[2] <- ".randomized"
  randomized
}

get_resubstitution <- function(x) {
  mtr <- collector(x, coll_col = ".metrics", excludes = FALSE)
  check_apparent_present(mtr)
  is_resubstitution <- mtr$id == "Apparent" & mtr$.estimator != "randomized"
  keep_cols <- c(".metric", ".estimate", other_metric_cols(mtr))
  resubstitution <- mtr[is_resubstitution, keep_cols]
  names(resubstitution)[2] <- ".resubstitution"
  resubstitution
}

other_metric_cols <- function(x) {
  intersect(names(x), c(".iter", ".config", ".eval_time"))
}

resampling_estimator <- function(x) {
  rs_info <- attr(x, "rset_info")$att
  if (rs_info$class != "bootstraps") {
    return("standard")
  }
  if (any(names(rs_info) == "estimator")) {
    estimator <- rs_info$estimator
  } else {
    estimator <- "standard"
  }
  estimator
}

# ------------------------------------------------------------------------------



bootstrap_632 <- function(x) {
  check_bootstrap(x)
  mtr <- collector(x, coll_col = ".metrics")
  mtr <- mtr[mtr$id != "Apparent",]

  extra_cols <- other_metric_cols(mtr)

  resubstitution <- get_resubstitution(x)

  mtr <- dplyr::inner_join(mtr, resubstitution, by = c(".metric", extra_cols))
  mtr$.estimate <- c_632 * mtr$.estimate + c_368 * mtr$.resubstitution
  mtr$.estimator <- "bootstrap 632"
  mtr$.resubstitution <- NULL

  # summarize
  prm_names <- .get_tune_parameter_names(x)
  by_vars <- c(prm_names, ".estimator", ".metric", extra_cols)

  res <-
    dplyr::group_by(mtr, !!!rlang::syms(by_vars)) %>%
    dplyr::summarize(
      mean = mean(.estimate, na.rm = TRUE),
      n = sum(!is.na(.estimate)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(std_err = NA_real_) %>%
    dplyr::select(!!prm_names, .metric, dplyr::any_of(".eval_time"), .estimator,
                  mean, n, std_err, .config, dplyr::any_of(".iter"))

  res[order(res$.config, res$.metric),,drop = FALSE]
}


# later, mtr will come out in collect_metrics

bootstrap_632_plus <- function(x)  {
  check_bootstrap(x)
  mtr <- collector(x, coll_col = ".metrics", excludes = FALSE)
  mtr <- mtr[mtr$id != "Apparent",]

  extra_cols <- other_metric_cols(mtr)
  prm_names <- .get_tune_parameter_names(x)

  ind_estimates <- mtr[mtr$id != "Apparent" & mtr$.estimator != "randomized",]
  resubstitution <- get_resubstitution(x)
  randomized <- get_randomized(x)

  mtr <-
    dplyr::inner_join(ind_estimates, resubstitution, by = c(".metric", extra_cols)) %>%
    dplyr::full_join(randomized, by = c(".metric", ".config", "id")) %>%
    dplyr::mutate(
      ror = (.estimate - .resubstitution) / (.randomized - .resubstitution),
      ror = ifelse(ror < 0, 0, ror),
      wt = c_632 / (1 - c_368 * ror),
      .estimate = wt * .estimate + (1 - wt) * .resubstitution,
      .estimator = "bootstrap 632+"
    ) %>%
    dplyr::select(-ror, -wt, -.resubstitution, -.randomized)

  # summarize
  extras <- intersect(names(mtr), c(".iter", ".eval_time"))
  by_vars <- c(prm_names, ".config", ".estimator", ".metric", extras)

  res <-
    dplyr::group_by(mtr, !!!rlang::syms(by_vars)) %>%
    dplyr::summarize(
      mean = mean(.estimate, na.rm = TRUE),
      n = sum(!is.na(.estimate)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(std_err = NA_real_) %>%
    dplyr::select(!!prm_names, .metric, dplyr::any_of(".eval_time"), .estimator,
                  mean, n, std_err, .config, dplyr::any_of(".iter"))

  res[order(res$.config, res$.metric),,drop = FALSE]
}

