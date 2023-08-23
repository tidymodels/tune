#' Bootstrap 632 estimate of performance
#'
#' This method blends the bootstrap estimate with the resubsitution estimate
#' to potentially reduce bias.
#'
#' @param x An object of class `'tune_results'` that used bootstrap sampling
#' with the option `bootstraps(apparent = TRUE)`.
#' @return A tibble with columns that emulate the structure of
#' `collect_metrics(summarize = TRUE)`. The `std_err` column is set to missing
#' values since the standard error of the 632 estimator is unknown.
#' @examplesIf !tune:::is_cran_check() & tune:::should_run_examples(c("modeldata", "kernlab"))
#' library(modeldata)
#' library(rsample)
#' library(parsnip)
#' library(yardstick)
#'
#' set.seed(1)
#' tr_dat <- sim_regression(50)
#' rs <- bootstraps(tr_dat, times = 20, apparent = TRUE)
#'
#' svm_spec <- svm_rbf(cost = tune()) %>% set_mode("regression")
#'
#' set.seed(2)
#' svm_res <-
#'   svm_spec %>%
#'   tune_grid(
#'     outcome ~ .,
#'     resamples = rs,
#'     grid = 2,
#'     metrics = metric_set(rmse)
#'   )
#'
#' collect_metrics(svm_res)
#' bootstrap_632(svm_res)
#'
#' @export
bootstrap_632 <- function(x) {
  if (!inherits(x, "tune_results")) {
    rlang::abort("The object must have class 'tune_results'.")

  }
  rs_type <- attributes(x)$rset_info$att$class[1]
  if (rs_type != "bootstraps") {
    rlang::abort("The methods is only applicable for bootstrap sampling.")
  }
  if (!any(x$id == "Apparent")) {
    rlang::abort("The option `apparent = TRUE` to `bootstraps()` should be set.")
  }

  mts <- collect_metrics(x, summarize = FALSE)

  resub <-
    dplyr::filter(mts, id == "Apparent") %>%
    dplyr::select(.metric, .config, resub = .estimate)

  bts <-
    dplyr::filter(mts, id != "Apparent") %>%
    dplyr::summarize(
      mean = mean(.estimate, na.rm = TRUE),
      n = sum(!is.na(.estimate)),
      .by = c(.metric, .config))

  prm <- .get_tune_parameter_names(x)
  bys <- c(prm, ".config", ".estimator")
  key <-
    collect_metrics(x, summarize = TRUE) %>%
    dplyr::select(dplyr::all_of(bys)) %>%
    dplyr::distinct()

  cn <- exp(-1)
  res <-
    dplyr::right_join(bts, resub, by = c(".metric", ".config")) %>%
    dplyr::mutate(
      mean = (cn * resub) + ((1 - cn) * mean),
      std_err = NA_real_
    ) %>%
    dplyr::right_join(key, by = ".config")

  nms <- c(prm, ".metric", ".estimator", "mean", "n", "std_err", ".config")
  res[, nms]
}
