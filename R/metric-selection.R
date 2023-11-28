# For iterative search and racing, what metric will be optimized?
first_metric <- function(mtr_set) {
  tibble::as_tibble(mtr_set)[1,]
}

# Did the user pass an improper metric (i.e. want rmse but not computed)?
check_chosen_metric <- function(metric, mtr_set) {
  mtr_info <- tibble::as_tibble(mtr_set)
  in_set <- any(mtr_info$metric == metric)
  if (!in_set) {
    cli::cli_abort("metric '{metric}' is not in the metric set.")
  }
  invisible(TRUE)
}

# Return the validated evaluation times. May subset to a single value if needed.
#' @export
select_eval_time <- function(mtr_set, eval_time = NULL, single = FALSE) {

  only_stc <- function(x) all(x$class == "static_survival_metric")
  only_dyn <- function(x) all(x$class == "dynamic_survival_metric")
  only_int <- function(x) all(x$class == "integrated_survival_metric")
  has_int  <- function(x) any(x$class == "integrated_survival_metric")
  has_dyn  <- function(x) any(x$class == "dynamic_survival_metric")

  if (single) {
    # This means that we will be using the metric results to rank or
    # optimize something. We need one eval time when the (single) metric
    # is dynamic; null otherwise
    mtr_info <- first_metric(mtr_set)
  } else {
    # In this case, we will use the results for autoplot(), int_pct(), or
    # augment(). We need a valid set of evaluation times which could be none
    # one, or more than one depending on the metric
    mtr_info <- tibble::as_tibble(mtr_set)
  }
  mtr_first <- first_metric(mtr_set)

  if (!any(grepl("_survival_", mtr_info$class))) {
    return(NULL)
  }

  # ------------------------------------------------------------------------------
  # check the size of the eval times

  num_times <- length(eval_time)

  if (only_stc(mtr_info) & num_times != 0) {
    cli::cli_warn("Evaluation times are only required for dynmanic or integrated metrics.")
    eval_time <- NULL
  }

  if (only_dyn(mtr_info) & num_times == 0) {
    cli::cli_abort("A single evaluation time is required; please choose one.")
  }

  # this requires all metrics
  if ( has_int( tibble::as_tibble(mtr_set) ) & num_times < 2 ) {
    cli::cli_abort("2+ evaluation times are required.")
  }

  # checks for cases where only a single eval time should be returned
  if (single) {

    if ( only_dyn(mtr_info) & num_times > 1 ) {
      eval_time <- eval_time[1]
      print_time <- format(eval_time, digits = 3)
      cli::cli_warn("{num_times} evaluation times were selected; the first ({print_time}) will be used.")
    }

    if (only_int(mtr_info)) {
      eval_time <- NULL
    }
  } else {
    # cases where we maybe need evaluation time and return them all

  }


  eval_time
}
