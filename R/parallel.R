# ------------------------------------------------------------------------------
# Helpers for parallel processing

# object should be a workflow
get_operator <- function(allow = TRUE, object, resamples, grid) {
  is_par <- foreach::getDoParWorkers() > 1
  pkgs <- required_pkgs(object)
  blacklist <- c("keras", "rJava")
  if (is_par & allow && any(pkgs %in% blacklist)) {
    pkgs <- pkgs[pkgs %in% blacklist]
    msg <- paste0("'", pkgs, "'", collapse = ", ")
    msg <- paste("Some required packages prohibit parallel processing: ", msg)
    cli::cli_alert_warning(msg)
    allow <- FALSE
  }

  if (nrow(resamples) == 1 & single_model(grid)) {
    single_task <- TRUE
  } else {
    single_task <- FALSE
  }

  cond <- allow && is_par && !single_task
  if (cond) {
    res <- foreach::`%dopar%`
  } else {
    res <- foreach::`%do%`
  }
  res
}

parallel_scheme <- function(ctrl, resamples) {
  res <- ctrl$parallel_over
  is_par <- foreach::getDoParWorkers() > 1
  if (nrow(resamples) == 1 & is_par) {
    if (res == "resamples") {
      res <- "everything"
      rlang::inform(
        paste("Since there is a single resample, the `parallel_over`'",
              "option was changed to 'everything'.")
        )
    }
  }
  res
}


single_model <- function(grid) {
  # In case of last fit
  res <- is.null(grid) | NROW(grid) <= 1
  if (res & any(names(grid) == ".submodels")) {
    res <- res & length(unlist(grid$.submodels)) == 0
  }
  res
}


