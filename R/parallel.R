# ------------------------------------------------------------------------------
# Helpers for parallel processing

get_operator <- function(allow = TRUE, object) {
  is_par <- foreach::getDoParWorkers() > 1
  pkgs <- mod_pkgs(object$fit$model$model)
  blacklist <- c("keras", "rJava")
  if (is_par & allow && any(pkgs %in% blacklist)) {
    pkgs <- pkgs[pkgs %in% blacklist]
    msg <- paste0("'", pkgs, "'", collapse = ", ")
    msg <- paste("Some required packages prohibit parallel processing: ", msg)
    cli::cli_alert_warning(msg)
    allow <- FALSE
  }

  cond <- allow && is_par
  if (cond) {
    res <- foreach::`%dopar%`
  } else {
    res <- foreach::`%do%`
  }
  res
}

