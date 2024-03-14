# ------------------------------------------------------------------------------
# Helpers for parallel processing

# object should be a workflow
allow_parallelism <- function(allow = TRUE, object = NULL) {
  is_par <- foreach::getDoParWorkers() > 1 || future::nbrOfWorkers() > 1
  if (!is.null(object)) {
    pkgs <- required_pkgs(object)
    blacklist <- c("keras", "rJava")
    if (is_par & allow && any(pkgs %in% blacklist)) {
      pkgs <- pkgs[pkgs %in% blacklist]
      msg <- paste0("'", pkgs, "'", collapse = ", ")
      msg <- paste("Some required packages prohibit parallel processing: ", msg)
      cli::cli_alert_warning(msg)
      allow <- FALSE
    }
  }

  allow && is_par
}

get_operator <- function(allow = TRUE, object) {
  if (allow_parallelism(allow, object)) {
    res <- switch(
      # note some backends can return +Inf
      min(future::nbrOfWorkers(), 2),
      list(op = foreach::`%dopar%`, is_future = FALSE),
      list(op = doFuture::`%dofuture%`, is_future = TRUE)
    )
  } else {
    res <- list(op = foreach::`%do%`, is_future = FALSE)
  }

  res
}
