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

    if (!res[["is_future"]]) {
      warn_foreach_deprecation()
    }
  } else {
    res <- list(op = foreach::`%do%`, is_future = FALSE)
  }

  res
}

warn_foreach_deprecation <- function() {
  cli::cli_warn(c(
    "!" = "{.pkg tune} detected a parallel backend registered with \\
               foreach but no backend registered with future.",
    "i" = "Support for parallel processing with foreach was \\
               soft-deprecated in {.pkg tune} 1.2.1.",
    "i" = "See {.help [?parallelism](tune::parallelism)} to learn more."
  ))
}

manange_global_limit <- function(min = 1e9) {
  currrent_value <- getOption("future.globals.maxSize")
  if (is.null(currrent_value)) {
    options(future.globals.maxSize = min)
  } else {
    if (currrent_value < min) {
      options(future.globals.maxSize = min)
    }
  }
  invisible(NULL)
}
