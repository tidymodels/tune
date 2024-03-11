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
      cli::cli_warn(c(
        "!" = "{.pkg tune} detected a parallel backend registered with \\
               foreach but no backend registered with future.",
        "i" = "Support for foreach was soft-deprecated in {.pkg tune} 1.2.0.",
        "i" = "See {.help [?parallelism](tune::parallelism)} to learn more."
      ))
    }
  } else {
    res <- list(op = foreach::`%do%`, is_future = FALSE)
  }

  res
}

#' Support for parallel processing in tune
#'
#' @description
#' Support for parallel backends registered with the [foreach][foreach::foreach]
#' package was deprecated in tune 1.2.0 in favor of the
#' [future][future::future] package. tune 1.2.0 was released in March 2024; for
#' the two years following, the package will raise a warning when:
#'
#' 1) A parallel backend has been registered with foreach, and
#' 2) No [plan][future::plan] has been specified with future.
#'
#' If parallelism has been configured with both framework, tune will use the
#' plan specified with future and will not warn. To learn more about plans in
#' future, see [future::plan()].
#'
#' @name parallelism
NULL
