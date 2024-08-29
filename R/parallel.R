# ------------------------------------------------------------------------------
# Helpers for parallel processing

# object should be a workflow
allow_parallelism <- function(allow = TRUE, object = NULL) {
  is_par <- future::nbrOfWorkers() > 1
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

warn_foreach_deprecation <- function() {
  cli::cli_warn(c(
    "!" = "{.pkg tune} detected a parallel backend registered with \\
               foreach but no backend registered with future.",
    "i" = "Support for parallel processing with foreach was \\
               soft-deprecated in {.pkg tune} 1.2.1.",
    "i" = "See {.help [?parallelism](tune::parallelism)} to learn more."
  ))
}

#' Support for parallel processing in tune
#'
#' @description
#' Support for parallel backends registered with the [foreach][foreach::foreach]
#' package was deprecated in tune 1.2.1 in favor of the
#' [future][future::future] package. The package will now raise a warning when:
#'
#' 1) A parallel backend has been registered with foreach, and
#' 2) No [plan][future::plan] has been specified with future.
#'
#' If parallelism has been configured with both framework, tune will use the
#' plan specified with future and will not warn. To transition your code from
#' foreach to future, remove code that registers a foreach `Backend`:
#'
#' ```r
#' library(doBackend)
#' registerDoBackend(cores = 4)
#' ```
#'
#' And replace it with:
#'
#' ```r
#' library(future)
#' plan(multisession, workers = 4)
#' ```
#'
#' See [future::plan()] for possible options other than `multisession`.
#'
#' @name parallelism
NULL
