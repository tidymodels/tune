# This are new changes for parallel process via future _or_ mirai. These
# functions should be able to replace what is contained in `parallel.R`

# ------------------------------------------------------------------------------

has_non_par_pkgs <- function(object, control, verbose = FALSE) {
  pkgs <- character(0)

  if (!is.null(object)) {
    pkgs <- required_pkgs(object)
  }
  if (!is.null(control)) {
    pkgs <- c(pkgs, control$pkgs)
  }
  pkgs <- unique(pkgs)
  if (length(pkgs) == 0) {
    return(FALSE)
  }
  naughty_list <- c("keras", "rJava")
  has_pkg <- pkgs %in% naughty_list
  if (any(has_pkg)) {
    pkgs <- pkgs[has_pkg]
    if (verbose) {
      cli::cli_inform(
        "These packages cannot be used with explicit parallel processing: {.pkg {pkgs}}."
      )
    }
  }
  any(has_pkg)
}

# ------------------------------------------------------------------------------

future_avail <- function() {
  any(search() == "package:future")
}
mirai_avail <- function() {
  any(search() == "package:mirai")
}

get_future_workers <- function(verbose) {
  has_future <- future_avail()
  if (has_future) {
    future_workers <- future::nbrOfWorkers()
    if (verbose) {
      cli::cli_inform(
        "{.pkg future} is loaded with {future_workers} worker{?s}"
      )
    }
  } else {
    if (verbose) {
      cli::cli_inform("{.pkg future} is not loaded")
    }

    future_workers <- 0L
  }
  future_workers
}

get_mirai_workers <- function(verbose) {
  has_mirai <- mirai_avail()
  if (has_mirai) {
    if (mirai::daemons_set()) {
      mirai_workers <- mirai::status()$connections
      if (verbose) {
        cli::cli_inform(
          "{.pkg mirai} is loaded with {mirai_workers} worker{?s}"
        )
      }
    } else {
      mirai_workers <- 0L
      if (verbose) {
        cli::cli_inform(
          "{.pkg mirai} is loaded with {mirai_workers} worker{?s}"
        )
      }
    }
  } else {
    if (verbose) {
      cli::cli_inform("{.pkg mirai} is not loaded")
    }
    mirai_workers <- 0L
  }
  mirai_workers
}

choose_framework <- function(
  object = NULL,
  control = NULL,
  verbose = FALSE,
  default = "mirai"
) {
  if (!is.null(control)) {
    if (!control$allow_par) {
      return("sequential")
    }
  }

  if (has_non_par_pkgs(object, control, verbose)) {
    return("sequential")
  }

  has_future <- future_avail()
  has_mirai <- mirai_avail()

  if (!has_future & !has_mirai) {
    if (verbose) {
      cli::cli_inform("Neither {.pkg mirai} or {.pkg future} are loaded")
    }
    return("sequential")
  }

  mirai_workers <- get_mirai_workers(verbose)
  future_workers <- get_future_workers(verbose)

  neither <- future_workers < 2 & mirai_workers < 2
  both <- future_workers >= 2 & mirai_workers >= 2

  if (neither) {
    res <- "sequential"
  } else if (both) {
    if (verbose) {
      cli::cli_inform(
        "Multiple workers exist for both {.pkg mirai} and {.pkg future}; 
        falling back to the default of {.pkg {default}}."
      )
    }
    res <- default
  } else {
    if (future_workers >= 2) {
      res <- "future"
    } else {
      res <- "mirai"
    }
  }
  res
}

# ------------------------------------------------------------------------------

#' Support for parallel processing in tune
#'
#' @description
#' There are two frameworks that can be used to parallel process your work: the
#' [future][future::future] package and the [mirai][mirai:: mirai] package.
#' Previously, you could use the [foreach][foreach::foreach] package, but this
#' has been deprecated as of version 1.2.1 of tune.
#'
#' By default, no parallelism is used to process models in \pkg{tune}; you have
#' to opt-in.
#'
#' ## Using futures
#'
#' You should install the package and choose your flavor of parallelism using
#' the [plan][future::plan] function. This allows you to specify the number of
#' worker processes and the specific technology to use.
#'
#' For example, you can use:
#'
#' ```r
#'    library(future)
#'    plan(multisession, workers = 4)
#' ```
#' and work will be conducted simultaneously (unless there is an exception; see
#' the section below).
#'
#' If you had previously used \pkg{foreach}, this would replace your existing
#' code that probably looked like:
#'
#' ```r
#'    library(doBackend)
#'    registerDoBackend(cores = 4)
#' ```
#'
#' See [future::plan()] for possible options other than `multisession`.
#'
#' Note that \pkg{tune} resets the _maximum_ limit of memory of global variables
#' (e.g., attached packages) to be greater than the default when the package is
#' loaded. This value can be altered using `options(future.globals.maxSize)`.
#'
#' ## Using mirai
#'
#' To set the specific for parallel processing with \pkg{mirai}, the
#' [mirai::daemons()] functions. The first argument, `n`, determines the number
#' of parallel workers. Using `daemons(0)` reverts to sequential processing.
#'
#' If you want to use \pkg{future} and \pkg{mirai} together, you can install
#' and load the \pkg{future.mirai} package.
#'
#' ## Exceptions
#'
#' There are a few times when you might specify that you wish to use parallel
#' processing, but it will revert to sequential execution.
#' - Many of the control functions (e.g. [control_grid()]) have an argument
#' called `allow_par`. If this is set to `FALSE`, parallel backends will always
#' be ignored.
#' - Some packages, such as \pkg{rJava} and \pkg{keras} are not compatable with
#' explict parallelization. If any of these packages are used, sequential
#' processing occurs.
#' - If you specify fewer than two workers, the computations will occur
#' sequentially.
#'
#' @references https://www.tmwr.org/grid-search#parallel-processing
#' @name parallelism
NULL
