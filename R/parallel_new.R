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
  rlang::is_installed("future")
}
mirai_avail <- function() {
  rlang::is_installed("mirai")
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
      cli::cli_inform("Neither {.pkg mirai} or {.pkg future} are installed")
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
#'
#' \pkg{tune} can enable the simultaneous parallel computations. Tierney (2008)
#'  defined different classes of parallel processing techniques:
#'
#'  - _Implicit_ is when a function uses low-level tools to perform a
#'  calculation that is small in scope in parallel. Examples are using
#' multithreaded linear algebra libraries (e.g., BLAS) or basic R vectorization
#' functions.
#'  - _Explicit_ parallelization occurs when the user requests that some
#' calculations should be run by generating multiple new R (sub)processes. These
#' calculations can be more complex than those for implicit parallel
#'  processing.
#'
#' For example, some decision tree libraries can implicitly parallelize their
#' search for the optimal splitting routine using multiple threads.
#'
#' Alternatively, if you are resampling a model _B_ times, you can explicitly
#' create _B_ new R jobs to train _B_ boosted trees in parallel and return their
#' resampling results to the main R process (e.g., [fit_resamples()]).
#'
#' There are two frameworks that can be used to explicitly parallel process
#' your work in \pkg{tune}: the [future][future::future] package and the
#' [mirai][mirai:: mirai] package. Previously, you could use the
#' [foreach][foreach::foreach] package, but this has been deprecated as of
#' version 1.2.1 of tune.
#'
#' By default, no parallelism is used to process models in \pkg{tune}; you have
#' to opt-in.
#'
#' ## Using future
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
#' If you want \pkg{future} to use \pkg{mirai} parallel workers, you can
#' install and load the \pkg{future.mirai} package.
#'
#' ## Using mirai
#'
#' To set the specific for parallel processing with \pkg{mirai}, the
#' [mirai::daemons()] functions. The first argument, `n`, determines the number
#' of parallel workers. Using `daemons(0)` reverts to sequential processing.
#'
#' The arguments `url` and `remote` are used to set up and launch parallel
#' processes over the network for distributed computing. See [mirai::daemons()]
#' documentation for more details.
#'
#' ## Exceptions
#'
#' There are a few times when you might specify that you wish to use parallel
#' processing, but it will revert to sequential execution.
#' - Many of the control functions (e.g. [control_grid()]) have an argument
#' called `allow_par`. If this is set to `FALSE`, parallel backends will always
#' be ignored.
#' - Some packages, such as \pkg{rJava} and \pkg{keras} are not compatible with
#' explicit parallelization. If any of these packages are used, sequential
#' processing occurs.
#' - If you specify fewer than two workers, or if there is only a single task,
#'  the computations will occur sequentially.
#'
#' @references
#' https://www.tmwr.org/grid-search#parallel-processing
#'
#' Tierney, Luke. "Implicit and explicit parallel computing in R." COMPSTAT
#' 2008: Proceedings in Computational Statistics. Physica-Verlag HD, 2008.
#' @name parallelism
NULL
