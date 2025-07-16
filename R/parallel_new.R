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

future_installed <- function() {
  rlang::is_installed("future")
}
mirai_installed <- function() {
  rlang::is_installed("mirai")
}

get_future_workers <- function(verbose) {
  has_future <- future_installed()

  if (has_future) {
    future_workers <- future::nbrOfWorkers()
    if (verbose) {
      if (future_workers == 0) {
        cli::cli_inform(
          "{.pkg future} is not active."
        )
      } else {
        cli::cli_inform(
          "{.pkg future} is active with {future_workers} worker{?s}."
        )
      }
    }
  } else {
    if (verbose) {
      cli::cli_inform("{.pkg future} is not installed.")
    }

    future_workers <- 0L
  }
  future_workers
}

get_mirai_workers <- function(verbose) {
  if (!mirai_installed()) {
    if (verbose) {
      cli::cli_inform("{.pkg mirai} is not installed.")
    }
    return(0L)
  }

  # note connections will be 0 if `!daemons_set()`
  mirai_workers <- mirai::status()$connections

  if (verbose) {
    if (mirai_workers == 0) {
      cli::cli_inform(
        "{.pkg mirai} is not active."
      )
    } else {
      cli::cli_inform(
        "{.pkg mirai} is active with {mirai_workers} worker{?s}."
      )
    }
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

  has_future <- future_installed()
  has_mirai <- mirai_installed()

  if (!has_future & !has_mirai) {
    if (verbose) {
      cli::cli_inform("Neither {.pkg mirai} or {.pkg future} are installed.")
    }
    return("sequential")
  }

  mirai_workers <- get_mirai_workers(verbose)
  future_workers <- get_future_workers(verbose)

  neither <- future_workers < 2 & mirai_workers < 2
  both <- future_workers >= 2 & mirai_workers >= 2

  if (neither) {
    if (verbose) {
      cli::cli_inform("Too few workers for parallel processing.")
    }
    return("sequential")
  }

  if (both) {
    if (verbose) {
      cli::cli_inform(
        "Multiple workers exist for both {.pkg mirai} and {.pkg future};
        falling back to the default of {.pkg {default}}."
      )
    }
    return(default)
  }

  if (future_workers >= 2) {
    res <- "future"
  } else {
    res <- "mirai"
  }

  if (verbose) {
    cli::cli_inform("{.pkg {res}} will be used for parallel processing}.")
  }

  res
}

get_parallel_seeds <- function(workers) {
  # Get current rng info and save
  orig_state <- .Random.seed
  orig_kind <- RNGkind()[1]
  # Reset the stream to get new rng's
  on.exit({
    RNGkind(orig_kind)
    assign(".Random.seed", orig_state, globalenv())
  })

  # Set to type used for multiple streams
  RNGkind("L'Ecuyer-CMRG")

  # Capture the seed to make more seeds.
  .seed <- .Random.seed

  res <- vector(mode = "list", length = workers)
  for (i in seq_along(res)) {
    res[[i]] <- parallel::nextRNGSubStream(.seed)
    .seed <- parallel::nextRNGStream(.seed)
  }

  res
}

# ------------------------------------------------------------------------------

#' Support for parallel processing in tune
#'
#' @description
#'
#' \pkg{tune} can enable simultaneous parallel computations. Tierney (2008)
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
#' To set the specific for parallel processing with \pkg{mirai}, use the
#' [mirai::daemons()] function. The first argument, `n`, determines the number
#' of parallel workers. Using `daemons(0)` reverts to sequential processing.
#'
#' The arguments `url` and `remote` are used to set up and launch parallel
#' processes over the network for distributed computing. See [mirai::daemons()]
#' documentation for more details.
#'
#' ## Reverting to sequential processing
#'
#' There are a few times when you might specify that you wish to use parallel
#' processing, but it will revert to sequential execution:
#'
#' - Many of the control functions (e.g. [control_grid()]) have an argument
#'   called `allow_par`. If this is set to `FALSE`, parallel backends will
#'   always be ignored.
#' - Some packages, such as \pkg{rJava} and \pkg{keras} are not compatible with
#'   explicit parallelization. If any of these packages are used, sequential
#'   processing occurs.
#' - If you specify fewer than two workers, or if there is only a single task,
#'   the computations will occur sequentially.
#'
#' ## Expectations for reproducibility
#'
#' We advise that you _always_ run [set.seed()] with a seed value just prior to
#' using a function that uses (or might use) random numbers. Given this:
#'
#' - You should expect to get the same results if you run that section of code
#'   repeatedly, conditional on using version 1.4.0 of tune.
#' - You should expect differences in results between version 1.4.0 of tune and
#'   previous versions.
#' - When using [last_fit()], you should be able to get the same results as
#'   manually using [generics::fit()] and [stats::predict()] to do the same work.
#' - When running with or without parallel processing (using any backend
#'   package), you should be able to achieve the same results from
#'   [fit_resamples()] and the various tuning functions.
#'
#' Specific exceptions:
#'
#' - For SVM classification models using the \pkg{kernlab} package, the random
#'   number generator is independent of R, and there is no argument to control
#'   it. Unfortunately, it is likely to give you different results from
#'   run-to-run.
#' - For some deep learning packages (e.g., \pkg{tensorflow}, \pkg{keras}, and
#'   \pkg{torch}), it is very difficult to achieve reproducible results. This
#'   is especially true when using GPUs for computations. Additionally, we have
#'   seen differences in computations (stochastic or non-random) between
#'   platforms due to the packages' use of different numerical tolerance
#'   constants across operating systems.
#'
#' @references
#' \url{https://www.tmwr.org/grid-search#parallel-processing}
#'
#' Tierney, Luke. "Implicit and explicit parallel computing in R." COMPSTAT
#' 2008: Proceedings in Computational Statistics. Physica-Verlag HD, 2008.
#' @name parallelism
NULL

# ------------------------------------------------------------------------------
# Choosing how to execute the looping structure

update_parallel_over <- function(control, resamples, grid) {
  num_candidates <- nrow(grid)

  if (is.null(control$parallel_over) | num_candidates == 0) {
    control$parallel_over <- "resamples"
  }
  if (length(resamples$splits) == 1 & num_candidates > 0) {
    control$parallel_over <- "everything"
  }
  control
}

# mirai_map() acts a little different form map(), lapply(), etc. It requires
# that the elements in .args be the args (not symbols). It also requires an
# extra step to collect the results and coerce them into a list.
eval_mirai <- function(.x, .f, ..., .args) {
  .args <- lapply(.args, get, envir = parent.frame())
  res <- mirai::mirai_map(.x, .f, ..., .args = .args)
  mirai::collect_mirai(res)
}

fns <- list(
  sequential = list(fn = "lapply", ns = NULL),
  future = list(fn = "future_lapply", ns = "future.apply"),
  mirai = list(fn = "eval_mirai", ns = NULL)
)

loop_call <-
  function(strategy, framework, opts) {
    if (strategy == "resamples") {
      base_cl <- rlang::call2(
        fns[[framework]][[1]],
        .ns = fns[[framework]][[2]],
        quote(resamples),
        quote(loop_over_all_stages)
      )
      base_args <- list(grid = quote(grid), static = quote(static))
    } else {
      base_cl <- rlang::call2(
        fns[[framework]][[1]],
        .ns = fns[[framework]][[2]],
        quote(inds),
        quote(loop_over_all_stages2)
      )
      base_args <- list(
        resamples = quote(resamples),
        grid = quote(candidates),
        static = quote(static)
      )
    }

    # Configure arguments
    base_args <- c(base_args, opts)

    if (framework == "future") {
      rlang::check_installed("future")

      future_opts <- list(
        future.label = "tune-grid-%d",
        future.stdout = TRUE,
        future.seed = NULL
      )
      base_args <- c(base_args, future_opts)
    }

    if (framework == "mirai") {
      rlang::check_installed("mirai")
      cl <- rlang::call_modify(base_cl, .args = base_args)
    } else {
      cl <- rlang::call_modify(base_cl, !!!base_args)
    }
    cl
  }

# ------------------------------------------------------------------------------
# for int_pctl

pctl_call <- function(framework, args = list()) {
  if (framework == "future") {
    rlang::check_installed("future")

    future_opts <- list(
      future.label = "int-pctl-%d",
      future.stdout = TRUE,
      future.seed = NULL
    )
    args <- c(args, future_opts)
  }

  main_args <- list(
    y = quote(y_nm),
    metrics = quote(metrics),
    event_level = quote(event_level),
    metrics_info = quote(metrics_info),
    param_names = quote(param_names),
    configs = quote(config)
  )
  args <- c(main_args, args)

  base_cl <- rlang::call2(
    fns[[framework]][[1]],
    .ns = fns[[framework]][[2]],
    quote(rs$splits),
    quote(boot_metrics)
  )
  if (framework == "mirai") {
    rlang::check_installed("mirai")
    cl <- rlang::call_modify(base_cl, .args = args)
  } else {
    cl <- rlang::call_modify(base_cl, !!!args)
  }

  cl
}
