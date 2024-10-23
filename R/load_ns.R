#' Quietly load package namespace
#'
#' For one or more packages, load the namespace. This is used during parallel
#' processing since the different parallel backends handle the package
#' environments differently.
#' @param x A character vector of packages.
#' @param infra Should base tidymodels packages be loaded as well?
#' @return An invisible NULL.
#' @keywords internal
#' @export
load_pkgs <- function(x, ..., infra = TRUE) {
  UseMethod("load_pkgs")
}

#' @export
load_pkgs.character <- function(x, ...) {
  rlang::check_dots_empty()
  withr::with_preserve_seed(.load_namespace(x))
}

#' @export
load_pkgs.model_spec <- function(x, ..., infra = TRUE) {
  rlang::check_dots_empty()
  pkgs <- required_pkgs(x)
  if (infra) {
    pkgs <- c(infra_pkgs, pkgs)
  }
  .load_namespace(unique(pkgs))
}

#' @export
load_pkgs.workflow <- function(x, ..., infra = TRUE) {
  rlang::check_dots_empty()
  load_pkgs.model_spec(extract_spec_parsnip(x), infra = infra)
}

full_load <- c("kknn", "earth")

#' @export
#' @rdname tune-internal-functions
.load_namespace <- function(x) {
  if (length(x) == 0) {
    return(invisible(TRUE))
  }

  x_full <- x[x %in% full_load]
  x <- x[!(x %in% full_load)]

  loaded <- purrr::map_lgl(x, isNamespaceLoaded)
  x <- x[!loaded]

  if (length(x) > 0) {
    did_load <- purrr::map_lgl(x, requireNamespace, quietly = TRUE)
    if (any(!did_load)) {
      bad <- x[!did_load]
      cli::cli_abort("The package{?s} {.pkg {bad}} could not be loaded.")
    }
  }

  if (length(x_full) > 0) {
    purrr::map(
      x_full,
      ~ try(suppressPackageStartupMessages(attachNamespace(.x)), silent = TRUE)
    )
  }

  invisible(TRUE)
}

infra_pkgs <- c(
  "tune", "recipes", "parsnip", "yardstick", "purrr", "dplyr", "tibble",
  "dials", "rsample", "workflows", "tidyr", "rlang", "vctrs"
)
