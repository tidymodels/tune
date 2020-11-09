#' Quietly load package namespace
#'
#' For one or more packages, load the namespace. This is used during parallel
#' processing since the different parallel backends handle the package
#' environments differently.
#' @param x A character vector of packages.
#' @return An invisible NULL.
#' @keywords internal
#' @export
load_pkgs <- function(x, ...) {
  UseMethod("load_pkgs")
}

#' @export
load_pkgs.character <- function(x, ...) {
  load_namespace(x)
}

#' @export
load_pkgs.model_spec <- function(x, ...) {
  pkgs <- required_pkgs(x)
  load_namespace(unique(pkgs))
}

#' @export
load_pkgs.workflow <- function(x, ...) {
  load_pkgs.model_spec(workflows::pull_workflow_spec(x))
}

full_load <- c("kknn", "earth")

load_namespace <- function(x) {
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
      msg <- paste0("'", bad, "'", collapse = ", ")
      stop(paste("These packages could not be loaded:", msg), call. = FALSE)
    }
  }

  if (length(x_full) > 0) {
    purrr::map(x_full, ~ try(attachNamespace(.x), silent = TRUE))
  }

  invisible(TRUE)
}

## -----------------------------------------------------------------------------

infra_pkgs <- c("tune", "recipes", "parsnip", "yardstick", "purrr", "dplyr",
                "tibble", "dials", "rsample", "workflows", "tidyr", "rlang",
                "vctrs")

#' Determine packages required by objects
#'
#' @param x An object.
#' @return A character string.
#' @keywords internal
#' @rdname required_pkgs
#' @export
required_pkgs.model_spec <- function(x, infra = TRUE, ...) {
  mod_name <- class(x)[1]
  pkg_list <-
    parsnip::get_from_env(paste0(mod_name, "_pkgs")) %>%
    dplyr::filter(engine == x$engine) %>%
    dplyr::pull(pkg)
  res <- pkg_list[[1]]
  if (infra) {
    res <- c(infra_pkgs, res)
  }
  res <- unique(res)
  res <- res[length(res) != 0]
  res
}

#' @rdname required_pkgs
#' @export
required_pkgs.workflow <- function(x, infra = TRUE, ...) {
  res <- required_pkgs(workflows::pull_workflow_spec(x), infra = FALSE)
  pp <- workflows::pull_workflow_preprocessor(x)
  if (inherits(pp, "recipe")) {
    res_rec <- required_pkgs(pp, infra = FALSE)
    res <- c(res, res_rec)
  }
  if (infra) {
    res <- c(infra_pkgs, res)
  }
  res <- unique(res)
  res <- res[length(res) != 0]
  res
}
