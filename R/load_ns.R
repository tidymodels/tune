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
  pkgs <- mod_pkgs(x)
  pkgs <- c(pkgs, "recipes", "parsnip", "yardstick", "purrr", "tibble", "dials",
            "rsample")
  load_namespace(pkgs)
}

#' @export
load_pkgs.workflow <- function(x, ...) {
  load_pkgs.model_spec(get_wflow_model(x))
}

full_load <- c("kknn", "earth")

load_namespace <- function(x) {
  if (length(x) == 0) {
    return(invisible(TRUE))
  }
  loaded <- purrr::map_lgl(x, isNamespaceLoaded)
  x <- x[!loaded]

  if (length(x) == 0) {
    return(invisible(TRUE))
  }
  fine <- purrr::map_lgl(x, requireNamespace, quietly = TRUE)
  if (any(!fine)) {
    bad <- x[!fine]
    msg <- paste0("'", bad, "'", collapse = ", ")
    stop(paste("These packages could not be loaded:", msg), call. = FALSE)
  }

  if (any(x %in% full_load)) {
    pkgs <- x[x %in% full_load]
    purrr::map(pkgs, attachNamespace)
  }

  invisible(TRUE)
}

mod_pkgs <- function(x) {
  mod_name <- class(x)[1]
  pkg_list <-
    parsnip::get_from_env(paste0(mod_name, "_pkgs")) %>%
    dplyr::filter(engine == x$engine) %>%
    dplyr::pull(pkg)
  pkg_list[[1]]
}
