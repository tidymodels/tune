#' @importFrom dplyr filter select %>% full_join mutate bind_rows case_when
#' @importFrom dplyr one_of ungroup slice bind_cols pull sample_n desc anti_join
#' @importFrom dplyr distinct arrange rename mutate_if starts_with
#' @importFrom purrr map_lgl map_dfr iwalk map map_chr map_int map2_dfc
#' @importFrom tibble tibble lst
#' @importFrom rlang call2 ns_env is_quosure is_quosures quo_get_expr call_name
#' @importFrom rlang is_false eval_tidy expr sym syms env_get is_function :=
#' @importFrom rlang is_missing
#' @importFrom glue glue glue_collapse
#' @importFrom utils globalVariables capture.output packageVersion
#' @importFrom dials parameters_constr is_unknown encode_unit
#' @importFrom stats sd qt qnorm dnorm pnorm predict model.matrix setNames
#' @importFrom stats model.matrix model.response model.frame
#' @importFrom yardstick rsq rmse accuracy kap
#' @importFrom tidyr unnest nest
#' @importFrom GPfit GP_fit
#' @importFrom parsnip get_from_env
#' @importFrom recipes all_predictors all_outcomes
#' @importFrom ggplot2 ggplot aes xlab geom_point geom_errorbar facet_wrap ylab
#' @importFrom ggplot2 facet_grid
#' @importFrom cli cli_alert_danger cli_alert_info cli_alert_warning
#' @importFrom cli cli_alert_success cli_alert
#' @importFrom foreach foreach getDoParName

# ------------------------------------------------------------------------------

#' @importFrom dials parameters
#' @export
dials::parameters

# ------------------------------------------------------------------------------

utils::globalVariables(
  c("engine", "name", "func", "parsnip", "call_name", ".step", "call_info",
    "component", "component_id", "id", "control", ".pred", ".metric",
    ".estimator", ".estimate", "perf", "n", "object", "splits", "grid", "rs",
    ".iter", "mean", ".submodels", "metrics", "data", ".mean", ".sd",
    "rs_iter", "pkg", ".pred_class", "std_err", "const", "objective", "delta",
    "sd_trunc", "snr", "z", "..val", "max_val", "has_submodel", "res",
    ".extracts", ".metrics", "value")
  )

# ------------------------------------------------------------------------------

tidyr_new_interface <- function() {
  packageVersion("tidyr") > "0.8.99"
}

# ------------------------------------------------------------------------------

# nocov start
s3_register <- function(generic, class, method = NULL) {
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  pieces <- strsplit(generic, "::")[[1]]
  stopifnot(length(pieces) == 2)
  package <- pieces[[1]]
  generic <- pieces[[2]]

  caller <- parent.frame()

  get_method_env <- function() {
    top <- topenv(caller)
    if (isNamespace(top)) {
      asNamespace(environmentName(top))
    } else {
      caller
    }
  }
  get_method <- function(method, env) {
    if (is.null(method)) {
      get(paste0(generic, ".", class), envir = get_method_env())
    } else {
      method
    }
  }

  method_fn <- get_method(method)
  stopifnot(is.function(method_fn))

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(package, "onLoad"),
    function(...) {
      ns <- asNamespace(package)

      # Refresh the method, it might have been updated by `devtools::load_all()`
      method_fn <- get_method(method)

      registerS3method(generic, class, method_fn, envir = ns)
    }
  )

  # Avoid registration failures during loading (pkgload or regular)
  if (!isNamespaceLoaded(package)) {
    return(invisible())
  }

  envir <- asNamespace(package)

  # Only register if generic can be accessed
  if (exists(generic, envir)) {
    registerS3method(generic, class, method_fn, envir = envir)
  }

  invisible()
}

# nocov end

.onLoad <- function(libname, pkgname) {

  # dynamically register autoplot
  s3_register("ggplot2::autoplot", "tune_results")

  invisible()
}
