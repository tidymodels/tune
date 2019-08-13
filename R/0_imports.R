#' @importFrom dplyr filter select %>% full_join mutate bind_rows case_when
#' @importFrom dplyr one_of ungroup
#' @importFrom purrr map_lgl map_dfr iwalk map map_chr
#' @importFrom tibble tibble
#' @importFrom rlang call2 ns_env is_quosure is_quosures quo_get_expr call_name
#' @importFrom rlang is_false eval_tidy expr sym
#' @importFrom glue glue
#' @importFrom utils globalVariables
#' @importFrom glue glue
#' @importFrom dials param_set_constr
#' @importFrom stats sd predict
#' @importFrom workflows outcome_names
#' @importFrom yardstick rsq rmse accuracy mn_log_loss
#' @importFrom tidyr unnest

# ------------------------------------------------------------------------------

#' @importFrom dials param_set
#' @export
dials::param_set

# ------------------------------------------------------------------------------

utils::globalVariables(
  c("engine", "name", "func", "parsnip", "call_name", ".step", "call_info",
    "component", "component_id", "id", "control", ".pred", ".metric",
    ".estimator", ".estimate", "perf", "n", "object", "splits", "grid", "rs")
  )

