#' @importFrom dplyr filter select %>% full_join mutate bind_rows case_when vars
#' @importFrom dplyr all_of ungroup slice bind_cols pull sample_n desc anti_join
#' @importFrom dplyr distinct arrange rename mutate_if starts_with inner_join
#' @importFrom dplyr last
#' @importFrom purrr map_int
#' @importFrom rlang call2 ns_env is_quosure is_quosures quo_get_expr call_name
#' @importFrom rlang is_false eval_tidy expr sym syms env_get is_function :=
#' @importFrom rlang is_missing %||% caller_env
#' @importFrom glue glue glue_collapse
#' @importFrom dials is_unknown encode_unit
#' @importFrom stats sd qt qnorm dnorm pnorm predict model.matrix setNames
#' @importFrom stats model.matrix model.response model.frame update median
#' @importFrom yardstick rsq rmse accuracy roc_auc brier_survival brier_class
#' @importFrom tidyr unnest nest
#' @importFrom GPfit GP_fit
#' @importFrom parsnip get_from_env required_pkgs
#' @importFrom recipes all_predictors all_outcomes
#' @importFrom ggplot2 ggplot aes xlab geom_point geom_errorbar facet_wrap ylab
#' @importFrom ggplot2 facet_grid geom_line aes_string aes_ scale_x_continuous
#' @importFrom cli cli_alert_danger cli_alert_info cli_alert_warning
#' @importFrom cli cli_alert_success cli_alert
#' @importFrom cli cli_inform cli_warn cli_abort qty
#' @importFrom foreach foreach getDoParName %dopar%
#' @importFrom tibble obj_sum size_sum
#' @import rlang
#' @importFrom future.apply future_lapply

# ------------------------------------------------------------------------------
# Only a small number of functions in workflows.
# It is worth just importing everything.

#' @import workflows

# ------------------------------------------------------------------------------

utils::globalVariables(
  c(
    ".", "engine", "name", "func", "parsnip", "call_name", ".step", "call_info",
    "component", "component_id", "id", "control", ".pred", ".metric",
    ".estimator", ".estimate", "n", "note", "object", "splits", "grid",
    "resamples", ".iter", "mean", ".submodels", "metrics", "data", ".mean",
    ".sd", "iteration", "pkg", ".pred_class", "std_err", "const", "objective",
    "delta", "sd_trunc", "snr", "z", "..val", "max_val", "has_submodel", "res",
    ".extracts", ".metrics", "value", ".notes", ".loss", ".bound",
    ".column", ".totals", ".value", "direction", ".config", "Freq", "Prediction",
    "Truth", ".seed", ".order", ".iter_model", ".iter_preprocessor",
    ".iter_config", ".msg_model", "# resamples", "seed", "pre", "type",
    "rowwise", ".best", "location", "msg", "..object", ".eval_time",
    ".pred_survival", ".pred_time", ".weight_censored", "nice_time",
    "time_metric", ".lower", ".upper", "i", "results", "term", ".alpha",
    ".method", "old_term", ".lab_pre", ".model", ".num_models", "model_stage",
    "predict_stage", "user", "num"
  )
)

# ------------------------------------------------------------------------------

release_bullets <- function() {
  c(
    "**Do this before checks!**. Update dependencies with `devtools::install_dev_deps()` and update the test objects via `R CMD BATCH --vanilla inst/test_objects.R`."
  )
}


