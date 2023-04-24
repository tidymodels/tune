#' @importFrom stats predict

#' @noRd
#' @method predict tune_results
#' @export
predict.tune_results <- function(object, ...) {
  cli::cli_abort(c(
          "`predict()` is not well-defined for tuning results.",
    "i" = "To predict with the optimal model configuration from tuning \\
           results, ensure that the tuning result was generated with the \\
           {.help [control option](tune::control_grid)} \\
           {.code save_workflow = TRUE}, run \\
           {.help [{.fun fit_best}](tune::fit_best)}, and then predict using \\
           {.help [{.fun predict}](workflows::predict.workflow)} on its output.",
    "i" = "To collect predictions from tuning results, ensure that \\
           the tuning result was generated with the \\
           {.help [control option](tune::control_grid)} \\
           {.code save_pred = TRUE} and run \\
           {.help [{.fun collect_predictions}](tune::collect_predictions)}."
  ))
}

#' @noRd
#' @method predict last_fit
#' @export
predict.last_fit <- function(object, ...) {
  cli::cli_abort(c(
          "`predict()` is not well-defined for {.code last_fit()} results.",
    "i" = "To predict with the model fitted in {.code last_fit()}, first run \\
           {.help [{.fun extract_workflow}](tune::extract_workflow.last_fit)} \\
           and then predict using \\
           {.help [{.fun predict}](workflows::predict.workflow)} on its output."
  ))
}


