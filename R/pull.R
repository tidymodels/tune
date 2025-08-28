extract_details <- function(object, extractor) {
  if (is.null(extractor)) {
    return(list())
  }
  extractor(object)
}

#' Convenience functions to extract model
#'
#' `r lifecycle::badge("soft-deprecated")`
#'
#' Use [`extract_fit_engine()`][extract_fit_engine.tune_results()] instead of `extract_model()`.
#'
#' When extracting the fitted results, the workflow is easily accessible. If
#' there is only interest in the model, this functions can be used
#' as a shortcut
#' @param x A fitted workflow object.
#' @return A fitted model.
#' @export
extract_model <- function(x) {
  lifecycle::deprecate_warn(
    "0.1.6",
    "extract_model()",
    "extract_fit_engine()"
  )
  parsnip_fit <- extract_fit_parsnip(x)
  model <- parsnip_fit$fit
  model
}
