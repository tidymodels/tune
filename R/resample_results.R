# ------------------------------------------------------------------------------

#' @export
`[.resample_results` <- function(x, i, j, ...) {
  out <- NextMethod()
  resample_results_reconstruct(out, x)
}

#' @export
`names<-.resample_results` <- function(x, value) {
  out <- NextMethod()
  resample_results_reconstruct(out, x)
}

# ------------------------------------------------------------------------------

new_resample_results <- function(x, parameters, metrics, outcomes = character(0), rset_info) {
  new_tune_results(
    x = x,
    parameters = parameters,
    metrics = metrics,
    outcomes = outcomes,
    rset_info = rset_info,
    class = "resample_results"
  )
}
