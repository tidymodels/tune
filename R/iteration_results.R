# ------------------------------------------------------------------------------

#' @export
`[.iteration_results` <- function(x, i, j, ...) {
  out <- NextMethod()
  iteration_results_reconstruct(out, x)
}

#' @export
`names<-.iteration_results` <- function(x, value) {
  out <- NextMethod()
  iteration_results_reconstruct(out, x)
}

# ------------------------------------------------------------------------------

# Same as `tune_results`, except that the vctrs invariants also require that
# the `.iter` column sticks around.

new_iteration_results <- function(x, parameters, metrics, rset_info) {
  new_tune_results(
    x = x,
    parameters = parameters,
    metrics = metrics,
    rset_info = rset_info,
    class = "iteration_results"
  )
}
