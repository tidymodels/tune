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


#' @export
#' @keywords internal
#' @rdname empty_ellipses
#' @param parameters A `parameters` object.
#' @param metrics A metric set.
#' @param eval_time A numeric vector of time points where dynamic event time
#' metrics should be computed (e.g. the time-dependent ROC curve, etc).
#' @param outcomes A character vector of outcome names.
#' @param rset_info Attributes from an `rset` object.
#' @param workflow The workflow used to fit the iteration results.
new_iteration_results <-
  function(x,
           parameters,
           metrics,
           eval_time,
           eval_time_target,
           outcomes = character(0),
           rset_info,
           workflow) {
    new_tune_results(
      x = x,
      parameters = parameters,
      metrics = metrics,
      eval_time = eval_time,
      eval_time_target = eval_time_target,
      outcomes = outcomes,
      rset_info = rset_info,
      workflow = workflow,
      class = "iteration_results"
    )
  }
