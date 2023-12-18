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

new_resample_results <-
  function(x,
           parameters,
           metrics,
           eval_time,
           eval_time_target = NULL,
           outcomes = character(0),
           rset_info,
           workflow = NULL) {
    new_tune_results(
      x = x,
      parameters = parameters,
      metrics = metrics,
      eval_time = eval_time,
      eval_time_target = eval_time_target,
      outcomes = outcomes,
      rset_info = rset_info,
      workflow = workflow,
      class = "resample_results"
    )
  }
