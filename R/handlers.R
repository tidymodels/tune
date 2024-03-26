# wrapper for executions that catches warnings and errors if they occur. See
# https://adv-r.hadley.nz/conditions.html
catcher <- function(expr) {
  signals <- list()
  add_cond <- function(cnd) {
    signals <<- append(signals, list(rlang::cnd_entrace(cnd)))
    rlang::cnd_muffle(cnd)
  }

  res <-
    rlang::try_fetch(
      expr,
      warning = add_cond,
      error = function(e) {
        structure(
          catch_message(e),
          class = "try-error",
          # if a simple error, add a traceback.
          # otherwise, pass the condition right along.
          condition = rlang::`%||%`(rlang::cnd_entrace(e), e)
        )
      }
    )

  list(res = res, signals = signals)
}

# A simplified version of the error handler supplied in `try()` source
catch_message <- function(e) {
  paste0("Error in ", deparse(conditionCall(e)), ": ", conditionMessage(e), "\n")
}
