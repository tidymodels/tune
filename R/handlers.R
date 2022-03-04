# wrapper for executions that catches warnings and errors if they occur. See
# https://adv-r.hadley.nz/conditions.html
catcher <- function(expr) {
  signals <- list()
  add_cond <- function(cnd) {
    signals <<- append(signals, list(cnd))
    rlang::cnd_muffle(cnd)
  }

  res <- try(withCallingHandlers(warning = add_cond, expr), silent = TRUE)
  list(res = res, signals = signals)
}
