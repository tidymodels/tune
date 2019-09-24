# ------------------------------------------------------------------------------
# Helpers for parallel processing

get_operator <- function(allow = TRUE, object) {

  pkgs <- mod_pkgs(object$fit$model$model)
  blacklist <- c("keras", "rJava")
  if (allow && any(pkgs %in% blacklist)) {
    pkgs <- pkgs[pkgs %in% blacklist]
    msg <- paste0("'", pkgs, "'", collapse = ", ")
    msg <- paste("Some required packages prohibit parallel processing: ", msg)
    cli::cli_alert_warning(msg)
    allow <- FALSE
  }

  cond <- allow && foreach::getDoParWorkers() > 1
  if (cond) {
    res <- foreach::`%dopar%`
  } else {
    res <- foreach::`%do%`
  }
  res
}

fe_pkg_list <- c('cli', 'crayon', 'dplyr', 'parsnip', 'purrr', 'recipes',
                 'rlang', 'rsample', 'tidyr', 'tune', 'yardstick')



