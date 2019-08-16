# ------------------------------------------------------------------------------
# Helpers for parallel processing

get_operator <- function(allow = TRUE) {
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

mod_pkgs <- function(x) {
  spec <- x$fit$model$model
  mod_name <- class(spec)[1]
  pkg_list <-
    parsnip::get_from_env(paste0(mod_name, "_pkgs")) %>%
    dplyr::filter(engine == x$fit$model$model$engine) %>%
    dplyr::pull(pkg)
  pkg_list[[1]]
}

