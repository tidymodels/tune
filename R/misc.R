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

# ------------------------------------------------------------------------------

# Genneral logging to the screen used in various places.

tune_log <- function(control, split, task, alert = cli::cli_alert_success) {
  if (!control$verbose) {
    return(invisible(NULL))
  }
  if (!is.null(split)) {
    labs <- labels(split)
    labs <- rev(unlist(labs))
    labs <- paste0(labs, collapse = ", ")
    labs <- paste0(labs, ": ")
  } else {
    labs <- ""
  }

  if (isTRUE(all.equal(alert, cli::cli_alert_warning))) {
    alert(cli::col_yellow(paste0(labs, task)))
  } else {
    alert(paste0(labs, task))
  }
  NULL
}

log_problems <- function(control, split, res, loc, warn_only = FALSE) {
  # Always log warnings and errors
  control2 <- control
  control2$verbose = TRUE

  wrn <- res$signals
  if (length(wrn) > 0) {
    wrn_msg <- map_chr(wrn, ~ .x$message)
    wrn_msg <- unique(wrn_msg)
    wrn_msg <- paste(wrn_msg, collapse = ", ")
    wrn_msg <- glue::glue_collapse(wrn_msg, width = options()$width - 5)
    wrn_msg <- paste0(loc, ": ", wrn_msg)
    tune_log(control2, split, wrn_msg, cli_alert_warning)
  }
  if (!warn_only) {
    if (inherits(res$res, "try-error")) {
      err_msg <- as.character(attr(res$res,"condition"))
      err_msg <- gsub("\n$", "", err_msg)
      err_msg <- glue::glue_collapse(err_msg, width = options()$width - 5)
      err_msg <- paste0(loc, ": ", err_msg)
      tune_log(control2, split, err_msg, cli_alert_danger)
    } else {
      tune_log(control, split, loc, cli::cli_alert_success)
    }
  }
  NULL
}

# ------------------------------------------------------------------------------

tidyr_new_interface <- function() {
  packageVersion("tidyr") > "0.8.99"
}



