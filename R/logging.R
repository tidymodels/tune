# General logging to the screen used in various places.
# ------------------------------------------------------------------------------

#' Write a message that respects the line width
#'
#' @param x A character string of the message text.
#' @param width An integer for the width.
#' @param prefix An optional string to go on the first line of the message.
#' @param color_text,color_prefix A function (or `NULL`) that is used to color
#'  the text and/or prefix.
#' @return The processed text is returned (invisibly) but a message is written.
#' @examples
#' library(cli)
#' Gaiman <-
#'   paste(
#'     '"Good point." Bod was pleased with himself, and glad he had thought of',
#'     "asking the poet for advice. Really, he thought, if you couldn't trust a",
#'     "poet to offer sensible advice, who could you trust?",
#'     collapse = ""
#'   )
#' message_wrap(Gaiman)
#' message_wrap(Gaiman, width = 20, prefix = "-")
#' message_wrap(Gaiman,
#'   width = 30, prefix = "-",
#'   color_text = cli::col_silver
#' )
#' message_wrap(Gaiman,
#'   width = 30, prefix = "-",
#'   color_text = cli::style_underline,
#'   color_prefix = cli::col_green
#' )
#' @export
message_wrap <-
  function(x, width = options()$width - 2, prefix = "", color_text = NULL, color_prefix = color_text) {
    if (!is.character(x) || length(x) > 1) {
      rlang::abort("'x' should be a single character string.")
    }
    if (!is.null(color_text) && !is.function(color_text)) {
      rlang::abort("'color_text' should be null or a function.")
    }
    if (!is.null(color_prefix) && !is.function(color_prefix)) {
      rlang::abort("'color_prefix' should be null or a function.")
    }
    n <- nchar(prefix)
    if (n > 0) {
      buffer <- paste0(rep(" ", n + 1), collapse = "")
    } else {
      buffer <- ""
    }
    msg <- strwrap(x, width = width - n - 1)
    if (!is.null(color_text)) {
      msg <- purrr::map_chr(msg, ~ color_text(.x))
    }
    if (!is.null(color_prefix)) {
      prefix <- color_prefix(prefix)
    }
    msg[-length(msg)] <- paste0(msg[-length(msg)], "\n")
    msg[-1] <- paste0(buffer, msg[-1])
    if (n > 0) {
      msg[1] <- paste(prefix, msg[1])
    }
    message(msg)
    invisible(msg)
  }

# issue cataloger --------------------------------------------------------------
tune_env <-
  rlang::new_environment(
    data = list(progress_env = NULL, progress_active = FALSE, progress_catalog = NULL)
  )

# determines whether a supplied bar id references a tuning process that
# uses the issue cataloger.
uses_catalog <- function() {
  isTRUE(tune_env$progress_active && !is_testing())
}

# determines whether a supplied bar id references an issue cataloger that
# has not yet been terminated. this function is intended to guard calls to
# `initialize_catalog()` inside of machinery that is wrapped by other tuning
# method. e.g. `tune_bayes()` has an issue cataloger that ought not to be
# overwritten when fitting over an initial grid.
catalog_is_active <- function() {
  tune_env$progress_active
}

# initializes machinery for the tune catalog inside of an environment.
# the `env` should be an execution environment that persists throughout the
# tuning process for a given tuning approach and exits once tuning is completed.
#' @rdname tune-internal-functions
#' @export
initialize_catalog <- function(env = rlang::caller_env(), control) {
  catalog <-
    tibble::tibble(
      type = character(0),
      note = character(0),
      n = numeric(0),
      id = numeric(0)
    )

  if (!(allow_parallelism(control$allow_par) || is_testing())) {
    progress_active <- TRUE
  } else {
    progress_active <- FALSE
  }

  rlang::env_bind(tune_env, progress_env = env)

  rlang::env_bind(tune_env, progress_catalog = catalog)
  withr::defer(
    rlang::env_bind(tune_env, progress_catalog = NULL),
    envir = env
  )

  rlang::env_bind(tune_env, progress_active = progress_active)
  withr::defer(
    rlang::env_bind(tune_env, progress_active = FALSE),
    envir = env
  )


  invisible(NULL)
}

# given a catalog, summarize errors and warnings in a 1-length glue vector.
# for use by the progress bar inside of `tune_catalog()`.
summarize_catalog <- function(catalog) {
  if (nrow(catalog) == 0) {
    return("")
  }

  res <- dplyr::arrange(catalog, id)
  res <- dplyr::mutate(res, color = dplyr::if_else(type == "warning", list(cli::col_yellow), list(cli::col_red)))
  res <- dplyr::rowwise(res)
  res <- dplyr::mutate(res, msg = glue::glue("{color(cli::style_bold(id))}: x{n}"))
  res <- dplyr::ungroup(res)
  res <- dplyr::pull(res, msg)
  res <- glue::glue_collapse(res, sep = "   ")

  res
}

# a light wrapper around `tune_catalog()` for use inside of `tune_log()`
log_catalog <- function(msg, type) {
  type <-
    switch(
      type,
      warning = "warning",
      danger = "error",
      return(invisible(NULL))
    )

  issues <-
    tibble::tibble(
      type = type,
      note = msg
    )

  tune_catalog(issues)

  invisible(NULL)
}

# an alternative to `tune_log()` that maintains a "catalog" of previously
# encountered issues, and interactively summarizes them by type rather than
# printing out each new tuning issue individually.
tune_catalog <- function(issues) {
  catalog <- rlang::env_get(env = tune_env, nm = "progress_catalog")

  res <- dplyr::count(issues, type, note) %>% mutate(id = NA_integer_)
  res <- dplyr::bind_rows(res, catalog)
  res <- dplyr::group_by(res, type, note)
  # dplyr::first will gain an `na_rm` argument in 1.1.0
  res <- dplyr::summarize(res, n = sum(n), id = dplyr::first(id[!is.na(id)]))
  res <- dplyr::ungroup(res)

  for (issue in seq_along(res[[1]])) {
    current <- res[issue,]
    if (is.na(current$id)) {
      current_ids <- res$id[!is.na(res$id)]
      if (length(current_ids) == 0) {
        res[issue, "id"] <- 1L
      } else {
        res[issue, "id"] <- max(current_ids) + 1L
      }

      # construct issue summary
      color <- if (current$type == "warning") {cli::col_yellow} else {cli::col_red}
      msg <- glue::glue(
        "{color(cli::style_bold(res[issue, 'id']))} | {color(current$type)}: {current$note}"
      )
      cli::cli_alert(msg)
    }
  }

  rlang::env_bind(tune_env, progress_catalog = res)
  rlang::env_bind(tune_env$progress_env, catalog_summary = summarize_catalog(res))

  if (nrow(catalog) == 0) {
    rlang::with_options(
      cli::cli_progress_bar(
        type = "custom",
        format = "There were issues with some computations   {catalog_summary}",
        clear = FALSE,
        .envir = tune_env$progress_env
      ),
      cli.progress_show_after = 0
    )
  }

  cli::cli_progress_update(.envir = tune_env$progress_env)

  invisible(TRUE)
}

# catching and logging ---------------------------------------------------------
siren <- function(x, type = "info") {
  tune_color <- get_tune_colors()
  types <- names(tune_color$message)
  type <- match.arg(type, types)

  msg <- glue::glue(x, .trim = FALSE)

  symb <- dplyr::case_when(
    type == "warning" ~ tune_color$symbol$warning("!"),
    type == "go" ~ tune_color$symbol$go(cli::symbol$pointer),
    type == "danger" ~ tune_color$symbol$danger("x"),
    type == "success" ~ tune_color$symbol$success(tune_symbol$success),
    type == "info" ~ tune_color$symbol$info("i")
  )

  msg <- dplyr::case_when(
    type == "warning" ~ tune_color$message$warning(msg),
    type == "go" ~ tune_color$message$go(msg),
    type == "danger" ~ tune_color$message$danger(msg),
    type == "success" ~ tune_color$message$success(msg),
    type == "info" ~ tune_color$message$info(msg)
  )

  if (inherits(msg, "character")) {
    msg <- as.character(msg)
  }
  message(paste(symb, msg))
}


tune_log <- function(control, split = NULL, task, type = "success") {
  if (!control$verbose) {
    return(invisible(NULL))
  }

  if (uses_catalog()) {
    log_catalog(task, type)
    return(NULL)
  }

  if (!is.null(split)) {
    labs <- labels(split)
    labs <- rev(unlist(labs))
    labs <- paste0(labs, collapse = ", ")
    labs <- paste0(labs, ": ")
  } else {
    labs <- ""
  }

  # see https://github.com/r-lib/cli/issues/92
  task <- gsub("\\{", "", task)

  siren(paste0(labs, task), type = type)
  NULL
}

# copied from testthat::is_testing
is_testing <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")
}

log_problems <- function(notes, control, split, loc, res, bad_only = FALSE) {
  # Always log warnings and errors
  control2 <- control
  control2$verbose <- TRUE

  should_catalog <- !(allow_parallelism(control$allow_par) || is_testing())

  wrn <- res$signals
  if (length(wrn) > 0) {
    wrn_msg <- purrr::map_chr(wrn, ~conditionMessage(.x))
    wrn_msg <- unique(wrn_msg)
    wrn_msg <- paste(wrn_msg, collapse = ", ")

    wrn_msg <- tibble::tibble(location = loc, type = "warning", note = wrn_msg)

    notes <- dplyr::bind_rows(notes, wrn_msg)

    if (should_catalog) {
      tune_catalog(dplyr::filter(notes, type == "warning" & location == loc))
    } else {
      wrn_msg <- format_msg(loc, wrn_msg$note)
      tune_log(control2, split, wrn_msg, type = "warning")
    }
  }
  if (inherits(res$res, "try-error")) {
    if (should_catalog) {
      err_msg <- conditionMessage(attr(res$res, "condition"))
    } else {
      err_msg <- as.character(attr(res$res, "condition"))
      err_msg <- gsub("\n$", "", err_msg)
    }

    err_msg <- tibble::tibble(location = loc, type = "error", note = err_msg)

    notes <- dplyr::bind_rows(notes, err_msg)

    if (should_catalog) {
      tune_catalog(dplyr::filter(notes, type == "error" & location == loc))
    } else {
      err_msg <- format_msg(loc, err_msg$note)
      tune_log(control2, split, err_msg, type = "danger")
    }
  } else {
    if (!bad_only) {
      tune_log(control, split, loc, type = "success")
    }
  }
  notes
}

format_msg <- function(loc, msg) {
  msg <- trimws(msg, "left")
  # truncate by line
  by_line <- strsplit(msg, split = "\n")[[1]]
  multi_line <- length(unlist(by_line)) > 1
  # A tad of indentation
  if (multi_line) {
    by_line <- purrr::map(by_line, ~ paste0("  ", .x))
  }
  by_line <- purrr::map(by_line, glue::glue_collapse, width = options()$width - 5)
  # reform message
  msg <- paste0(by_line, collapse = "\n")
  sep <- ifelse(multi_line, ":\n", ": ")
  paste0(loc, sep, msg)
}

#' @export
#' @rdname tune-internal-functions
.catch_and_log <- function(.expr, ..., bad_only = FALSE, notes) {
  tune_log(..., type = "info")
  tmp <- catcher(.expr)
  new_notes <- log_problems(notes, ..., tmp, bad_only = bad_only)
  assign("out_notes", new_notes, envir = parent.frame())
  tmp$res
}

#' @export
#' @rdname tune-internal-functions
.catch_and_log_fit <- function(.expr, ..., notes) {
  tune_log(..., type = "info")

  caught <- catcher(.expr)
  result <- caught$res

  # Log failures that come from parsnip before the model is fit
  if (is_failure(result)) {
    result_parsnip <- list(res = result, signals = list())

    new_notes <- log_problems(notes, ..., result_parsnip)
    assign("out_notes", new_notes, envir = parent.frame())
    return(result)
  }

  if (!is_workflow(result)) {
    rlang::abort("Internal error: Model result is not a workflow!")
  }

  # Extract the parsnip model from the fitted workflow
  fit <- result$fit$fit$fit

  # Log underlying fit failures that parsnip caught during the actual
  # fitting process
  if (is_failure(fit)) {
    result_fit <- list(res = fit, signals = list())

    new_notes <- log_problems(notes, ..., result_fit)
    assign("out_notes", new_notes, envir = parent.frame())
    return(result)
  }

  new_notes <- log_problems(notes, ..., caught)
  assign("out_notes", new_notes, envir = parent.frame())

  result
}

log_best <- function(control, iter, info, digits = 4) {
  if (!control$verbose) {
    return(invisible(NULL))
  }

  message("")
  message(cli::rule(left = bold(paste("Iteration", iter))))
  message("")

  msg <-
    paste0(
      "Current best:\t\t",
      info$metrics,
      "=",
      signif(info$best_val, digits = digits),
      " (@iter ",
      info$best_iter,
      ")"
    )
  tune_log(control, split = NULL, task = msg, type = "info")
}

check_and_log_flow <- function(control, results) {
  if (all(is.na(results$.mean))) {
    if (nrow(results) < 2) {
      tune_log(control, split = NULL, task = "Halting search", type = "danger")
      eval.parent(parse(text = "break"))
    } else {
      tune_log(control, split = NULL, task = "Skipping to next iteration", type = "danger")
      eval.parent(parse(text = "next"))
    }
  }
  invisible(NULL)
}

log_progress <- function(control, x, maximize = TRUE, objective = NULL, digits = 4) {
  if (!control$verbose) {
    return(invisible(NULL))
  }

  x <- dplyr::filter(x, .metric == objective)
  if (maximize) {
    bst <- which.max(x$mean)
  } else {
    bst <- which.min(x$mean)
  }
  bst_iter <- x$.iter[bst]
  max_iter <- max(x$.iter)
  bst_val <- x$mean[x$.iter == max_iter]
  bst_se <- x$std_err[x$.iter == max_iter]
  msg <-
    paste0(
      " Newest results:\t",
      objective,
      "=",
      signif(bst_val, digits = digits)
    )
  if (!is.na(bst_se) && bst_se > 0) {
    msg <- paste0(msg, " (+/-", signif(bst_se, digits = digits - 1), ")")
  }

  if (bst_iter == max_iter) {
    msg <- paste0(red(cli::symbol$heart), msg)
  } else {
    msg <- paste0(silver(cli::symbol$circle_cross), msg)
  }
  message(msg)
}

param_msg <- function(control, candidate) {
  if (!control$verbose) {
    return(invisible(NULL))
  }
  candidate <- candidate[, !(names(candidate) %in% c(".mean", ".sd", "objective"))]
  p_chr <- paste0(names(candidate), "=", format(as.data.frame(candidate), digits = 3))
  p_chr <- paste0(p_chr, collapse = ", ")
  message_wrap(p_chr,
    prefix = "i",
    color_text = get_tune_colors()$message$info,
    color_prefix = get_tune_colors()$symbol$info
  )
  invisible(NULL)
}


acq_summarizer <- function(control, iter, objective = NULL, digits = 4) {
  if (!control$verbose) {
    return(invisible(NULL))
  }
  if (inherits(objective, "conf_bound") && is.function(objective$kappa)) {
    val <- paste0("Kappa value: ", signif(objective$kappa(iter), digits = digits))
  } else {
    if (inherits(objective, c("exp_improve", "prob_improve")) &&
      is.function(objective$trade_off)) {
      val <- paste0("Trade-off value: ", signif(objective$trade_off(iter), digits = digits))
    } else {
      val <- NULL
    }
  }
  if (!is.null(val)) {
    tune_log(control, split = NULL, task = val, type = "info")
  }
  invisible(NULL)
}
