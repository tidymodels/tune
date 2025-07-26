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
    check_string(x)
    check_function(color_text, allow_null = TRUE)
    check_function(color_prefix, allow_null = TRUE)
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
tune_env <- rlang::new_environment(
  data = list(
    progress_env = NULL,
    progress_active = FALSE,
    progress_catalog = NULL,
    progress_started = FALSE
  )
)

lbls <- c(LETTERS, letters, 1:1e3)

# determines whether a currently running tuning process uses the cataloger.
uses_catalog <- function() {
  isTRUE(tune_env$progress_active && !is_testing())
}

# determines whether a tuning process is currently active.
# this function is intended to guard calls to `initialize_catalog()`
# inside of machinery that is wrapped by other tuning
# methods. e.g. `tune_bayes()` has an issue cataloger that ought not to be
# overwritten when fitting over an initial grid.
catalog_is_active <- function() {
  tune_env$progress_active
}

# initializes machinery for the tune catalog inside of an environment.
# the `env` should be an execution environment that persists throughout the
# tuning process for a given tuning approach and exits once tuning is completed.
#
# this function attaches an exit handler (see `on.exit()`) to the execution
# environment of the function it's called within, by default. pay close
# attention when other exit handlers are attached to the same environment;
# a call to `on.exit()` in `env` after this function is called will cause
# issues with the catalog. (see #845.)
#
#' @rdname tune-internal-functions
#' @export
initialize_catalog <- function(control, env = rlang::caller_env()) {
  catalog <-
    tibble::new_tibble(
      list(
        type = character(0),
        note = character(0),
        n = numeric(0),
        id = numeric(0)
      ),
      nrow = 0
    )

  if (!(allow_parallelism(control$allow_par) ||
        is_testing()) &&
      !control$verbose) {
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
  withr::defer(
    rlang::env_bind(tune_env, progress_started = FALSE),
    envir = env
  )

  invisible(NULL)
}

# given a catalog, summarize errors and warnings in a 1-length glue vector.
# for use by the progress bar inside of `tune_catalog()`.
summarize_catalog <- function(catalog, sep = "   ") {
  if (nrow(catalog) == 0) {
    return("")
  }

  res <- dplyr::arrange(catalog, id)
  res <- dplyr::mutate(
    res,
    color = dplyr::if_else(
      type == "warning",
      list(cli::col_yellow),
      list(cli::col_red)
    )
  )
  res <- dplyr::rowwise(res)
  res <- dplyr::mutate(
    res,
    msg = glue::glue("{color(cli::style_bold(lbls[id]))}: x{n}")
  )
  res <- dplyr::ungroup(res)
  res <- dplyr::pull(res, msg)
  res <- glue::glue_collapse(res, sep = sep)

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
    tibble::new_tibble(
      list(type = type, note = msg),
      nrow = length(msg)
    )

  tune_catalog(issues)

  invisible(NULL)
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

  msg <- paste(symb, msg)

  message(msg)
}

tune_log <- function(control, split_labels = NULL, task, type = "success", catalog = TRUE, ...) {
  if (!any(control$verbose, control$verbose_iter)) {
    return(invisible(NULL))
  }

  if (task == "internal") {
    return(NULL)
  }

  if (uses_catalog() & catalog) {
    log_catalog(task, type)
    return(NULL)
  }

  if (!is.null(split_labels)) {
    labs <- rev(unlist(split_labels))
    labs <- paste0(labs, collapse = ", ")
    labs <- paste0(labs, ": ")
  } else {
    labs <- ""
  }

  # see https://github.com/r-lib/cli/issues/92
  task <- gsub("\\{", "", task)

  task <- paste0(labs, task)
  siren(task, type = type)
  NULL
}

# copied from testthat::is_testing
is_testing <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")
}

#' @export
#' @rdname tune-internal-functions
.catch_and_log <- function(.expr, ..., bad_only = FALSE, notes, catalog = TRUE) {
  dots <- list(...)
  tune_log(..., type = "info", catalog = catalog, task = dots$location)
  tmp <- catcher(.expr)

  if (has_log_notes(tmp)) {
    notes <- append_log_notes(notes, tmp, dots$location)
    catalog_log(notes)
  }
  tmp <- remove_log_notes(tmp)
  assign("notes", notes, envir = parent.frame())

  tmp
}

catcher <- function(expr) {
  signals <- list()
  add_cond <- function(cnd) {
    signals <<- append(signals, list(rlang::cnd_entrace(cnd)))
    rlang::cnd_muffle(cnd)
  }

  res <- rlang::try_fetch(
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

  attr(res, "notes") <- signals
  res
}

is_failure <- function(x) {
  inherits(x, "try-error")
}

has_log_notes <- function(x) {
  is_failure(x) || NROW(attr(x, "notes")) > 0
}

append_log_notes <- function(notes, x, location) {
  if (is.null(notes)) {
    notes <- new_note()
  }

  wrns <- attr(x, "notes")
  if (length(wrns) > 0) {
    for (wrn in wrns) {
      type <- "warning"
      note <- wrn$message

      notes <- tibble::add_row(
        notes,
        location = unclass(location),
        type = type,
        note = note,
        trace = list(wrn$trace)
      )
    }
  }

  if (is_failure(x)) {
    type <- "error"
    x <- attr(x, "condition")
    note <- conditionMessage(x)

    notes <- tibble::add_row(
      notes,
      location = unclass(location),
      type = type,
      note = note,
      trace = list(x$trace)
    )
  }

  notes
}

remove_log_notes <- function(x) {
  attr(x, "notes") <- NULL
  x
}

catalog_log <- function(x) {
  catalog <- rlang::env_get(tune_env, "progress_catalog")

  if (is.null(catalog)) {
    catalog <- tibble::new_tibble(
      list(
        type = character(0),
        note = character(0),
        n = numeric(0),
        id = numeric(0)
      ),
      nrow = 0
    )
  }

  for (i in seq_along(x$note)) {
    x_note <- x$note[i]
    x_type <- x$type[i]

    if (x_note %in% catalog$note) {
      idx <- match(x_note, catalog$note)
      catalog$n[idx] <- catalog$n[idx] + 1
    } else {
      new_id <- nrow(catalog) + 1
      catalog <- tibble::add_row(
        catalog,
        tibble::tibble(
          type = x_type,
          note = x_note,
          n = 1,
          id = new_id
        )
      )

      # construct issue summary
      color <- if (x_type == "warning") cli::col_yellow else cli::col_red
      # pad by nchar(label) + nchar("warning") + additional spaces and symbols
      pad <- nchar(new_id) + 14L
      justify <- paste0("\n", strrep("\u00a0", pad))
      note <- gsub("\n", justify, x_note)
      # pad `nchar("warning") - nchar("error")` spaces to the right of the `:`
      if (x_type == "error") {
        note <- paste0("\u00a0\u00a0", note)
      }
      msg <- glue::glue(
        "{color(cli::style_bold(lbls[new_id]))} | {color(x_type)}: {note}"
      )
      cli::cli_alert(msg)
    }
  }

  rlang::env_bind(tune_env, progress_catalog = catalog)
  rlang::env_bind(
    tune_env$progress_env,
    catalog_summary = summarize_catalog(catalog)
  )

  if (uses_catalog()) {
    if (!tune_env$progress_started) {

      rlang::with_options(
        cli::cli_progress_bar(
          type = "custom",
          format = "There were issues with some computations   {catalog_summary}",
          clear = FALSE,
          .envir = tune_env$progress_env
        ),
        cli.progress_show_after = 0
      )
      rlang::env_bind(tune_env, progress_started = TRUE)
    }


    cli::cli_progress_update(.envir = tune_env$progress_env)
  }

  return(NULL)
}

new_note <- function(
    location = character(0),
    type = character(0),
    note = character(0),
    trace = list()
) {
  tibble::new_tibble(
    list(
      location = location,
      type = type,
      note = note,
      trace = trace
    )
  )
}

log_best <- function(control, iter, info, digits = 4) {
  # TODO melodie; keep: used by tune_bayes
  if (!isTRUE(control$verbose_iter)) {
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
  tune_log(control, split_labels = NULL, task = msg, type = "info", catalog = FALSE)
}

check_and_log_flow <- function(control, results) {
  if (!isTRUE(control$verbose_iter)) {
    return(invisible(NULL))
  }

  if (all(is.na(results$.mean))) {
    if (nrow(results) < 2) {
      tune_log(control, split_labels = NULL, task = "Halting search",
               type = "danger", catalog = FALSE)
      eval.parent(parse(text = "break"))
    } else {
      tune_log(control, split_labels = NULL, task = "Skipping to next iteration",
               type = "danger", catalog = FALSE)
      eval.parent(parse(text = "next"))
    }
  }
  invisible(NULL)
}

log_progress <- function(control, x, maximize = TRUE, objective = NULL,
                         eval_time = NULL, digits = 4) {
  if (!isTRUE(control$verbose_iter)) {
    return(invisible(NULL))
  }

  x <- dplyr::filter(x, .metric == objective)
  if (!is.null(eval_time)) {
    x <- dplyr::filter(x, .eval_time == eval_time)
  }

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
  if (!isTRUE(control$verbose_iter)) {
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
  if (!isTRUE(control$verbose_iter)) {
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
    tune_log(control, split_labels = NULL, task = val, type = "info", catalog = FALSE)
  }
  invisible(NULL)
}
