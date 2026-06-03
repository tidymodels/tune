# Overview ---------------------------------------------------------------------
#
# General logging to the screen is used in various places. The code in this file
# is split into 4 sections.
#
# The first section contains functions that tune uses to let the user know what
# is happening. The main function is `update_printer()`, which is used in
# `logging.R` and throughout the package. Some examples of these are
# `Fold01: preprocessor 1/1, model 1/1` from `tune_grid()` and
# `Generating 3 candidates` from `tune_bayes()`. This function should be fully
# controlled by `control$verbose`.
#
# The second section contains everything related to the catalog object. This
# object contains all the errors and warnings that are captured when handling
# workflows inside the training loop. The catalog is a tibble with the columns
# `type`, `note`, `n`, and `id`.
#
# - `type` being whether it is a warning or an error,
# - `note` is what the condition was as a string,
# - `n` is how many times it was seen
# - `id` is a unique identifier (created using `lbls`)
#
# All of this infrastructure is done using the `tune_env` environment that
# contains the catalog itself, and other useful information that toggles whether
# we are using the catalog and ensures it is used correctly across the fits.
#
# The third section, `catching and logging`, is what is being used in tune or
# extension packages. `.catch_and_log()` is wrapping some code that we want to
# collect on. If an error or warning is seen, it will be captured and handled
# using the code in this and the previous section.
#
# The forth section contains functions that are exclusively used in
# `tune_bayes()`.

# Tune printing ----------------------------------------------------------------

update_printer <- function(
  control,
  split_labels = NULL,
  task,
  type = "success",
  catalog = TRUE,
  ...
) {
  if (!any(control$verbose, control$verbose_iter)) {
    return(invisible(NULL))
  }

  if (task == "internal") {
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

# Catalog ----------------------------------------------------------------------

tune_env <- rlang::new_environment(
  data = list(
    progress_env = NULL,
    progress_active = FALSE,
    progress_catalog = NULL,
    progress_status_ids = list(),
    progress_modulus = 1L,
    progress_in_unit = 0L
  )
)

# determines whether a currently running tuning process uses the cataloger.
uses_catalog <- function() {
  isTRUE(tune_env$progress_active && !is_testing())
}

# copied from testthat::is_testing
is_testing <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")
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
initialize_catalog <- function(
  control,
  env = rlang::caller_env(),
  workflow = NULL
) {
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

  if (is.null(workflow)) {
    workflow <- workflow()
  }

  if (
    choose_framework(workflow, control) == "sequential" &&
      !is_testing() &&
      !control$verbose
  ) {
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

  rlang::env_bind(tune_env, progress_status_ids = list())
  withr::defer(
    catalog_cleanup(),
    envir = env
  )

  invisible(NULL)
}

catalog_cleanup <- function() {
  ids <- tune_env$progress_status_ids
  catalog <- tune_env$progress_catalog

  if (length(ids) > 0 && !is.null(catalog) && nrow(catalog) > 0) {
    if (cli::is_dynamic_tty()) {
      # Update headers with final counts, then keep as permanent output
      for (i in seq_len(nrow(catalog))) {
        entry_id <- catalog$id[i]
        entry <- ids[[entry_id]]
        if (!is.null(entry)) {
          header <- catalog_header(catalog$type[i], catalog$n[i])
          cli::cli_status_update(id = entry$header, msg = header)
          cli::cli_status_clear(entry$header, result = "clear")
          for (cont_id in entry$continuation) {
            cli::cli_status_clear(cont_id, result = "clear")
          }
        }
      }
    } else {
      # Non-dynamic: print compact summary, then clear silently
      has_duplicates <- any(catalog$n > 1)
      if (has_duplicates) {
        parts <- vapply(
          seq_len(nrow(catalog)),
          function(i) {
            color <- if (catalog$type[i] == "warning") {
              cli::col_yellow
            } else {
              cli::col_red
            }
            color(paste0(catalog$type[i], " (x", catalog$n[i], ")"))
          },
          character(1)
        )
        cli::cli_inform(paste0(
          "Issue totals: ",
          paste(parts, collapse = ", ")
        ))
      }
      suppressMessages(
        for (entry in ids) {
          for (cont_id in rev(entry$continuation)) {
            try(cli::cli_status_clear(cont_id, result = "clear"), silent = TRUE)
          }
          try(
            cli::cli_status_clear(entry$header, result = "clear"),
            silent = TRUE
          )
        }
      )
    }
  }

  rlang::env_bind(tune_env, progress_status_ids = list())
}

# The catalog heartbeat: a single progress bar that sits at the top of the
# progress area and advances once per model fit, so that a tuning run shows it's
# making progress even when no issues are caught. Gated on `uses_catalog()`, so
# it only appears in sequential, non-verbose, non-testing runs.
#
# `total` is the number of model fits across the whole run and `modulus` is the
# number per resample (the per-resample tick budget). We force an initial render
# at creation so the bar claims the top status-bar slot before any issue bar is
# created during the first resample; otherwise issues caught mid-resample would
# render above it. `progress_in_unit` counts ticks within the current resample so
# `catalog_progress_trueup()` can pad the bar up to the next multiple of
# `modulus` when upstream failures cause a resample to fit fewer models than the
# schedule allows for.
catalog_progress_init <- function(total, modulus = 1L) {
  if (!uses_catalog()) {
    return(invisible(NULL))
  }

  rlang::env_bind(tune_env, progress_modulus = modulus, progress_in_unit = 0L)

  rlang::with_options(
    cli::cli_progress_bar(
      total = total,
      format = paste(
        "{cli::pb_spin} tuning {cli::pb_bar}",
        "{cli::pb_current}/{cli::pb_total} {cli::pb_eta_str}"
      ),
      format_done = "{cli::col_green(cli::symbol$tick)} tuning complete [{cli::pb_elapsed}]",
      clear = FALSE,
      .envir = tune_env$progress_env
    ),
    cli.progress_show_after = 0
  )

  cli::cli_progress_update(
    set = 0,
    force = TRUE,
    .envir = tune_env$progress_env
  )

  invisible(NULL)
}

catalog_progress_tick <- function() {
  if (!uses_catalog()) {
    return(invisible(NULL))
  }

  # `force` so the displayed count keeps pace with the issue bars, which repaint
  # on every occurrence; without it the throttled heartbeat lags behind and can
  # show fewer fits than there are issues (#tune-heartbeat).
  cli::cli_progress_update(force = TRUE, .envir = tune_env$progress_env)
  tune_env$progress_in_unit <- tune_env$progress_in_unit + 1L

  invisible(NULL)
}

catalog_progress_trueup <- function() {
  if (!uses_catalog()) {
    return(invisible(NULL))
  }

  pad <- tune_env$progress_modulus - tune_env$progress_in_unit
  if (pad > 0) {
    cli::cli_progress_update(
      inc = pad,
      force = TRUE,
      .envir = tune_env$progress_env
    )
  }
  tune_env$progress_in_unit <- 0L

  invisible(NULL)
}

# The number of model fits the grid loop performs per resample, used as the
# heartbeat's per-resample tick budget. The schedule depends only on the grid
# and workflow (not the split), so it's the same for every resample. Submodel
# parameters share a single fit, so a grid that only tunes submodel parameters
# counts as one fit per resample.
catalog_count_model_iters <- function(grid, workflow) {
  if (is.null(grid) || nrow(grid) == 0) {
    return(1L)
  }

  sched <- schedule_grid(grid, workflow)
  as.integer(sum(vapply(
    sched$model_stage,
    function(m) max(nrow(m), 1L),
    integer(1)
  )))
}

catalog_progress_done <- function() {
  if (!uses_catalog()) {
    return(invisible(NULL))
  }

  cli::cli_progress_done(.envir = tune_env$progress_env)

  invisible(NULL)
}

# catching and logging ---------------------------------------------------------

#' @export
#' @rdname tune-internal-functions
.catch_and_log <- function(
  .expr,
  ...,
  bad_only = FALSE,
  notes,
  catalog = TRUE
) {
  dots <- list(...)
  update_printer(..., type = "info", catalog = catalog, task = dots$location)
  tmp <- catcher(.expr)

  if (has_log_notes(tmp)) {
    # log only the notes from this catch; `catalog_log()` increments counts, so
    # passing the resample's accumulated `notes` would re-count earlier entries
    catalog_log(append_log_notes(NULL, tmp, dots$location))
    notes <- append_log_notes(notes, tmp, dots$location)
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

has_log_notes <- function(x) {
  is_failure(x) || NROW(attr(x, "notes")) > 0
}

is_failure <- function(x) {
  inherits(x, "try-error")
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

      if (uses_catalog() && cli::is_dynamic_tty()) {
        entry_id <- catalog$id[idx]
        ids <- tune_env$progress_status_ids[[entry_id]]
        if (!is.null(ids)) {
          header <- catalog_header(x_type, catalog$n[idx])
          cli::cli_status_update(id = ids$header, msg = header)
        }
      }
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

      if (uses_catalog()) {
        header <- catalog_header(x_type, 1L)
        body_lines <- catalog_body_lines(x_note)

        header_id <- cli::cli_status(
          header,
          msg_done = header,
          .keep = TRUE,
          .auto_close = FALSE
        )
        cont_ids <- vapply(
          body_lines,
          function(line) {
            cli::cli_status(
              line,
              msg_done = line,
              .keep = TRUE,
              .auto_close = FALSE
            )
          },
          character(1)
        )

        tune_env$progress_status_ids[[new_id]] <- list(
          header = header_id,
          continuation = cont_ids
        )
      } else {
        color <- if (x_type == "warning") cli::col_yellow else cli::col_red
        symbol <- if (x_type == "warning") "!" else cli::symbol$cross
        header_text <- paste0(color(symbol), " ", color(x_type), " (x1):")
        body_lines <- catalog_body_lines(x_note)
        cli::cli_alert(paste0(
          header_text,
          "\n",
          paste0(body_lines, collapse = "\n")
        ))
      }
    }
  }

  rlang::env_bind(tune_env, progress_catalog = catalog)

  return(NULL)
}

catalog_header <- function(type, n) {
  color <- if (type == "warning") cli::col_yellow else cli::col_red
  symbol <- if (type == "warning") "!" else cli::symbol$cross
  paste0(color(symbol), " ", color(paste0(type, " (x", n, "):")))
}

catalog_body_lines <- function(note) {
  indent <- 2L
  width <- cli::console_width() - indent
  lines <- strsplit(note, "\n")[[1]]
  wrapped <- unlist(lapply(lines, function(line) {
    strwrap(line, width = width)
  }))
  paste0(strrep(" ", indent), wrapped)
}

remove_log_notes <- function(x) {
  attr(x, "notes") <- NULL
  x
}

# Bayes specific printing ------------------------------------------------------

log_best <- function(control, iter, info, digits = 4) {
  if (!isTRUE(control$verbose_iter)) {
    return(invisible(NULL))
  }

  message("")
  message(cli::rule(left = cli::style_bold(paste("Iteration", iter))))
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
  update_printer(
    control,
    split_labels = NULL,
    task = msg,
    type = "info",
    catalog = FALSE
  )
}

check_and_log_flow <- function(control, results) {
  if (!isTRUE(control$verbose_iter)) {
    return(invisible(NULL))
  }

  if (all(is.na(results$.mean))) {
    if (nrow(results) < 2) {
      update_printer(
        control,
        split_labels = NULL,
        task = "Halting search",
        type = "danger",
        catalog = FALSE
      )
      eval.parent(parse(text = "break"))
    } else {
      update_printer(
        control,
        split_labels = NULL,
        task = "Skipping to next iteration",
        type = "danger",
        catalog = FALSE
      )
      eval.parent(parse(text = "next"))
    }
  }
  invisible(NULL)
}

log_progress <- function(
  control,
  x,
  maximize = TRUE,
  objective = NULL,
  eval_time = NULL,
  digits = 4
) {
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
    msg <- paste0(cli::col_red(cli::symbol$heart), msg)
  } else {
    msg <- paste0(cli::col_silver(cli::symbol$circle_cross), msg)
  }
  message(msg)
}

param_msg <- function(control, candidate) {
  if (!isTRUE(control$verbose_iter)) {
    return(invisible(NULL))
  }
  candidate <- candidate[,
    !(names(candidate) %in% c(".mean", ".sd", "objective"))
  ]
  p_chr <- paste0(
    names(candidate),
    "=",
    format(as.data.frame(candidate), digits = 3)
  )
  p_chr <- paste0(p_chr, collapse = ", ")
  message_wrap(
    p_chr,
    prefix = "i",
    color_text = get_tune_colors()$message$info,
    color_prefix = get_tune_colors()$symbol$info
  )
  invisible(NULL)
}

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
  function(
    x,
    width = options()$width - 2,
    prefix = "",
    color_text = NULL,
    color_prefix = color_text
  ) {
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
      msg <- purrr::map_chr(msg, color_text)
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


acq_summarizer <- function(control, iter, objective = NULL, digits = 4) {
  if (!isTRUE(control$verbose_iter)) {
    return(invisible(NULL))
  }
  if (inherits(objective, "conf_bound") && is.function(objective$kappa)) {
    val <- paste0(
      "Kappa value: ",
      signif(objective$kappa(iter), digits = digits)
    )
  } else {
    if (
      inherits(objective, c("exp_improve", "prob_improve")) &&
        is.function(objective$trade_off)
    ) {
      val <- paste0(
        "Trade-off value: ",
        signif(objective$trade_off(iter), digits = digits)
      )
    } else {
      val <- NULL
    }
  }
  if (!is.null(val)) {
    update_printer(
      control,
      split_labels = NULL,
      task = val,
      type = "info",
      catalog = FALSE
    )
  }
  invisible(NULL)
}

# End --------------------------------------------------------------------------
