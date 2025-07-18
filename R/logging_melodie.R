.catch_and_log_melodie <- function(.expr) {
  tmp <- catcher_melodie(.expr)
  tmp
}

catcher_melodie <- function(expr) {
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

is_failure_melodie <- function(x) {
  inherits(x, "try-error")
}

has_log_notes <- function(x) {
  is_failure_melodie(x) || NROW(attr(x, "notes")) > 0
}

append_log_notes <- function(notes, x, location) {
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

  if (is_failure_melodie(x)) {
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

melodie_env <-
  rlang::new_environment(
    data = list(
      progress_env = NULL,
      progress_active = FALSE,
      progress_catalog = NULL,
      progress_started = FALSE
    )
  )

lbls_melodie <- c(LETTERS, letters, 1:1e3)

# determines whether a currently running tuning process uses the cataloger.
uses_catalog <- function() {
  isTRUE(melodie_env$progress_active && !is_testing())
}

# copied from testthat::is_testing
is_testing <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")
}

catalog_log <- function(x) {
  catalog <- rlang::env_get(melodie_env, "progress_catalog")

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
        "{color(cli::style_bold(lbls_melodie[new_id]))} | {color(x_type)}: {note}"
      )
      cli::cli_alert(msg)
    }
  }

  rlang::env_bind(melodie_env, progress_catalog = catalog)
  rlang::env_bind(
    melodie_env$progress_env,
    catalog_summary = summarize_catalog_melodie(catalog)
  )

  if (uses_catalog()) {
    if (!melodie_env$progress_started) {

      rlang::with_options(
        cli::cli_progress_bar(
          type = "custom",
          format = "There were issues with some computations   {catalog_summary}",
          clear = FALSE,
          .envir = melodie_env$progress_env
        ),
        cli.progress_show_after = 0
      )
      rlang::env_bind(melodie_env, progress_started = TRUE)
    }


    cli::cli_progress_update(.envir = melodie_env$progress_env)
  }

  return(NULL)
}

# given a catalog, summarize errors and warnings in a 1-length glue vector.
# for use by the progress bar inside of `tune_catalog()`.
summarize_catalog_melodie <- function(catalog, sep = "   ") {
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
    msg = glue::glue("{color(cli::style_bold(lbls_melodie[id]))}: x{n}")
  )
  res <- dplyr::ungroup(res)
  res <- dplyr::pull(res, msg)
  res <- glue::glue_collapse(res, sep = sep)

  res
}

catalog_is_active_melodie <- function() {
  melodie_env$progress_active
}

initialize_catalog_melodie <- function(control, env = rlang::caller_env()) {
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


  rlang::env_bind(melodie_env, progress_env = env)

  rlang::env_bind(melodie_env, progress_catalog = catalog)
  withr::defer(
    rlang::env_bind(melodie_env, progress_catalog = NULL),
    envir = env
  )

  rlang::env_bind(melodie_env, progress_active = progress_active)
  withr::defer(
    rlang::env_bind(melodie_env, progress_active = FALSE),
    envir = env
  )
  withr::defer(
    rlang::env_bind(melodie_env, progress_started = FALSE),
    envir = env
  )

  invisible(NULL)
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
