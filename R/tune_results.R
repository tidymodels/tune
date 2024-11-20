#' @export
print.tune_results <- function(x, ...) {
  cl <- match.call()
  if (inherits(x, "resample_results")) {
    cat("# Resampling results\n")
  } else {
    cat("# Tuning results\n")
  }

  att <- attributes(x)
  rset_info <- att$rset_info

  if (is.null(rset_info)) {
    print_compat_tune_results_label(x)
  } else {
    cat("#", rset_info$label, "\n")
  }

  print(tibble::as_tibble(x), ...)

  summarize_notes(x)
}

# `tune_results` have been changed to no longer inherit from `rset`,
# and should instead use the `rset_info` attribute. This code
# ensures that printing still works on old versions of `tune_results`
# that might have been saved to disk and then loaded back up with
# a new version of tune.
print_compat_tune_results_label <- function(x) {
  # Somehow we don't have the `rset_info` attribute, but this isn't
  # an rset subclass. Just don't print a label to avoid erroring.
  if (!inherits(x, "rset")) {
    return()
  }

  label <- try(pretty(x), silent = TRUE)

  # Somehow the rset `pretty()` method failed.
  # Just don't print a label to avoid erroring.
  if (inherits(label, "try-error")) {
    return()
  }

  cat("#", label, "\n")
}


has_notes <- function(x) {
  if (is.null(x)) {
    return(0L)
  }
  nrow(x)
}

summarize_notes <- function(x) {
  num_notes <- sum(purrr::map_int(x$.notes, has_notes))
  if (num_notes == 0) {
    return(invisible(NULL))
  }
  notes <-
    x %>%
    dplyr::select(dplyr::starts_with("id"), .notes) %>%
    tidyr::unnest(cols = .notes)
  by_type <-
    notes %>%
    dplyr::group_nest(type) %>%
    dplyr::mutate(data = purrr::map(data, ~ dplyr::count(.x, note))) %>%
    tidyr::unnest(data) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      note = gsub("(Error:)", "", note),
      note = glue::glue_collapse(note, width = 0.85 * getOption("width")),
      note = gsub("\n", " ", note, fixed = TRUE),
      pre = ifelse(type == "error", "  - Error(s) x", "  - Warning(s) x"),
      note = paste0(pre, n, ": ", note)
    )
  cat("\nThere were issues with some computations:\n\n")
  cat(by_type$note, sep = "\n")
  cat("\nRun `show_notes(.Last.tune.result)` for more information.\n")
  invisible(NULL)
}


# ------------------------------------------------------------------------------

#' @export
`[.tune_results` <- function(x, i, j, ...) {
  out <- NextMethod()
  tune_results_reconstruct(out, x)
}

#' @export
`names<-.tune_results` <- function(x, value) {
  out <- NextMethod()
  tune_results_reconstruct(out, x)
}

# ------------------------------------------------------------------------------

new_tune_results <-
  function(x,
           parameters,
           metrics,
           eval_time,
           eval_time_target,
           outcomes = character(0),
           rset_info,
           ...,
           class = character()) {
    new_bare_tibble(
      x = x,
      parameters = parameters,
      metrics = metrics,
      eval_time = eval_time,
      eval_time_target = eval_time_target,
      outcomes = outcomes,
      rset_info = rset_info,
      ...,
      class = c(class, "tune_results")
    )
  }

is_tune_results <- function(x) {
  inherits(x, "tune_results")
}

peek_tune_results_outcomes <- function(x) {
  if (!is_tune_results(x)) {
    cli::cli_abort("Internal error: {.arg outcomes} can only be extracted from
                   {.cls tune_results}.")
  }

  out <- attr(x, "outcomes", exact = TRUE)

  if (is.null(out)) {
    cli::cli_abort("The object of type {.cls tune_results} doesn't have an
                    {.code outcomes} attribute.")
  }

  out
}

# ------------------------------------------------------------------------------

#' Display distinct errors from tune objects
#' @param x An object of class `tune_results`.
#' @param n An integer for how many unique notes to show.
#' @return Invisibly, `x`. Function is called for side-effects and printing.
#' @export
show_notes <- function(x, n = 10) {
  res <-
    collect_notes(x) %>%
    dplyr::distinct(type, note)

  if (nrow(res) == 0) {
    cat("Great job! No notes to show.\n")
    return(invisible(x))
  }

  n <- min(nrow(res), n)
  notes <- res$note[1:n]

  msg <- "unique notes:\n"
  if (n != nrow(res)) {
    msg <- paste0("first ", n, msg)
  }

  sub_notes <- strsplit(notes, split = "\n")[[1]]
  max_width <- max(purrr::map_int(sub_notes, nchar))
  max_width <- min(max_width, cli::console_width())

  notes <-  paste(cli::rule(width = max_width), notes, sep = "\n")
  notes <-  paste0(notes, "\n")
  cat(msg)
  cat(notes, sep = "")
  invisible(x)
}

