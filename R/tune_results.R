#' @export
print.tune_results <- function(x, ...) {
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

summarize_notes <- function(x) {
  num_notes <- sum(purrr::map_int(x$.notes, nrow))
  if (num_notes == 0) {
    return(invivible(NULL))
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
    dplyr::mutate(
      note = gsub("(Error:)", "", note),
      note = substr(note, 1, max_note_nchar),
      note = gsub("\n", " ", note, fixed = TRUE),
      pre = ifelse(type == "error", "Error(s) x", "Warning(s) x"),
      note = paste0(pre, n, ": ", note)
      )
  by_type
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

new_tune_results <- function(x, parameters, metrics, outcomes = character(0), rset_info, ..., class = character()) {
  new_bare_tibble(
    x = x,
    parameters = parameters,
    metrics = metrics,
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
    rlang::abort("Internal error: `outcomes` can only be extracted from 'tune_results'.")
  }

  out <- attr(x, "outcomes", exact = TRUE)

  if (is.null(out)) {
    rlang::abort("'tune_results' object doesn't have an 'outcomes' attribute.")
  }

  out
}
