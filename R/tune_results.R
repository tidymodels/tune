#' @export
print.tune_results <- function(x, ...) {
  total_notes <- sum(purrr::map_int(x$.notes, nrow)) > 0
  if (total_notes > 0) {
    max_note_nchar <- 500
    note_samples <-
      x %>%
      dplyr::select(.notes) %>%
      tidyr::unnest(.notes) %>%
      dplyr::mutate(
        note = dplyr::row_number(),
        .notes = substr(.notes, 1, max_note_nchar)
      )

    note_samples <-
      note_samples %>%
      dplyr::sample_n(min(3, nrow(note_samples))) %>%
      dplyr::arrange(note) %>%
      dplyr::pull(.notes)

    print_notes <- glue::glue("This tuning result has notes. ",
                              "Example notes on model fitting include:\n",
                              glue::glue_collapse(note_samples, sep = "\n"))
    rlang::warn(print_notes)
  }

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
