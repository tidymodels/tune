#' @export
print.tune_results <- function(x, ...) {
  total_notes <- sum(purrr::map_int(x$.notes, nrow)) > 0
  if (total_notes > 0) {
    print_notes <- glue::glue("This tuning result has notes.\n",
                              "Example notes on model fitting include:\n",
                              glue::glue_collapse(
                                x %>%
                                  dplyr::select(.notes) %>%
                                  tidyr::unnest(.notes) %>%
                                  dplyr::mutate(note = dplyr::row_number()) %>%
                                  dplyr::sample_n(3) %>%
                                  dplyr::arrange(note) %>%
                                  dplyr::pull(.notes),
                                sep = "\n"))
    rlang::warn(print_notes)
  }

  if (inherits(x, "resample_results")) {
    cat("# Resampling results\n")
  } else {
    cat("# Tuning results\n")
  }

  att <- attributes(x)
  if (any(names(att) == "rset_info")) {
    cat("#", att$rset_info$label, "\n")
  } else {
    if (inherits(x, "rset")) {
      rset_info <- try(pretty(x), silent = TRUE)
      if (!inherits(rset_info, "try-error")) {
        cat("#", rset_info, "\n")
      }
    }
  }

  print(tibble::as_tibble(x), ...)
}
