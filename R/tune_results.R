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
    warning(print_notes, call. = FALSE)
  }
  class(x) <- class(x)[!(class(x) %in% c("tune_results"))]
  print(x, ...)
}
