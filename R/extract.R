
extract_details <- function(object, extractor) {
  if (is.null(extractor)) {
    return(list())
  }
  try(extractor(object), silent = TRUE)
}

pull_metrics <- function(rs, res) {
  rs %>%
    dplyr::full_join(
      purrr::map_dfr(res, ~.x$.metrics) %>% tidyr::nest(-starts_with("id"), .key = .metrics),
      by = names(labels(rs$splits[[1]]))
    )
}

pull_extracts <- function(rs, res, control) {
  if (!is.null(control$extract)) {
    rs <-
      rs %>%
      dplyr::full_join(
        purrr::map_dfr(res, ~.x$.extract) %>% tidyr::nest(-starts_with("id"), .key = .extract),
        by = names(labels(rs$splits[[1]]))
      )
  }
  rs
}

append_metrics <- function(collection, predictions, workflow, perf, split) {
  tmp_est <- estimate_perf(predictions, perf, workflow)
  tmp_est <- cbind(tmp_est, labels(split))
  dplyr::bind_rows(collection, tmp_est)
}

