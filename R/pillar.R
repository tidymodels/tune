
#' @export
obj_sum.tune_results <- function(x) {
  null_metrics <- purrr::map_lgl(x$.metrics, is.null)
  if (!all(null_metrics)) {
    res <- "tune[+]"
  } else {
    res <- "tune[x]"
  }
  res
}

#' Helpers for pillar formatting
#' @param x an object
#' @return A character string.
#' @export
#' @keywords internal
#' @rdname pillar-helpers
size_sum.tune_results <- function(x) {
  ""
}


#' @export
obj_sum.resample_results <- function(x) {
  null_metrics <- purrr::map_lgl(x$.metrics, is.null)
  if (!all(null_metrics)) {
    res <- "rsmp[+]"
  } else {
    res <- "rsmp[x]"
  }
  res
}

#' @export
#' @keywords internal
#' @rdname pillar-helpers
size_sum.resample_results <- function(x) {
  ""
}


newer_tibble <- function() {
  utils::packageVersion("tibble") >= "3.0.6"
}
