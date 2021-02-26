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

# also used for resample_results
#' @export
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
obj_sum.tune_race <- function(x) {
  null_metrics <- purrr::map_lgl(x$.metrics, is.null)
  if (!all(null_metrics)) {
    res <- "race[+]"
  } else {
    res <- "race[x]"
  }
  res
}
