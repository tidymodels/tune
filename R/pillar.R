
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

# @export - lazy in .onLoad()
# also used for resample_results
size_sum_tune_results <- function(x) {
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

newer_tibble <- function() {
  utils::packageVersion("tibble") >= "3.0.6"
}
