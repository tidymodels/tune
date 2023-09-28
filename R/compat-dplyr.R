#' @importFrom dplyr dplyr_reconstruct
dplyr::dplyr_reconstruct

#' @noRd
#' @export
dplyr_reconstruct.tune_results <- function(data, template) {
  tune_results_reconstruct(data, template)
}

#' @noRd
#' @export
dplyr_reconstruct.resample_results <- function(data, template) {
  resample_results_reconstruct(data, template)
}

#' @noRd
#' @export
dplyr_reconstruct.iteration_results <- function(data, template) {
  iteration_results_reconstruct(data, template)
}
