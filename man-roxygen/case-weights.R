#' @section Case Weights:
#' Some models can utilize case weights during training. tidymodels currently
#' supports two types of case weights: importance weights (doubles) and
#' frequency weights (integers). Frequency weights are used during model
#' fitting and evaluation, whereas importance weights are only used during
#' fitting.
#'
#' To know if your model is capable of using case weights, create a model spec
#' and test it using [parsnip::case_weights_allowed()].
#'
#' To use them, you will need a numeric column in your data set that has been
#' passed through either [hardhat:: importance_weights()] or
#' [hardhat::frequency_weights()].
#'
#' For functions such as [fit_resamples()] and the `tune_*()` functions, the
#' model must be contained inside of a [workflows::workflow()]. To declare that
#' case weights are used, invoke [workflows::add_case_weights()] with the
#' corresponding (unquoted) column name.
#'
#' From there, the packages will appropriately handle the weights during model
#' fitting and (if appropriate) performance estimation.
#'
