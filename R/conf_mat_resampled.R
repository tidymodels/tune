#' Compute average confusion matrix across resamples
#'
#' For classification problems, `conf_mat_resampled()` computes a separate
#' confusion matrix for each resample then averages the cell counts.
#'
#' @param x An object with class `tune_results` that was used with a
#' classification model that was run with `control_*(save_pred = TRUE)`.
#' @param parameters A tibble with a single tuning parameter combination. Only
#' one tuning parameter combination (if any were used) is allowed here.
#' @param tidy Should the results come back in a tibble (`TRUE`) or a matrix.
#' @return A tibble or matrix with the average cell count across resamples.
#' @examples
#' library(parsnip)
#' library(rsample)
#' library(dplyr)
#'
#' data(two_class_dat, package = "modeldata")
#'
#' set.seed(2393)
#' res <-
#'   logistic_reg() %>%
#'   set_engine("glm") %>%
#'   fit_resamples(Class ~ ., resamples = vfold_cv(two_class_dat, v = 3),
#'                 control = control_resamples(save_pred = TRUE))
#'
#' conf_mat_resampled(res)
#' conf_mat_resampled(res, tidy = FALSE)
#' @export
conf_mat_resampled <- function(x, parameters = NULL, tidy = TRUE) {
  if (!inherits(x, "tune_results")) {
    rlang::abort(
      "The first argument needs to be an object with class 'tune_results'."
    )
  }
  if (!any(names(x) == ".predictions")) {
    rlang::abort(
      paste0(
        "The function was not run with the `save_pred = TRUE` option. ",
        "Please re-run with that option."
      )
    )
  }
  preds <- collect_predictions(x, summarize = FALSE, parameters = parameters)

  if (!any(names(preds) == ".pred_class")) {
    rlang::abort("Cannot find the predicted classes. Was this a classification model?")
  }
  # check for multiple parameter combinations
  params <- .get_tune_parameter_names(x)
  if (length(params) > 0) {
    param_combos <-
      preds %>%
      dplyr::select(!!!params) %>%
      distinct()
    if (nrow(param_combos) > 1) {
      rlang::abort(
        paste0(
          "It looks like there are ", nrow(param_combos), " tuning parameter ",
          "combination(s) in the data. Pleasse use the `parameters` ",
          "argument to select one combination of parameters."
        )
      )
    }
  }

  truth <- .get_tune_outcome_names(x)
  if (length(truth) != 1) {
    rlang::abort("Cannot determine the proper outcome name")
  }

  id_cols <- grep("(^id$)|($id[1-9]$)", names(preds), value = TRUE)
  preds <-
    preds %>%
    dplyr::group_nest(!!!syms(id_cols)) %>%
    dplyr::mutate(
      conf_mats =
        purrr::map(data, ~ yardstick::conf_mat(.x, truth = {{truth}}, estimate = .pred_class))
    )

  res <-
    purrr::map_dfr(preds$conf_mats, ~ as.data.frame(.x$table)) %>%
    dplyr::group_by(Prediction, Truth)

  if (utils::packageVersion("dplyr") <= "0.8.5") {
    res <-
      res %>%
      dplyr::summarize(Freq = mean(Freq, na.rm = TRUE), .groups = "keep") %>%
      dplyr::ungroup()
  } else {
    res <-
      res %>%
      dplyr::summarize(Freq = mean(Freq, na.rm = TRUE)) %>%
      dplyr::ungroup()
  }

  if (!tidy) {
    lvls <- levels(res$Prediction)
    res <- matrix(res$Freq, ncol = length(lvls))
    colnames(res) <- lvls
    rownames(res) <- lvls
  }
  res
}

