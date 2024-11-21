#' Compute average confusion matrix across resamples
#'
#' For classification problems, `conf_mat_resampled()` computes a separate
#' confusion matrix for each resample then averages the cell counts.
#'
#' @param x An object with class `tune_results` that was used with a
#' classification model that was run with `control_*(save_pred = TRUE)`.
#' @param ... Currently unused, must be empty.
#' @param parameters A tibble with a single tuning parameter combination. Only
#' one tuning parameter combination (if any were used) is allowed here.
#' @param tidy Should the results come back in a tibble (`TRUE`) or a `conf_mat`
#' object like `yardstick::conf_mat()` (`FALSE`)?
#' @return A tibble or `conf_mat` with the average cell count across resamples.
#' @examplesIf rlang::is_installed("modeldata")
#' # example code
#'
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
#'   fit_resamples(
#'     Class ~ .,
#'     resamples = vfold_cv(two_class_dat, v = 3),
#'     control = control_resamples(save_pred = TRUE)
#'   )
#'
#' conf_mat_resampled(res)
#' conf_mat_resampled(res, tidy = FALSE)
#' @export
conf_mat_resampled <- function(x, ..., parameters = NULL, tidy = TRUE) {
  rlang::check_dots_empty()
  if (!inherits(x, "tune_results")) {
    cli::cli_abort("The first argument needs to be {.cls tune_results} object,
                   not {.obj_type_friendly {mtcars}}.")
  }
  if (!any(names(x) == ".predictions")) {
    cli::cli_abort(
      "The function was not run with the {.code save_pred = TRUE} option.
       Please re-run with that option."
    )
  }
  preds <- collect_predictions(x, summarize = FALSE, parameters = parameters)

  if (!any(names(preds) == ".pred_class")) {
    cli::cli_abort(
      "Cannot find the predicted classes. Was this a classification model?"
    )
  }
  # check for multiple parameter combinations
  params <- .get_tune_parameter_names(x)
  if (length(params) > 0) {
    param_combos <-
      preds %>%
      dplyr::select(!!!params) %>%
      distinct()
    if (nrow(param_combos) > 1) {
      cli::cli_abort(
        "It looks like there are {nrow(param_combos)} tuning parameter
        combination in the data. Please use the {.arg parameters}
        argument to select one combination of parameters."
      )
    }
  }

  truth <- .get_tune_outcome_names(x)
  if (length(truth) != 1) {
    cli::cli_abort("Cannot determine the proper outcome name")
  }

  id_cols <- grep("(^id$)|($id[1-9]$)", names(preds), value = TRUE)
  preds <-
    preds %>%
    dplyr::group_nest(!!!syms(id_cols)) %>%
    dplyr::mutate(
      conf_mats =
        purrr::map(data, ~ yardstick::conf_mat(.x, truth = {{ truth }}, estimate = .pred_class))
    )

  opt <- getOption("dplyr.summarise.inform", default = "FALSE")
  options(dplyr.summarise.inform = FALSE)

  res <-
    purrr::map(preds$conf_mats, ~ as.data.frame(.x$table)) %>%
    purrr::list_rbind() %>%
    dplyr::group_by(Prediction, Truth) %>%
    dplyr::summarize(Freq = mean(Freq, na.rm = TRUE)) %>%
    dplyr::ungroup()

  options(dplyr.summarise.inform = opt)

  if (!tidy) {
    lvls <- levels(res$Prediction)
    res <- matrix(res$Freq, ncol = length(lvls), byrow = TRUE)
    colnames(res) <- lvls
    rownames(res) <- lvls
    res <- as.table(res) %>% yardstick::conf_mat()
  }
  res
}
