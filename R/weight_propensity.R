#' Helper for bridging two-stage causal fits
#'
#' @description
#' `weight_propensity()` is a helper function to more easily link the
#' propensity and outcome models in causal workflows. In the case of a
#' single model fit, as with `model_fit`s or `workflow`s, the function
#' is roughly analogous to an `augment()` method that additionally takes in
#' a propensity weighting function. For `tune_results`, the method carries
#' out this same augment-adjacent procedure on the training data underlying
#' the resampling object for each element of the analysis set.
#'
#' @inheritParams parsnip::weight_propensity.model_fit
#'
#' @inherit parsnip::weight_propensity.model_fit return
#'
#' @inherit parsnip::weight_propensity.model_fit references
#'
#' @examplesIf tune:::should_run_examples(suggests = "modeldata")
#' # load needed packages
#' library(modeldata)
#' library(parsnip)
#' library(workflows)
#' library(rsample)
#'
#' library(ggplot2)
#' library(dplyr)
#' library(purrr)
#'
#' # example data: model causal estimate for `Class`
#' two_class_dat <- two_class_dat[1:250,]
#' two_class_dat
#'
#' # see `propensity::wt_ate()` for a more realistic example
#' # of a propensity weighting function
#' silly_wt_fn <- function(.propensity, .exposure = NULL, ...) {
#'   .propensity
#' }
#'
#' propensity_wf <- workflow(Class ~ B, logistic_reg())
#' outcome_wf <- workflow(A ~ Class, linear_reg()) %>% add_case_weights(.wts)
#'
#' # single model --------------------------------------------------------------
#' propensity_fit <- fit(propensity_wf, two_class_dat)
#'
#' two_class_weighted <-
#'   weight_propensity(propensity_fit, silly_wt_fn, data = two_class_dat)
#'
#' two_class_weighted
#'
#' outcome_fit <- fit(outcome_wf, two_class_weighted)
#'
#' outcome_fit %>% extract_fit_engine() %>% coef()
#'
#' # resampled model -----------------------------------------------------------
#' set.seed(1)
#' boots <- bootstraps(two_class_dat[1:250,], times = 100)
#'
#' res_tm <-
#'   # fit the propensity model to resamples
#'   fit_resamples(
#'     propensity_wf,
#'     resamples = boots,
#'     # note `extract = identity` rather than `extract`
#'     control = control_resamples(extract = identity)
#'   ) %>%
#'   # determine weights for outcome model based on
#'   # propensity model's predictions
#'   weight_propensity(silly_wt_fn) %>%
#'   # fit outcome workflow using generated `.wts`
#'   fit_resamples(
#'     outcome_wf,
#'     resamples = .,
#'     # would usually `extract = tidy` here
#'     control = control_resamples(extract = identity)
#'   )
#'
#' # extracts contain the properly resampled fitted workflows:
#' collect_extracts(res_tm)
#'
#' # plot the properly resampled distribution of estimates:
#' collect_extracts(res_tm) %>%
#'   pull(.extracts) %>%
#'   map(extract_fit_engine) %>%
#'   map(coef) %>%
#'   bind_rows() %>%
#'   ggplot() +
#'   aes(x = ClassClass2) +
#'   geom_histogram()
#'
#' @name weight_propensity
#' @aliases weight_propensity.tune_results
#' @importFrom parsnip weight_propensity
#' @method weight_propensity tune_results
#' @export
weight_propensity.tune_results <- function(object, wt_fn, ...) {
  if (rlang::is_missing(wt_fn) || !is.function(wt_fn)) {
    cli::cli_abort("{.arg wt_fn} must be a function.")
  }

  wf_1 <- purrr::pluck(object, ".extracts", 1, ".extracts", 1)
  if (!inherits(wf_1, "workflow")) {
    cli::cli_abort(
      "{.arg object} must have been generated with the \\
       {.help [control option](tune::control_grid)} {.code extract = identity}."
    )
  }

  dots <- rlang::list2(...)
  if ("data" %in% names(dots)) {
    cli::cli_abort(
      "The {.cls tune_results} method for {.fn weight_propensity} does not take \\
       a {.arg data} argument, but one was supplied."
    )
  }

  for (resample in seq_along(object$splits)) {
    object$splits[[resample]] <-
      augment_split(
        object$splits[[resample]],
        object$.extracts[[resample]]$.extracts[[1]],
        wt_fn = wt_fn,
        ...
      )
  }

  tibble::new_tibble(
    object[, c("splits", "id")],
    !!!attr(object, "rset_info")$att,
    class = c(attr(object, "rset_info")$att$class, "rset")
  )
}

augment_split <- function(split, workflow, wt_fn, ...) {
  split[["data"]]$..id <- seq_along(split[["data"]][[1]])
  d <- rsample::analysis(split)
  d <- vctrs::vec_slice(d, !duplicated(d$..id))
  d <- weight_propensity(workflow, wt_fn, ..., data = d)

  split[["data"]][d$..id, ".wts"] <- d$.wts
  split[["data"]]$..id <- NULL

  split
}
