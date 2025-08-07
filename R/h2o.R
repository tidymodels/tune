# tune creates the schedule object. With h2o, we need to determine the model
# and post candidates (if any). The model parameters should be converted into
# a tibble along with post_stage that contains any tuning parameter info for
# the postprocessors. This has to be done for each preprocessing tuning parameter
# combination (if any). Note that the predict_stage is not needed for h2o so
# we need to handle submodel information that was in the predict_state and
# return it to the model grid.

submodel_names <- function(x) names(x)[names(x) != "post_stage"]

late_stage_grid <- function(sched_pre_iter) {

  if (nrow(sched_pre_iter) == 0) {
    return(tibble::tibble(post_stage = list()))
  }

  if (!any(names(sched_pre_iter) == "model_stage")) {
    sched_pre_iter$post_stage <- list(tibble::tibble())
    return(sched_pre_iter)
  }

  res <-
    sched_pre_iter |>
    dplyr::select(model_stage) |>
    tidyr::unnest(model_stage)

  submodel_param <- unique(unlist(purrr::map(res$predict_stage, submodel_names)))

  if (any(names(res) == submodel_param)) {
    res[[submodel_param]] <- NULL
  }

  tidyr::unnest(res, predict_stage)
}

