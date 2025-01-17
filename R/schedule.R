#' Get tune schedule
#'
#' @param wflow A workflow object.
#' @param param A dials parameters set.
#' @param grid A tibble containing the parameter grid.
#'
#' @returns A schedule object, inheriting from either 'single_schedule',
#' 'grid_schedule', or 'resample_schedule'.
#'
#' @export
get_tune_schedule <- function(wflow, param, grid) {
	if (!inherits(wflow, "workflow")) {
		cli::cli_abort("Argument {.arg wflow} must be a workflow object.")
	}

	if (!inherits(param, "parameters")) {
		cli::cli_abort("Argument {.arg param} must be a dials parameters set.")
	}

	if (!tibble::is_tibble(grid)) {
		cli::cli_abort("Argument {.arg grid} must be a tibble.")
	}

	# Which parameter belongs to which stage and which is a submodel parameter?
	param_info <- get_param_info(wflow)

	schedule <- schedule_stages(grid, param_info, wflow)

	og_cls <- class(schedule)
	if (nrow(param) == 0) {
		cls <- "resample_schedule"
	} else {
		cls <- "grid_schedule"
	}

	if (nrow(grid) == 1) {
		cls <- c("single_schedule", cls)
	}

	class(schedule) <- c(cls, "schedule", og_cls)

	schedule
}

schedule_stages <- function(grid, param_info, wflow) {
	# schedule preprocessing stage and push the rest into a nested tibble
	param_pre_stage <- param_info %>% 
    filter(source == "recipe") %>% 
    pull(id)
	schedule <- grid %>% 
    tidyr::nest(.by = all_of(param_pre_stage), .key = "model_stage")

	# schedule next stages recursively
	schedule %>% 
		mutate(
      model_stage = 
        purrr::map(
					model_stage,
					schedule_model_stage_i, 
					param_info = param_info,
					wflow = wflow
				)
    )
}

schedule_model_stage_i <- function(model_stage, param_info, wflow){
  model_param <- param_info %>% 
    filter(source == "model_spec") %>% 
    pull(id)
  non_submodel_param <- param_info %>% 
    filter(source == "model_spec" & !has_submodel) %>% 
    pull(id)
  
  # schedule model parameters
  schedule <- min_model_grid(model_stage, model_param, wflow)

  # push remaining paramters into the next stage
  next_stage <- model_stage %>% 
    tidyr::nest(.by = all_of(non_submodel_param), .key = "predict_stage")

  schedule <- schedule %>% 
    dplyr::left_join(next_stage, by = all_of(non_submodel_param))

	# schedule next stages recursively
	schedule %>% 
		mutate(
      predict_stage = 
        purrr::map(predict_stage, schedule_predict_stage_i, param_info = param_info)
    )
}

min_model_grid <- function(grid, model_param, wflow){
  # work on only the model parameters
  model_grid <- grid %>% 
    select(all_of(model_param)) %>% 
    dplyr::distinct()

  min_grid(
    extract_spec_parsnip(wflow),
    model_grid
  ) %>% 
    select(all_of(model_param))
}

schedule_predict_stage_i <- function(predict_stage, param_info) {
  submodel_param <- param_info %>% 
    filter(source == "model_spec" & has_submodel) %>% 
    pull(id)

  predict_stage %>% 
    tidyr::nest(.by = all_of(submodel_param), .key = "post_stage")
}

# TODO check if existing tune functionality already covers this
get_param_info <- function(wflow) {
  param_info <- tune_args(wflow) %>% 
		select(name, id, source)

  model_spec <- extract_spec_parsnip(wflow)
	model_type <- class(model_spec)[1]
	model_eng <- model_spec$engine

	model_param <- parsnip::get_from_env(paste0(model_type, "_args")) %>%
		dplyr::filter(engine == model_spec$engine) %>%
		dplyr::select(name = parsnip, has_submodel)

	param_info <- dplyr::left_join(param_info, model_param, by = "name")
  
  param_info
}
