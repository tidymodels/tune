#' Schedule a grid
#'
#' @param grid A tibble containing the parameter grid.
#' @param wflow The workflow object for which we schedule the grid.
#'
#' @returns A schedule object, inheriting from either 'single_schedule',
#' 'grid_schedule', or 'resample_schedule'.
#'
#' @keywords internal
#' @export
schedule_grid <- function(grid, wflow) {
	if (!tibble::is_tibble(grid)) {
		cli::cli_abort(
			"Argument {.arg grid} must be a tibble, not {.obj_type_friendly {grid}}."
		)
	}
	if (!inherits(wflow, "workflow")) {
		cli::cli_abort(
			"Argument {.arg wflow} must be a workflow object, not {.obj_type_friendly {wflow}}."
		)
	}

	schedule <- schedule_stages(grid, wflow)

	param_info <- tune_args(wflow)

	og_cls <- class(schedule)
	if (nrow(param_info) == 0) {
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

schedule_stages <- function(grid, wflow) {
	# Which parameter belongs to which stage and which is a submodel parameter?
	param_info <- get_param_info(wflow)

	# schedule preprocessing stage and push the rest into a nested tibble
	param_pre_stage <- param_info %>%
		dplyr::filter(source == "recipe") %>%
		dplyr::pull(id)
	schedule <- grid %>%
		tidyr::nest(.by = dplyr::all_of(param_pre_stage), .key = "model_stage")

	# schedule next stages nested within `schedule_model_stage_i()`
	schedule %>%
		dplyr::mutate(
			model_stage = purrr::map(
				model_stage,
				schedule_model_stage_i,
				param_info = param_info,
				wflow = wflow
			)
		)
}

schedule_model_stage_i <- function(model_stage, param_info, wflow) {
	model_param <- param_info %>%
		dplyr::filter(source == "model_spec") %>%
		dplyr::pull(id)
	non_submodel_param <- param_info %>%
		dplyr::filter(source == "model_spec" & !has_submodel) %>%
		dplyr::pull(id)

	any_non_submodel_param <- length(non_submodel_param) > 0

	# schedule model parameters
	schedule <- min_model_grid(model_stage, model_param, wflow)

	# push remaining parameters into the next stage
	next_stage <- model_stage %>%
		tidyr::nest(
			.by = dplyr::all_of(non_submodel_param),
			.key = "predict_stage"
		)

	if (any_non_submodel_param) {
		# min_model_grid() may change the row order, thus use next_stage as the
		# "left" data frame here to preserve the original row order
		schedule <- next_stage %>%
			dplyr::left_join(schedule, by = non_submodel_param) %>%
			dplyr::relocate(dplyr::all_of(model_param))
	} else {
		schedule <- dplyr::bind_cols(schedule, next_stage)
	}

	# schedule next stages nested within `schedule_predict_stage_i()`
	schedule %>%
		dplyr::mutate(
			predict_stage = purrr::map(
				predict_stage,
				schedule_predict_stage_i,
				param_info = param_info
			)
		)
}

min_model_grid <- function(grid, model_param, wflow) {
	# work on only the model parameters
	model_grid <- grid %>%
		dplyr::select(dplyr::all_of(model_param)) %>%
		dplyr::distinct()

	if (nrow(model_grid) < 1) {
		return(model_grid)
	}

	min_grid(extract_spec_parsnip(wflow), model_grid) %>%
		dplyr::select(dplyr::all_of(model_param))
}

schedule_predict_stage_i <- function(predict_stage, param_info) {
  submodel_param <- param_info %>%
			dplyr::filter(source == "model_spec" & has_submodel) %>%
			dplyr::pull(id)

  predict_stage %>%
			tidyr::nest(
				.by = dplyr::all_of(submodel_param),
				.key = "post_stage"
			)
}

get_param_info <- function(wflow) {
	param_info <- tune_args(wflow) %>%
		dplyr::select(name, id, source)

	model_spec <- extract_spec_parsnip(wflow)
	model_type <- class(model_spec)[1]
	model_eng <- model_spec$engine

	model_param <- parsnip::get_from_env(paste0(model_type, "_args")) %>%
		dplyr::filter(engine == model_spec$engine) %>%
		dplyr::select(name = parsnip, has_submodel)

	param_info <- dplyr::left_join(param_info, model_param, by = "name")

	param_info
}
