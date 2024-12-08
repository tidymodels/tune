#' @export
get_tune_schedule <- function(wflow, param, grid) {
	# ----------------------------------------------------------------------------
	# Get information on the parameters associated with the supervised model

	model_spec <- extract_spec_parsnip(wflow)
	model_type <- class(model_spec)[1]
	model_eng <- model_spec$engine

	# Which, if any, is a submodel
	model_param <- parsnip::get_from_env(paste0(model_type, "_args")) %>%
		dplyr::filter(engine == model_spec$engine) %>%
		dplyr::select(name = parsnip, has_submodel)

	# Merge the info in with the other parameters
	param <- dplyr::left_join(param, model_param, by = "name") %>%
		dplyr::mutate(
			has_submodel = if_else(is.na(has_submodel), FALSE, has_submodel)
		)

	# ------------------------------------------------------------------------------
	# Get tuning parameter IDs for each stage of the workflow

	if (any(param$source == "recipe")) {
		pre_id <- param$id[param$source == "recipe"]
	} else {
		pre_id <- character(0)
	}

	if (any(param$source == "model_spec")) {
		model_id <- param$id[param$source == "model_spec"]
		sub_id <- param$id[param$source == "model_spec" & param$has_submodel]
		non_sub_id <- param$id[param$source == "model_spec" & !param$has_submodel]
	} else {
		model_id <- sub_id <- non_sub_id <- character(0)
	}

	if (any(param$source == "tailor")) {
		post_id <- param$id[param$source == "tailor"]
	} else {
		post_id <- character(0)
	}

	ids <- list(
		all = param$id,
		pre = pre_id,
		# All model param
		model = model_id,
		fits = c(pre_id, non_sub_id),
		sub = sub_id,
		non_sub = non_sub_id,
		post = post_id
	)
	# convert to symbols
	symbs <- purrr::map(ids, syms)

	has_submodels <- length(ids$sub) > 0

	# ------------------------------------------------------------------------------
	# First collapse the submodel parameters (if any)

	if (has_submodels) {
		sched <- grid %>%
			dplyr::group_nest(!!!symbs$fits, .key = "predict_stage")
		# Note: multi_predict() should only be triggered for a submodel parameter if
		# there are multiple rows in the `predict_stage` list column. i.e. the submodel
		# column will always be there but we only multipredict when there are 2+
		# values to predict.
		first_loop_info <- min_grid(model_spec, grid)
	} else {
		sched <- grid %>%
			dplyr::group_nest(!!!symbs$all, .key = "predict_stage")
		first_loop_info <- grid
	}

	first_loop_info <- first_loop_info %>%
		dplyr::select(!!!c(symbs$pre, symbs$model)) %>%
		dplyr::distinct()

	# ------------------------------------------------------------------------------
	# Add info an any postprocessing parameters

	sched <- sched %>%
		dplyr::mutate(
			predict_stage = purrr::map(
				predict_stage,
				~.x %>% dplyr::group_nest(!!!symbs$sub, .key = "post_stage")
			)
		)

	# ------------------------------------------------------------------------------
	# Merge in submodel fit value (if any)

	loop_names <- names(sched)[names(sched) != "predict_stage"]
	sched <- dplyr::full_join(sched, first_loop_info, by = loop_names)

	# ------------------------------------------------------------------------------
	# Now collapse over the preprocessor for conditional execution

	sched <- sched %>% dplyr::group_nest(!!!symbs$pre, .key = "model_stage")

	# ------------------------------------------------------------------------------

	og_cls <- class(sched)
	if (nrow(param) == 0) {
		cls <- "resample_schedule"
	} else if (nrow(param) == 1) {
		cls <- "single_schedule"
	} else {
		cls <- "grid_schedule"
	}
	class(sched) <- c(cls, "tune_schedule", og_cls)
	sched
}
