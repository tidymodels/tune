# This code is a working version to figure some things out; it probably
# won't make it into main as-is

no_stage <- function(x) {
	stages <- c("model_stage", "predict_stage", "post_stage")
	x[, !(names(x) %in% stages)]
}

text_param <- function(x) {
	x <- no_stage(x)
	x <- as.list(x)
	x <- purrr::map_chr(x, ~format(.x, digits = 3))
	x <- paste0(names(x), ": ", x)
	cli::format_inline("{x}")
}

has_pre_param <- function(x) {
	any(names(x) != "model_stage")
}

has_mod_param <- function(x) {
	any(names(x) != "predict_stage")
}

has_sub_param <- function(x) {
	not_post_list <- names(x) != "post_stage"
	has_param_col <- any(not_post_list)
	if (!has_param_col) {
		return(FALSE)
	}
	param_col_nm <- names(x)[not_post_list]
	param_col <- x[[param_col_nm]]
	two_plus_vals <- length(param_col) > 1
	two_plus_vals
}

get_sub_param <- function(x) {
	not_post_list <- names(x) != "post_stage"
	names(x)[not_post_list]
}

# from workflows
has_tailor <- function(x) {
	"tailor" %in% names(x$post$actions)
}
#
has_tailor_tuned <- function(x) {
	if (!has_tailor(x)) {
		res <- FALSE
	} else {
		res <- any(tune_args(x)$source == "tailor")
	}
	res
}
has_tailor_estimated <- function(x) {
	if (!has_tailor(x)) {
		res <- FALSE
	} else {
		post <- extract_postprocessor(x)
		res <- tailor::tailor_requires_fit(post)
	}
	res
}

# TODO add .row to data

# ------------------------------------------------------------------------------

pred_post_strategy <- function(x) {
	if (has_tailor(x)) {
		if (has_tailor_tuned(x) | has_tailor_estimated(x)) {
			# TODO do we need to check estimation?
			# There is no way to get around having to estimate/fit the tailor object
			# for each tuning combination
			res <- "loop over pred and post"
		} else {
			# For a set of predictions, or a set of submodel predictions, we can
			# just apply the tailor object (i.e. predict) to the set(s)
			res <- "predict and post at same time"
		}
	} else {
		# Stop at prediction, submodels or not
		res <- "just predict"
	}
	res
}

predict_only <- function(wflow, sched, dat, grid) {
	outputs <- get_output_columns(wflow, syms = TRUE)

	if (has_sub_param(sched$predict_stage[[1]])) {
		cli::cli_inform("multipredict only")
		sub_param <- get_sub_param(sched$predict_stage[[1]])

		# get submodel name and vector; remove col from grid
		# loop over types
		# move to predict_wrapper
		processed_data_pred <- forge_from_workflow(dat$pred, wflow)
		pred <- wflow %>%
			extract_fit_parsnip() %>%
		  # TODO use predict wrapper
			multi_predict(processed_data_pred$predictors, trees = 1:10) %>%
			mutate(.row = dat$index) %>%
			tidyr::unnest(.pred) %>%
			dplyr::full_join(dat$pred %>% add_rowindex(), by = ".row") %>%
			dplyr::select(
				!!!outputs$outcome,
				!!!outputs$estimate,
				.row,
				dplyr::all_of(sub_param)
			) %>%
			cbind(grid %>% dplyr::select(-dplyr::all_of(sub_param))) %>%
			dplyr::arrange(.row)
	} else {
		cli::cli_inform("predict only")
		pred <- augment(wflow, dat$pred) %>%
			dplyr::select(!!!unlist(outputs)) %>%
		  dplyr::mutate(.row = dat$index) %>%
			cbind(grid) %>%
		  dplyr::arrange(.row)
	}
	pred
}

predict_post_one_shot <- function(wflow, sched, dat, grid) {
	cli::cli_inform("predict/post once")

	outputs <- get_output_columns(wflow, syms = TRUE)

	if (has_sub_param(sched$predict_stage)) {
		sub_param <- get_sub_param(sched$predict_stage[[1]])

		dat_ex <- augment(wflow, dat$pred) %>%
			dplyr::select(!!!unlist(unname(outputs)))
		post_obj <- wflow %>%
			extract_postprocessor() %>%
			fit(
				.data = dat_ex,
				outcome = !!outputs$outcome[[1]],
				estimate = !!outputs$estimate[[1]],
				probabilities = c(!!!outputs$probabilities)
			)
		processed_data_pred <- forge_from_workflow(dat$pred, wflow)
		pred <- wflow %>%
			extract_fit_parsnip() %>%
			# Do this will call2?
			multi_predict(processed_data_pred$predictors, trees = 1:10) %>%
		  dplyr::mutate(.row = dat$index) %>%
			dplyr::mutate(.pred = purrr::map(.pred, ~predict(post_obj, .x))) %>%
			tidyr::unnest(.pred) %>%
			dplyr::full_join(
				processed_data_pred$outcomes %>% add_rowindex(),
				by = ".row"
			) %>%
			cbind(grid %>% dplyr::select(-dplyr::all_of(sub_param))) %>%
			dplyr::arrange(.row)
	} else {
		pred <- wflow %>%
			.fit_post(dat$fit) %>%
			.fit_finalize() %>%
			predict(dat$pred) %>%
		  dplyr::mutate(.row = dat$index) %>%
			cbind(grid) %>%
		  dplyr::arrange(.row)
	}
	pred
}

predict_post_loop <- function(wflow, sched, dat, grid) {
	cli::cli_inform("predict/post looping")
	outputs <- get_output_columns(wflow, syms = TRUE)
	num_pred_iter <- nrow(sched$predict_stage[[1]])

	## pre-allocate?

	for (prd in seq_len(num_pred_iter)) {
		current_pred <- sched$predict_stage[[1]][prd, ]

		num_post_iter <- nrow(current_pred$post_stage[[1]])

		wflow_clone <- wflow
		for (post in seq_len(num_post_iter)) {
			current_post <- current_pred$post_stage[[1]][post, ]

			current_grid <- dplyr::bind_cols(grid, no_stage(current_post))
			wflow_clone <- post_update_fit(wflow, current_post, dat$cal)

			pred <- augment(wflow_clone, dat$pred) %>%
				dplyr::select(!!!unlist(outputs)) %>%
				cbind(current_grid) %>%
			  dplyr::mutate(.row = dat$index) %>%
			  dplyr::arrange(.row)
			# bind cols;
		}
	}
	pred
}

predictions <- function(wflow, sched, dat, grid) {
	strategy <- pred_post_strategy(wflow)
	if (strategy == "just predict") {
		pred <- predict_only(wflow, sched, dat, grid)
	} else if (strategy == "predict and post at same time") {
		pred <- predict_post_one_shot(wflow, sched, dat, grid)
	} else {
		pred <- predict_post_loop(wflow, sched, dat, grid)
	}
	if (tibble::is_tibble(pred)) {
	  pred <- dplyr::as_tibble(pred)
	}
	pred
}

# ------------------------------------------------------------------------------

pre_update_fit <- function(wflow, grid, fit_data) {
	pre_proc <- extract_preprocessor(wflow)

	if (inherits(pre_proc, "recipe")) {
		grid <- no_stage(grid)
		pre_proc_param <- extract_parameter_set_dials(pre_proc)
		pre_proc_id <- pre_proc_param$id

		if (length(pre_proc_id) > 0) {
			grid <- grid[, pre_proc_id]
			pre_proc <- finalize_recipe(pre_proc, grid)
			wflow <- set_workflow_recipe(wflow, pre_proc)
		}
	}
	.fit_pre(wflow, fit_data)
}

model_update_fit <- function(wflow, grid) {
	mod_spec <- extract_spec_parsnip(wflow)

	grid <- no_stage(grid)
	pre_proc_param <- extract_parameter_set_dials(mod_spec)
	pre_proc_id <- pre_proc_param$id

	if (length(pre_proc_id) > 0) {
		grid <- grid[, pre_proc_id]
		mod_spec <- finalize_model(mod_spec, grid)
		wflow <- set_workflow_spec(wflow, mod_spec)
	}

	.fit_model(wflow, control_workflow())
}

post_update_fit <- function(wflow, grid, post_data) {
	post_spec <- extract_postprocessor(wflow)

	grid <- no_stage(grid)
	post_proc_param <- extract_parameter_set_dials(post_spec)
	post_proc_id <- post_proc_param$id

	if (length(post_proc_id) > 0) {
		grid <- grid[, post_proc_id]
		post_spec <- finalize_tailor(post_spec, grid)
		wflow <- set_workflow_tailor(wflow, post_spec)
	}

	wflow <- .fit_post(wflow, post_data)
	.fit_finalize(wflow)
}

rebind_grid <- function(...) {
	list(...) %>% purrr::map(no_stage) %>% purrr::list_cbind()
}

get_output_columns <- function(x, syms = FALSE) {
	pred_cols <- .get_prediction_column_names(x, syms = TRUE)
	res <- c(list(outcome = rlang::syms(outcome_names(x))), pred_cols)
	res
}

# ------------------------------------------------------------------------------
# pre-allocating predictions

initialize_pred_reserve <- function(predictions, grid_size) {
	if (tibble::is_tibble(predictions)) {
		predictions <- dplyr::as_tibble(predictions)
	}
	grid_size <- max(1, grid_size)
	ptype <- predictions[0, ]
	size <- nrow(predictions) * grid_size
	res <- ptype[1:size, ]
	dplyr::as_tibble(res)
}

replace_reserve_rows <- function(iter, chunk) {
	start_loc <- (iter - 1) * chunk + 1
	end_loc <- iter * chunk
	start_loc:end_loc
}

update_reserve <- function(reserve, iter, predictions, grid_size) {
	grid_size <- min(1, grid_size)
	pred_size <- nrow(predictions)

	if (is.null(reserve)) {
		reserve <- initialize_pred_reserve(predictions, grid_size)
	} else {
		if (tibble::is_tibble(predictions)) {
			predictions <- dplyr::as_tibble(predictions)
		}
	}
	reserve[replace_reserve_rows(iter, pred_size), ] <- predictions
	reserve
}

# ------------------------------------------------------------------------------

#' @export
loopy <- function(sched, wflow, grid_size, dat) {
	pred_reserve <- NULL
	pred_iter <- 0

	# ----------------------------------------------------------------------------
	# Iterate over preprocessors

	num_pre_iter <- nrow(sched)

	for (pre in seq_len(num_pre_iter)) {
		current_pre <- sched[pre, ]
		cli::cli_inform(
			"{pre}/{num_pre_iter} preprocessing: {text_param(current_pre)}"
		)

		current_wflow <- pre_update_fit(wflow, current_pre, dat$fit)
		num_mod_iter <- nrow(current_pre$model_stage[[1]])

		# --------------------------------------------------------------------------
		# Iterate over model parameters

		for (mod in seq_len(num_mod_iter)) {
			current_model <- current_pre$model_stage[[1]][mod, ]
			cli::cli_inform(
				"├── {mod}/{num_mod_iter} model: {text_param(current_model)}"
			)

			current_wflow <- model_update_fit(current_wflow, current_model)

			num_pred_iter <- nrow(current_model$predict_stage[[1]])
			current_grid <- rebind_grid(current_pre, current_model)

			# ------------------------------------------------------------------------
			# Iterate over predictions and postprocessors

			pred <- predictions(
				current_wflow,
				current_model,
				dat,
				current_grid
			)

			pred_iter <- pred_iter + 1
			pred_reserve <- update_reserve(pred_reserve, pred_iter, pred, grid_size)
		} # model loop
	} # pre loop
	pred_reserve
}
