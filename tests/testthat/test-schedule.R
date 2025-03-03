
# `get_param_info()` -----------------------------------------------------

test_that("`get_param_info()` works for a workflow without tags for tuning", {
	wflow <- workflow(mpg ~ ., parsnip::linear_reg())
	param_info <- get_param_info(wflow)

	expect_named(param_info, c("name", "id", "source", "has_submodel"))
	expect_identical(nrow(param_info), 0L)
})

test_that("`get_param_info()` works for a workflow with tags for tuning", {
	skip_if_not_installed("splines2")
	skip_if_not_installed("probably")

	# tuning tags in all components
	wflow <- workflow(rec_tune, mod_tune_no_submodel, tlr_tune)

	param_info <- get_param_info(wflow)

	expect_named(param_info, c("name", "id", "source", "has_submodel"))
	expect_identical(
		param_info$id,
		c("min_n", "threshold", "disp_df", "lower_limit")
	)
	expect_identical(param_info$has_submodel, c(FALSE, NA, NA, NA))
})

test_that("`get_param_info()` works when there are submodel parameters", {
	skip_if_not_installed("probably")

	# tuning tags only in model spec
	rec_no_steps <- recipes::recipe(mpg ~ ., data = mtcars)

	wflow <- workflow(rec_no_steps, mod_tune, tlr_no_tune)

	param_info <- get_param_info(wflow)

	expect_named(param_info, c("name", "id", "source", "has_submodel"))
	expect_identical(param_info$name, c("trees", "min_n"))
	expect_identical(param_info$has_submodel, c(TRUE, FALSE))
})


# `schedule_predict_stage_i()` -------------------------------------------

test_that("`schedule_predict_stage_i()` works with: no submodel, no post-processing", {
	skip_if_not_installed("probably")

	wflow <- workflow(mpg ~ ., mod_no_tune, tlr_no_tune)
	param_info <- get_param_info(wflow)
	grid_predict_stage <- tibble::tibble()

	schedule <- schedule_predict_stage_i(grid_predict_stage, param_info)
	expect_named(schedule, c("post_stage"))
	expect_identical(nrow(schedule), 0L)
})

test_that("`schedule_predict_stage_i()` works with: no submodel, with post-processing", {
	skip_if_not_installed("probably")

	wflow <- workflow(mpg ~ ., mod_no_tune, tlr_tune)
	param_info <- get_param_info(wflow)
	grid_predict_stage <- tibble::tibble(lower_limit = 1:2)

	schedule <- schedule_predict_stage_i(grid_predict_stage, param_info)
	expect_named(schedule, c("post_stage"))
	expect_identical(nrow(schedule), 1L)
	expect_identical(schedule$post_stage[[1]], grid_predict_stage)
})

test_that("`schedule_predict_stage_i()` works with: with submodel, no post-processing", {
	skip_if_not_installed("probably")

	wflow <- workflow(mpg ~ ., mod_tune_submodel, tlr_no_tune)
	param_info <- get_param_info(wflow)
	grid_predict_stage <- tibble::tibble(trees = 1:2)

	schedule <- schedule_predict_stage_i(grid_predict_stage, param_info)
	expect_named(schedule, c("trees", "post_stage"))
	expect_identical(schedule$trees, grid_predict_stage$trees)
	expect_identical(
		purrr::map_int(schedule$post_stage, ncol),
		c(0L, 0L)
	)
})

test_that("`schedule_predict_stage_i()` works with: with submodel, with post-processing", {
	skip_if_not_installed("probably")

	wflow <- workflow(mpg ~ ., mod_tune_submodel, tlr_tune)
	param_info <- get_param_info(wflow)
	# semi-regular grid
	grid_predict_stage <- list(
		tibble::tibble(trees = 1L, lower_limit = 1L),
		tibble::tibble(trees = c(2L, 2L), lower_limit = 1:2)
	) %>%
		purrr::list_rbind()

	schedule <- schedule_predict_stage_i(grid_predict_stage, param_info)
	expect_named(schedule, c("trees", "post_stage"))
	expect_identical(schedule$trees, 1:2)
	expect_identical(
		schedule$post_stage[[1]],
		tibble::tibble(lower_limit = 1L)
	)
	expect_identical(
		schedule$post_stage[[2]],
		tibble::tibble(lower_limit = 1:2)
	)
})


# `schedule_model_stage_i()` ---------------------------------------------

test_that("`schedule_model_stage_i()` works with: no tuning at all", {
	skip_if_not_installed("probably")

	wflow <- workflow(mpg ~ ., mod_no_tune, tlr_no_tune)
	param_info <- get_param_info(wflow)
	grid_model_stage <- tibble::tibble()

	schedule <- schedule_model_stage_i(grid_model_stage, param_info, wflow)
	expect_named(schedule, c("predict_stage"))
	expect_identical(nrow(schedule), 0L)
})

test_that("`schedule_model_stage_i()` works with only non-submodel: with non-submodel, no submodel, no post", {
	skip_if_not_installed("probably")

	wflow <- workflow(mpg ~ ., mod_tune_no_submodel, tlr_no_tune)
	param_info <- get_param_info(wflow)
	grid_model_stage <- tibble::tibble(min_n = 1:2)

	schedule <- schedule_model_stage_i(grid_model_stage, param_info, wflow)
	expect_named(schedule, c("min_n", "predict_stage"))
	expect_identical(nrow(schedule), 2L)
	expect_identical(schedule$min_n, 1:2)
	expect_identical(
		purrr::map_chr(schedule$predict_stage, ~ class(.)[1]),
		c("tbl_df", "tbl_df")
	)
	expect_identical(
		purrr::map_chr(schedule$predict_stage, names),
		c("post_stage", "post_stage")
	)
})

test_that("`schedule_model_stage_i()` works with only submodel: no non-submodel, with submodel, no post", {
	skip_if_not_installed("probably")

	wflow <- workflow(mpg ~ ., mod_tune_submodel, tlr_no_tune)
	param_info <- get_param_info(wflow)
	grid_model_stage <- tibble::tibble(trees = 1:2)

	schedule <- schedule_model_stage_i(grid_model_stage, param_info, wflow)
	expect_named(schedule, c("trees", "predict_stage"))
	expect_identical(nrow(schedule), 1L)
	expect_identical(schedule$trees, 2L)
	expect_identical(
		purrr::map_chr(schedule$predict_stage, ~ class(.)[1]),
		c("tbl_df")
	)
	expect_identical(
		purrr::map(schedule$predict_stage, names),
		list(c("trees", "post_stage"))
	)
	expect_identical(
		schedule$predict_stage[[1]] %>% pull("trees"),
		1:2
	)
})

test_that("`schedule_model_stage_i()` works with only post: no non-submodel, no submodel, with post", {
	skip_if_not_installed("probably")

	wflow <- workflow(mpg ~ ., mod_no_tune, tlr_tune)
	param_info <- get_param_info(wflow)
	grid_model_stage <- tibble::tibble(lower_limit = 1:2)

	schedule <- schedule_model_stage_i(grid_model_stage, param_info, wflow)
	expect_named(schedule, c("predict_stage"))
	expect_identical(nrow(schedule), 1L)
	expect_identical(
		purrr::map_chr(schedule$predict_stage, ~ class(.)[1]),
		c("tbl_df")
	)
	expect_identical(
		purrr::map(schedule$predict_stage, names),
		list(c("post_stage"))
	)
	expect_identical(
		schedule$predict_stage[[1]] %>% pull("post_stage"),
		list(tibble::tibble(lower_limit = 1:2))
	)
})

test_that("`schedule_model_stage_i()` works with both model types only: with non-submodel, with submodel, no post", {
	skip_if_not_installed("probably")

	wflow <- workflow(mpg ~ ., mod_tune, tlr_no_tune)
	param_info <- get_param_info(wflow)
	# irregular grid
	grid_model_stage <- list(
		tibble::tibble(trees = 1:2, min_n = 1L),
		tibble::tibble(trees = 2L, min_n = 2L),
		tibble::tibble(trees = 2:3, min_n = 3L)
	) %>%
		purrr::list_rbind()

	schedule <- schedule_model_stage_i(grid_model_stage, param_info, wflow)

	expect_named(schedule, c("trees", "min_n", "predict_stage"))
	expect_identical(nrow(schedule), 3L)
	expect_identical(schedule$trees, c(2L, 2L, 3L))
	expect_identical(schedule$min_n, 1:3)
	expect_identical(
		purrr::map_chr(schedule$predict_stage, ~ class(.)[1]) %>% unique(),
		"tbl_df"
	)
	expect_identical(
		purrr::map(schedule$predict_stage, names),
		list(
			c("trees", "post_stage"),
			c("trees", "post_stage"),
			c("trees", "post_stage")
		)
	)
	expect_identical(
		schedule %>%
			filter(min_n == 1) %>%
			pull("predict_stage") %>%
			purrr::pluck(1) %>%
			pull("trees"),
		1:2
	)
	expect_identical(
		schedule %>%
			filter(min_n == 2) %>%
			pull("predict_stage") %>%
			purrr::pluck(1) %>%
			pull("trees"),
		2L
	)
	expect_identical(
		schedule %>%
			filter(min_n == 3) %>%
			pull("predict_stage") %>%
			purrr::pluck(1) %>%
			pull("trees"),
		2:3
	)
})

test_that("`schedule_model_stage_i()` works without submodel: with non-submodel, no submodel, with post", {
	skip_if_not_installed("probably")

	wflow <- workflow(mpg ~ ., mod_tune_no_submodel, tlr_tune)
	param_info <- get_param_info(wflow)
	# semi-regular grid
	grid_model_stage <- list(
		tibble::tibble(min_n = 1L, lower_limit = 1:2),
		tibble::tibble(min_n = 2L, lower_limit = 1:3),
		tibble::tibble(min_n = 3L, lower_limit = 3L)
	) %>%
		purrr::list_rbind()

	schedule <- schedule_model_stage_i(grid_model_stage, param_info, wflow)
	expect_named(schedule, c("min_n", "predict_stage"))
	expect_identical(nrow(schedule), 3L)
	expect_identical(schedule$min_n, 1:3)
	expect_identical(
		purrr::map_chr(schedule$predict_stage, ~ class(.)[1]) %>% unique(),
		"tbl_df"
	)
	expect_identical(
		purrr::map(schedule$predict_stage, names) %>%
			purrr::list_c() %>%
			unique(),
		"post_stage"
	)
	expect_identical(
		schedule %>%
			filter(min_n == 1) %>%
			pull("predict_stage") %>%
			purrr::pluck(1) %>%
			pull("post_stage") %>%
			purrr::pluck(1) %>%
			pull("lower_limit"),
		1:2
	)
	expect_identical(
		schedule %>%
			filter(min_n == 2) %>%
			pull("predict_stage") %>%
			purrr::pluck(1) %>%
			pull("post_stage") %>%
			purrr::pluck(1) %>%
			pull("lower_limit"),
		1:3
	)
	expect_identical(
		schedule %>%
			filter(min_n == 3) %>%
			pull("predict_stage") %>%
			purrr::pluck(1) %>%
			pull("post_stage") %>%
			purrr::pluck(1) %>%
			pull("lower_limit"),
		3L
	)
})

test_that("`schedule_model_stage_i()` works everything: with non-submodel, with submodel, with post", {
	skip_if_not_installed("probably")

	wflow <- workflow(mpg ~ ., mod_tune, tlr_tune)
	param_info <- get_param_info(wflow)
	# semi-regular grid
	grid_model_stage <- list(
		tibble::tibble(
			min_n = 1L,
			trees = c(1L, 1L, 2L, 2L),
			lower_limit = c(1:2, 1:2)
		),
		tibble::tibble(
			min_n = 2L,
			trees = c(1L, rep(2L, 3)),
			lower_limit = c(1L, 1:3)
		),
		# another row to be combined under min_n = 1
		tibble::tibble(min_n = 1L, trees = 3L, lower_limit = 4L)
	) %>%
		purrr::list_rbind()

	schedule <- schedule_model_stage_i(grid_model_stage, param_info, wflow)
	expect_named(schedule, c("trees", "min_n", "predict_stage"))
	expect_identical(nrow(schedule), 2L)
	expect_identical(schedule$min_n, 1:2)
	expect_identical(
		purrr::map_chr(schedule$predict_stage, ~ class(.)[1]) %>% unique(),
		"tbl_df"
	)
	expect_identical(
		purrr::map(schedule$predict_stage, names) %>%
			purrr::list_c() %>%
			unique(),
		c("trees", "post_stage")
	)
	expect_identical(
		schedule %>%
			filter(min_n == 1) %>%
			select(-predict_stage),
		tibble::tibble(trees = 3L, min_n = 1L)
	)
	expect_identical(
		schedule %>%
			filter(min_n == 1) %>%
			pull(predict_stage) %>%
			purrr::pluck(1) %>%
			pull(trees),
		1:3
	)
	expect_identical(
		schedule %>%
			filter(min_n == 1) %>%
			pull("predict_stage") %>%
			purrr::pluck(1) %>%
			pull("post_stage") %>%
			purrr::list_rbind() %>%
			pull("lower_limit"),
		c(1:2, 1:2, 4L)
	)
	expect_identical(
		schedule %>%
			filter(min_n == 2) %>%
			select(-predict_stage),
		tibble::tibble(trees = 2L, min_n = 2L)
	)
	expect_identical(
		schedule %>%
			filter(min_n == 2) %>%
			pull(predict_stage) %>%
			purrr::pluck(1) %>%
			pull(trees),
		1:2
	)
	expect_identical(
		schedule %>%
			filter(min_n == 2) %>%
			pull("predict_stage") %>%
			purrr::pluck(1) %>%
			pull("post_stage") %>%
			purrr::list_rbind() %>%
			pull("lower_limit"),
		c(1L, 1:3)
	)
})


# `schedule_stages()` ----------------------------------------------------

test_that("`schedule_stages()` works without preprocessing", {
	skip_if_not_installed("probably")

	wflow <- workflow(mpg ~ ., mod_no_tune, tlr_no_tune)
	grid <- tibble::tibble()

	schedule <- schedule_stages(grid, wflow)
	expect_named(schedule, c("model_stage"))
	expect_identical(nrow(schedule), 0L)
})

test_that("`schedule_stages()` works with preprocessing", {
	skip_if_not_installed("splines2")
	skip_if_not_installed("probably")

	wflow <- workflow(rec_tune, mod_no_tune, tlr_no_tune)
	grid <- tibble::tibble(
		threshold = rep(1:2, each = 2),
		disp_df = c(1:2, 1:2)
	)

	schedule <- schedule_stages(grid, wflow)
	expect_named(schedule, c("threshold", "disp_df", "model_stage"))
	expect_identical(nrow(schedule), 4L)
	expect_identical(
		schedule %>% dplyr::select(-model_stage),
		grid
	)
})

# `schedule_grid()` ------------------------------------------------------

# No tuning or postprocesing estimation

test_that("grid processing schedule - no parameters", {
	wflow_nada <- workflow(outcome ~ ., parsnip::logistic_reg())
	grid_nada <- tibble::tibble()

	sched_nada <- schedule_grid(grid_nada, wflow_nada)

	expect_named(sched_nada, "model_stage")
	expect_equal(nrow(sched_nada), 0)

	expect_s3_class(
		sched_nada,
		c("resample_schedule", "schedule", "tbl_df", "tbl", "data.frame")
	)
})

test_that("grid processing schedule - recipe and model", {
	skip_if_not_installed("splines2")

	wflow_pre_only <- workflow(rec_no_tune, parsnip::logistic_reg())
	grid_pre_only <- tibble::tibble()
	sched_pre_only <- schedule_grid(grid_pre_only, wflow_pre_only)

	expect_named(sched_pre_only, c("model_stage"))
	expect_equal(nrow(sched_pre_only), 0)

	expect_s3_class(
		sched_pre_only,
		c("resample_schedule", "schedule", "tbl_df", "tbl", "data.frame")
	)

})

test_that("grid processing schedule - recipe, model, and post", {
	skip_if_not_installed("splines2")
	skip_if_not_installed("probably")

	wflow_three <- workflow(rec_no_tune, parsnip::logistic_reg(), tlr_no_tune)
	grid_three <- tibble::tibble()
	sched_three <- schedule_grid(grid_three, wflow_three)

	expect_named(sched_three, c("model_stage"))
	expect_equal(nrow(sched_three), 0)

	expect_s3_class(
		sched_three,
		c("resample_schedule", "schedule", "tbl_df", "tbl", "data.frame")
	)

})

# Tuning, no postprocesing estimation

test_that("grid processing schedule - recipe only", {
	skip_if_not_installed("splines2")

	wflow_pre_only <- workflow(rec_tune, parsnip::logistic_reg())
	grid_pre_only <-
		extract_parameter_set_dials(wflow_pre_only) %>%
		dials::grid_regular(levels = 3) %>%
		arrange(threshold, disp_df)
	sched_pre_only <-
		schedule_grid(grid_pre_only, wflow_pre_only)

	expect_named(sched_pre_only, c("threshold", "disp_df", "model_stage"))
	expect_equal(nrow(sched_pre_only), nrow(grid_pre_only))

	# All of the other nested tibbles should be empty
	expect_equal(
		sched_pre_only %>%
			tidyr::unnest(model_stage) %>%
			tidyr::unnest(predict_stage) %>%
			tidyr::unnest(post_stage),
		grid_pre_only
	)

	expect_s3_class(
		sched_pre_only,
		c("grid_schedule", "schedule", "tbl_df", "tbl", "data.frame")
	)

})

test_that("grid processing schedule - model only, no submodels", {
	wflow_rf_only <- workflow(outcome ~ ., mod_tune_no_submodel)
	grid_rf_only <-
		extract_parameter_set_dials(wflow_rf_only) %>%
		dials::grid_regular(levels = 3)
	sched_rf_only <-
		schedule_grid(grid_rf_only, wflow_rf_only)

	expect_named(sched_rf_only, c("model_stage"))
	expect_equal(nrow(sched_rf_only), 1L)

	rf_n <- length(sched_rf_only$model_stage)
	for (i in 1:rf_n) {
		# No real need for the loop here
		expect_named(sched_rf_only$model_stage[[i]], c("min_n", "predict_stage"))
		expect_equal(
			sched_rf_only$model_stage[[i]] %>%
				tidyr::unnest(predict_stage) %>%
				tidyr::unnest(post_stage),
			grid_rf_only
		)
	}

	expect_s3_class(
		sched_rf_only,
		c("grid_schedule", "schedule", "tbl_df", "tbl", "data.frame")
	)

})

test_that("grid processing schedule - model only, submodels, regular grid", {
	wflow_bst <- workflow(outcome ~ ., mod_tune)
	grid_bst <- extract_parameter_set_dials(wflow_bst) %>%
		dials::grid_regular(levels = 3)

	min_n_only <- grid_bst %>% dplyr::distinct(min_n) %>% dplyr::arrange(min_n)
	trees_only <- grid_bst %>% dplyr::distinct(trees) %>% dplyr::arrange(trees)

	# regular grid
	sched_bst <- schedule_grid(grid_bst, wflow_bst)

	expect_named(sched_bst, c("model_stage"))
	expect_equal(nrow(sched_bst), 1L)

	reg_n <- length(sched_bst$model_stage)
	for (i in 1:reg_n) {
		expect_named(sched_bst$model_stage[[i]], c("trees", "min_n", "predict_stage"))

		expect_equal(
			sched_bst$model_stage[[i]] %>%
				dplyr::select(-trees, -predict_stage),
			min_n_only
		)

		for (j in seq_along(sched_bst$model_stage[[i]]$predict_stage)) {
			expect_named(
				sched_bst$model_stage[[i]]$predict_stage[[j]],
				c("trees", "post_stage"))
			expect_equal(
				sched_bst$model_stage[[i]]$predict_stage[[j]] %>%
					dplyr::select(trees),
				trees_only
			)
		}

		expect_equal(
			sched_bst$model_stage[[i]] %>%
				dplyr::select(-trees) %>%
				tidyr::unnest(predict_stage) %>%
				tidyr::unnest(post_stage) %>%
				dplyr::select(trees, min_n),
			grid_bst
		)
	}

	expect_s3_class(
		sched_bst,
		c("grid_schedule", "schedule", "tbl_df", "tbl", "data.frame")
	)
})

test_that("grid processing schedule - model only, submodels, SFD grid", {
	wflow_bst <- workflow(outcome ~ ., mod_tune)
	grid_sfd_bst <- extract_parameter_set_dials(wflow_bst) %>%
		dials::grid_space_filling(size = 5, type = "uniform")
	sched_sfd_bst <- schedule_grid(grid_sfd_bst, wflow_bst)

	expect_named(sched_sfd_bst, c("model_stage"))
	expect_equal(nrow(sched_sfd_bst), 1L)

	irreg_n <- length(sched_sfd_bst$model_stage)
	expect_equal(irreg_n, 1L)

	expect_named(sched_sfd_bst$model_stage[[1]], c("trees", "min_n", "predict_stage"))
	expect_equal(
		sched_sfd_bst$model_stage[[1]] %>%
			dplyr::select(-predict_stage) %>%
			dplyr::select(trees, min_n) %>%
			dplyr::arrange(trees, min_n),
		grid_sfd_bst %>%
			dplyr::select(trees, min_n) %>%
			dplyr::arrange(trees, min_n)
	)

	expect_equal(
		sched_sfd_bst$model_stage[[1]] %>%
			dplyr::select(-trees) %>%
			tidyr::unnest(predict_stage) %>%
			tidyr::unnest(post_stage) %>%
			dplyr::select(trees, min_n) %>%
			dplyr::arrange(trees, min_n),
		grid_sfd_bst %>%
			dplyr::select(trees, min_n) %>%
			dplyr::arrange(trees, min_n)
	)

	expect_s3_class(
		sched_sfd_bst,
		c("grid_schedule", "schedule", "tbl_df", "tbl", "data.frame")
	)

})

test_that("grid processing schedule - model only, submodels, irregular design", {
	wflow_bst <- workflow(outcome ~ ., mod_tune)
	grid_odd_bst <- tibble::tibble(
		min_n = c(1, 1, 2, 3, 4, 5),
		trees = rep(1:2, 3)
	)
	sched_odd_bst <- schedule_grid(grid_odd_bst, wflow_bst)

	expect_named(sched_odd_bst, c("model_stage"))
	expect_equal(nrow(sched_odd_bst), 1L)

	odd_n <- length(sched_odd_bst$model_stage)
	expect_equal(odd_n, 1L)

	expect_named(sched_odd_bst$model_stage[[1]], c("trees", "min_n", "predict_stage"))
	expect_equal(
		sched_odd_bst$model_stage[[1]] %>%
			dplyr::select(-predict_stage) %>%
			dplyr::select(trees, min_n),
		tibble::tibble(trees = c(2, 1, 2, 1, 2), min_n = c(1, 2, 3, 4, 5))
	)

	for (i in 1:nrow(sched_odd_bst$model_stage[[1]])) {
		prd <- sched_odd_bst$model_stage[[1]]$predict_stage[[i]]
		if (sched_odd_bst$model_stage[[1]]$min_n[i] == 1) {
			expect_equal(nrow(prd), 2L)
		} else {
			expect_equal(nrow(prd), 1L)
		}
		expect_true(
			all(purrr::map_int(prd$post_stage, nrow) == 1)
		)
	}

	expect_s3_class(
		sched_odd_bst,
		c("grid_schedule", "schedule", "tbl_df", "tbl", "data.frame")
	)
})

test_that("grid processing schedule - model only, submodels, 1 point design", {
	wflow_bst <- workflow(outcome ~ ., mod_tune)

	set.seed(1)
	grid_1_pt <- extract_parameter_set_dials(wflow_bst) %>%
		dials::grid_random(size = 1)
	sched_1_pt <- schedule_grid(grid_1_pt, wflow_bst)

	expect_named(sched_1_pt, c("model_stage"))
	expect_equal(nrow(sched_1_pt), 1L)
	expect_equal(length(sched_1_pt$model_stage), 1L)
	expect_named(
		sched_1_pt$model_stage[[1]],
		c("trees", "min_n", "predict_stage")
	)

	expect_equal(
		length(sched_1_pt$model_stage[[1]]$predict_stage),
		1L
	)
	expect_named(
		sched_1_pt$model_stage[[1]]$predict_stage[[1]],
		c("trees", "post_stage")
	)

	expect_equal(
		length(sched_1_pt$model_stage[[1]]$predict_stage[[1]]$post_stage),
		1L
	)
	expect_equal(
		dim(sched_1_pt$model_stage[[1]]$predict_stage[[1]]$post_stage[[1]]),
		1:0
	)

	expect_s3_class(
		sched_1_pt,
		c(
			"single_schedule",
			"grid_schedule",
			"schedule",
			"tbl_df",
			"tbl",
			"data.frame"
		)
	)
})

test_that("grid processing schedule - postprocessing only", {
	skip_if_not_installed("probably")

	wflow_thrsh <- workflow(outcome ~ ., parsnip::logistic_reg(), tlr_tune)
	grid_thrsh <- extract_parameter_set_dials(wflow_thrsh) %>%
		update(lower_limit = dials::lower_limit(c(0, 1))) %>%
		dials::grid_regular(levels = 3)

	sched_thrsh <- schedule_grid(grid_thrsh, wflow_thrsh)

	expect_named(sched_thrsh, c("model_stage"))
	expect_equal(nrow(sched_thrsh), 1L)

	expect_named(sched_thrsh$model_stage[[1]], c("predict_stage"))
	expect_equal(nrow(sched_thrsh$model_stage[[1]]), 1L)

	expect_named(
		sched_thrsh$model_stage[[1]]$predict_stage[[1]],
		c("post_stage")
	)
	expect_equal(nrow(sched_thrsh$model_stage[[1]]), 1L)

	expect_equal(
		sched_thrsh$model_stage[[1]]$predict_stage[[1]]$post_stage[[1]],
		grid_thrsh
	)

	expect_s3_class(
		sched_thrsh,
		c("grid_schedule", "schedule", "tbl_df", "tbl", "data.frame")
	)
})

test_that("grid processing schedule - recipe + postprocessing, regular grid", {
	skip_if_not_installed("splines2")
	skip_if_not_installed("probably")

	wflow_pre_post <- workflow(rec_tune, parsnip::logistic_reg(), tlr_tune)
	grid_pre_post <-
		extract_parameter_set_dials(wflow_pre_post) %>%
		update(lower_limit = dials::lower_limit(c(0, 1))) %>%
		dials::grid_regular(levels = 3)

	grid_pre <-
		grid_pre_post %>%
		distinct(threshold, disp_df)
	grid_post <-
		grid_pre_post %>%
		distinct(lower_limit) %>%
		arrange(lower_limit)

	sched_pre_post <- schedule_grid(grid_pre_post, wflow_pre_post)

	expect_named(sched_pre_post, c("threshold", "disp_df", "model_stage"))
	expect_equal(
		sched_pre_post %>% select(-model_stage) %>% tibble::as_tibble(),
		grid_pre
	)

	for (i in seq_along(sched_pre_post$model_stage)) {
		expect_named(sched_pre_post$model_stage[[i]], c("predict_stage"))
		expect_equal(nrow(sched_pre_post$model_stage[[i]]), 1L)
	}

	for (i in seq_along(sched_pre_post$model_stage)) {
		expect_named(
			sched_pre_post$model_stage[[i]]$predict_stage[[1]],
			c("post_stage")
		)
		expect_identical(
			sched_pre_post$model_stage[[i]]$predict_stage[[1]]$post_stage[[1]] %>%
				arrange(lower_limit),
			grid_post
		)
	}

	expect_s3_class(
		sched_pre_post,
		c("grid_schedule", "schedule", "tbl_df", "tbl", "data.frame")
	)
})

test_that("grid processing schedule - recipe + postprocessing, irregular grid", {
	skip_if_not_installed("splines2")
	skip_if_not_installed("probably")

	wflow_pre_post <- workflow(rec_tune, parsnip::logistic_reg(), tlr_tune)
	grid_pre_post <-
		extract_parameter_set_dials(wflow_pre_post) %>%
		update(lower_limit = dials::lower_limit(c(0, 1))) %>%
		dials::grid_regular() %>%
		dplyr::slice(-c(1, 14))

	grid_pre <-
		grid_pre_post %>%
		distinct(threshold, disp_df) 

	grids_post <-
		grid_pre_post %>%
		dplyr::group_nest(threshold, disp_df) %>%
		mutate(data = purrr::map(data, ~ arrange(.x, lower_limit)))


	sched_pre_post <- schedule_grid(grid_pre_post, wflow_pre_post)

	expect_named(sched_pre_post, c("threshold", "disp_df", "model_stage"))
	expect_equal(
		sched_pre_post %>% select(-model_stage) %>% tibble::as_tibble(),
		grid_pre
	)

	for (i in seq_along(sched_pre_post$model_stage)) {
		expect_named(sched_pre_post$model_stage[[i]], c("predict_stage"))
		expect_equal(nrow(sched_pre_post$model_stage[[i]]), 1L)
	}

	for (i in seq_along(sched_pre_post$model_stage)) {
		expect_named(
			sched_pre_post$model_stage[[i]]$predict_stage[[1]],
			c("post_stage")
		)

		pre_grid_i <-
			sched_pre_post %>%
			slice(i) %>%
			select(threshold, disp_df)

		post_grid_i <-
			pre_grid_i %>%
			inner_join(grids_post, by = dplyr::join_by(threshold, disp_df)) %>%
			purrr::pluck("data") %>%
			purrr::pluck(1) %>%
			arrange(lower_limit)

		expect_identical(
			sched_pre_post$model_stage[[i]]$predict_stage[[1]]$post_stage[[1]] %>%
				arrange(lower_limit),
			post_grid_i
		)
	}

	expect_s3_class(
		sched_pre_post,
		c("grid_schedule", "schedule", "tbl_df", "tbl", "data.frame")
	)
})

test_that("grid processing schedule - recipe + model, no submodels, regular grid", {
	skip_if_not_installed("splines2")

	wflow_pre_model <- workflow(rec_tune, mod_tune_no_submodel)
	grid_pre_model <-
		extract_parameter_set_dials(wflow_pre_model) %>%
		dials::grid_regular()

	grid_pre <-
		grid_pre_model %>%
		distinct(threshold, disp_df)

	grid_model <-
		grid_pre_model %>%
		distinct(min_n) %>%
		arrange(min_n)

	
	sched_pre_model <- schedule_grid(grid_pre_model, wflow_pre_model)

	expect_named(sched_pre_model, c("threshold", "disp_df", "model_stage"))
	expect_equal(
		sched_pre_model %>% select(-model_stage) %>% tibble::as_tibble(),
		grid_pre
	)

	for (i in seq_along(sched_pre_model$model_stage)) {
		expect_named(sched_pre_model$model_stage[[i]], c("min_n", "predict_stage"))
		expect_equal(
			sched_pre_model$model_stage[[i]] %>% select(min_n) %>% arrange(min_n),
			grid_model
		)
	}

	for (i in seq_along(sched_pre_model$model_stage)) {
		expect_named(
			sched_pre_model$model_stage[[i]]$predict_stage[[1]],
			c("post_stage")
		)

		expect_equal(
			nrow(sched_pre_model$model_stage[[i]]$predict_stage[[1]]),
			1L
		)
		expect_equal(
			nrow(sched_pre_model$model_stage[[i]]$predict_stage[[1]]$post_stage[[1]]),
			1L
		)
	}

	expect_s3_class(
		sched_pre_model,
		c("grid_schedule", "schedule", "tbl_df", "tbl", "data.frame")
	)
})

test_that("grid processing schedule - recipe + model, submodels, irregular grid", {
	skip_if_not_installed("splines2")

	wflow_pre_model <- workflow(rec_tune, mod_tune)
	grid_pre_model <-
		extract_parameter_set_dials(wflow_pre_model) %>%
		dials::grid_regular() %>%
		# This will make the submodel parameter (trees) unbalanced for some
		# combination of parameters of the other parameters.
		slice(-c(1, 2, 11))

	grid_pre <-
		grid_pre_model %>%
		distinct(threshold, disp_df)

	grid_model <-
		grid_pre_model %>%
		dplyr::group_nest(threshold, disp_df) %>%
		mutate(
			data = purrr::map(data, ~ .x %>% dplyr::summarize(trees = max(trees), .by = c(min_n))),
			data = purrr::map(data, ~ .x %>% arrange(min_n))
		)

	
	sched_pre_model <- schedule_grid(grid_pre_model, wflow_pre_model)

	expect_named(sched_pre_model, c("threshold", "disp_df", "model_stage"))
	expect_equal(
		sched_pre_model %>% select(-model_stage) %>% tibble::as_tibble(),
		grid_pre
	)

	for (i in seq_along(sched_pre_model$model_stage)) {
		model_i <- sched_pre_model$model_stage[[i]]
		expect_named(model_i, c("trees", "min_n", "predict_stage"))
		expect_equal(
			model_i %>% select(min_n, trees) %>% arrange(min_n),
			grid_model$data[[i]]
		)

		for (j in seq_along(sched_pre_model$model_stage[[i]]$predict_stage)) {
			predict_j <- model_i$predict_stage[[j]]

			# We need to figure out the trees that need predicting for the current
			# set of other parameters.

			# Get the settings that have already be resolved:
			other_ij <-
				model_i %>%
				select(-predict_stage, -trees) %>%
				slice(j) %>%
				vctrs::vec_cbind(
					sched_pre_model %>%
						select(threshold, disp_df) %>%
						slice(i)
				)
			# What are the matching values from the grid?
			trees_ij <-
				grid_pre_model %>%
				inner_join(other_ij, by = c("min_n", "threshold", "disp_df")) %>%
				select(trees)


			expect_equal(
				predict_j %>% select(trees) %>% arrange(trees),
				trees_ij %>% arrange(trees)
			)

		}
	}

	expect_s3_class(
		sched_pre_model,
		c("grid_schedule", "schedule", "tbl_df", "tbl", "data.frame")
	)
})

test_that("grid processing schedule - recipe + model + tailor, submodels, irregular grid", {
	skip_if_not_installed("splines2")
	skip_if_not_installed("probably")

	wflow_pre_model_post <- workflow(rec_tune, mod_tune, tlr_tune)
	grid_pre_model_post <-
		extract_parameter_set_dials(wflow_pre_model_post) %>%
		update(lower_limit = dials::lower_limit(c(0, 1))) %>%
		dials::grid_regular() %>%
		# This will make the submodel parameter (trees) unbalanced for some
		# combination of parameters of the other parameters.
		slice(seq(1, 240, by = 7))

	grid_pre <-
		grid_pre_model_post %>%
		distinct(threshold, disp_df)

	grid_model <-
		grid_pre_model_post %>%
		select(-lower_limit) %>%
		dplyr::group_nest(threshold, disp_df) %>%
		mutate(
			data = purrr::map(
				data,
				~ .x %>% dplyr::summarize(trees = max(trees), .by = c(min_n))
			),
			data = purrr::map(data, ~ .x %>% arrange(min_n))
		)

	
	sched_pre_model_post <- schedule_grid(
		grid_pre_model_post,
		wflow_pre_model_post
	)


	expect_named(sched_pre_model_post, c("threshold", "disp_df", "model_stage"))
	expect_equal(
		sched_pre_model_post %>% select(-model_stage) %>% tibble::as_tibble(),
		grid_pre
	)

	for (i in seq_along(sched_pre_model_post$model_stage)) {
		model_i <- sched_pre_model_post$model_stage[[i]]

		# Get the current set of preproc parameters to remove
		other_i <-
			sched_pre_model_post[i,] %>%
			dplyr::select(-model_stage)

		# We expect to evaluate these specific models for this set of preprocessors
		exp_i <-
			grid_pre_model_post %>%
			inner_join(other_i, by = c("threshold", "disp_df")) %>%
			arrange(trees, min_n, lower_limit) %>%
			select(trees, min_n, lower_limit)

		# What we will evaluate:
		subgrid_i <-
			model_i %>%
			select(-trees) %>%
			unnest(predict_stage) %>%
			unnest(post_stage) %>%
			arrange(trees, min_n, lower_limit) %>%
			select(trees, min_n, lower_limit)

		expect_equal(subgrid_i, exp_i)

		for (j in seq_along(sched_pre_model_post$model_stage[[i]]$predict_stage)) {
			model_ij <- model_i[j,]
			expect_named(model_ij, c("trees", "min_n", "predict_stage"))

			predict_j <- model_ij$predict_stage[[1]]
			expect_named(predict_j, c("trees", "post_stage"))

			exp_post_grid <-
				# Condition on the current set of non-submodel or post param to see
				# what we should be evaluating:
				model_ij %>%
				dplyr::select(-trees) %>%
				vctrs::vec_cbind(other_i) %>%
				dplyr::inner_join(
					grid_pre_model_post,
					by = c("threshold", "disp_df", "min_n")
				) %>%
				dplyr::select(trees, lower_limit) %>%
				dplyr::arrange(trees, lower_limit)

			# Which as scheduled to be evaluated:
			subgrid_ij <-
				predict_j %>%
				unnest(post_stage) %>%
				dplyr::arrange(trees, lower_limit)

			expect_equal(subgrid_ij, exp_post_grid)
		}
	}

	expect_s3_class(
		sched_pre_model_post,
		c("grid_schedule", "schedule", "tbl_df", "tbl", "data.frame")
	)
})

