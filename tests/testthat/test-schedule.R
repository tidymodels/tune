if (rlang::is_installed("splines2")) {
	rec_no_tune <-
		recipes::recipe(mpg ~ ., data = mtcars) %>%
		recipes::step_corr(all_predictors(), threshold = .1) %>%
		recipes::step_spline_natural(disp, deg_free = 5)

	rec_tune_2 <-
		recipes::recipe(mpg ~ ., data = mtcars) %>%
		recipes::step_corr(all_predictors(), threshold = tune()) %>%
		recipes::step_spline_natural(disp, deg_free = tune("disp_df"))
}

mod_no_tune <-
	parsnip::rand_forest(mode = "regression")

mod_tune <-
	parsnip::boost_tree(
		trees = tune(),
		min_n = tune(),
		mode = "regression"
	)

mod_tune_submodel <-
	parsnip::boost_tree(
		trees = tune(),
		mode = "regression"
	)

mod_tune_no_submodel <-
	parsnip::rand_forest(
		min_n = tune(),
		mode = "regression"
	)

if (rlang::is_installed("probably")) {
	tlr_tune <-
		tailor::tailor() %>%
		tailor::adjust_numeric_range(lower_limit = tune())

	tlr_tune_cal <-
		tailor::tailor() %>%
		tailor::adjust_numeric_calibration(method = "linear") %>%
		tailor::adjust_numeric_range(lower_limit = tune())

	tlr_no_tune <-
		tailor::tailor() %>%
		tailor::adjust_numeric_range(lower_limit = 0)
}

# `get_param_info()` -----------------------------------------------------

test_that("`get_param_info()` works for a workflow without tags for tuning", {
	wflow <- workflow(mpg ~ ., parsnip::linear_reg())
	param_info <- get_param_info(wflow)

	expect_named(param_info, c("name", "id", "source", "has_submodel"))
	expect_identical(nrow(param_info), 0L)
})

test_that("`get_param_info()` works for a workflow with tags for tuning", {
	# tuning tags in all components
	rec_tune_2 <- recipes::recipe(mpg ~ ., data = mtcars) %>%
		recipes::step_corr(recipes::all_predictors(), threshold = tune()) %>%
		recipes::step_spline_natural(disp, deg_free = tune("disp_df"))

	mod_tune_no_submodel <- parsnip::rand_forest(
		min_n = tune(),
		mode = "regression"
	)

	tlr_tune <- tailor::tailor() %>%
		tailor::adjust_numeric_range(lower_limit = tune())

	wflow <- workflow(rec_tune_2, mod_tune_no_submodel, tlr_tune)

	param_info <- get_param_info(wflow)

	expect_named(param_info, c("name", "id", "source", "has_submodel"))
	expect_identical(
		param_info$id,
		c("min_n", "threshold", "disp_df", "lower_limit")
	)
	expect_identical(param_info$has_submodel, c(FALSE, NA, NA, NA))
})

test_that("`get_param_info()` works when there are submodel parameters", {
	# tuning tags only in model spec
	rec_no_tuning <- recipes::recipe(mpg ~ ., data = mtcars)

	mod_tune_submodel <- parsnip::boost_tree(
		trees = tune(),
		min_n = tune(),
		mode = "regression"
	)

	tlr_no_tune <- tailor::tailor() %>%
		tailor::adjust_numeric_range(lower_limit = 0)

	wflow <- workflow(rec_no_tuning, mod_tune_submodel, tlr_no_tune)

	param_info <- get_param_info(wflow)

	expect_named(param_info, c("name", "id", "source", "has_submodel"))
	expect_identical(param_info$name, c("trees", "min_n"))
	expect_identical(param_info$has_submodel, c(TRUE, FALSE))
})


# `schedule_predict_stage_i()` -------------------------------------------

test_that("`schedule_predict_stage_i()` works with: no submodel, no post-processing", {
	wflow <- workflow(mpg ~ ., mod_no_tune, tlr_no_tune)
	param_info <- get_param_info(wflow)
	grid_predict_stage <- tibble::tibble()

	schedule <- schedule_predict_stage_i(grid_predict_stage, param_info)
	expect_named(schedule, c("post_stage"))
	expect_identical(nrow(schedule), 0L)
})

test_that("`schedule_predict_stage_i()` works with: no submodel, with post-processing", {
	wflow <- workflow(mpg ~ ., mod_no_tune, tlr_tune)
	param_info <- get_param_info(wflow)
	grid_predict_stage <- tibble::tibble(lower_limit = 1:2)

	schedule <- schedule_predict_stage_i(grid_predict_stage, param_info)
	expect_named(schedule, c("post_stage"))
	expect_identical(nrow(schedule), 1L)
	expect_identical(schedule$post_stage[[1]], grid_predict_stage)
})

test_that("`schedule_predict_stage_i()` works with: with submodel, no post-processing", {
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
	wflow <- workflow(mpg ~ ., mod_no_tune, tlr_no_tune)
	param_info <- get_param_info(wflow)
	grid_model_stage <- tibble::tibble()

	schedule <- schedule_model_stage_i(grid_model_stage, param_info, wflow)
	expect_named(schedule, c("predict_stage"))
	expect_identical(nrow(schedule), 0L)
})

test_that("`schedule_model_stage_i()` works with only non-submodel: with non-submodel, no submodel, no post", {
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
		purrr::map(schedule$predict_stage, names) %>% purrr::list_c() %>% unique(),
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
		purrr::map(schedule$predict_stage, names) %>% purrr::list_c() %>% unique(),
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
	wflow <- workflow(mpg ~ ., mod_no_tune, tlr_no_tune)
	grid <- tibble::tibble()

	schedule <- schedule_stages(grid, wflow)
	expect_named(schedule, c("model_stage"))
	expect_identical(nrow(schedule), 0L)
})

test_that("`schedule_stages()` works with preprocessing", {
	wflow <- workflow(rec_tune_2, mod_no_tune, tlr_no_tune)
	grid <- tibble::tibble(threshold = rep(1:2, each = 2), disp_df = c(1:2, 1:2))

	schedule <- schedule_stages(grid, wflow)
	expect_named(schedule, c("threshold", "disp_df", "model_stage"))
	expect_identical(nrow(schedule), 4L)
	expect_identical(
		schedule %>% dplyr::select(-model_stage),
		grid
	)
})

