
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
