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
