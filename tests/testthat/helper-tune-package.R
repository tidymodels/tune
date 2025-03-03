new_rng_snapshots <- utils::compareVersion("3.6.0", as.character(getRversion())) > 0

# New (as of 4.3.0) a new option generates different snapshots
rankdeficient_version <- any(names(formals("predict.lm")) == "rankdeficient")


helper_objects_tune <- function() {
  rec_tune_1 <-
    recipes::recipe(mpg ~ ., data = mtcars) %>%
    recipes::step_normalize(recipes::all_predictors()) %>%
    recipes::step_pca(recipes::all_predictors(), num_comp = tune())

  rec_no_tune_1 <-
    recipes::recipe(mpg ~ ., data = mtcars) %>%
    recipes::step_normalize(recipes::all_predictors())

  lm_mod <- parsnip::linear_reg() %>% parsnip::set_engine("lm")

  svm_mod <- parsnip::svm_rbf(mode = "regression", cost = tune()) %>%
    parsnip::set_engine("kernlab")

  list(
    rec_tune_1 = rec_tune_1,
    rec_no_tune_1 = rec_no_tune_1,
    lm_mod = lm_mod,
    svm_mod = svm_mod
  )
}

# When not interactive, the cli progress bar is occasionally "reset" at random
# intervals throughout tuning, which means that intermediate counts may appear
# in snapshots. In an output like:
#
# > A | error:   AHHhH
# There were issues with some computations   A: x2
# There were issues with some computations   A: x4
# There were issues with some computations   A: x5
# There were issues with some computations   A: x5
#
# ...we just want to see the unique issues, e.g.
#
# > A | error:   AHHhH
catalog_lines <- function(lines) {
  lines[grepl("^>", lines)]
}

# Make a new binding to prevent infinite recursion when the original is mocked.
initialize_catalog_ <- tune:::initialize_catalog

# Sets a new exit handler on `initialize_catalog()` that stores the summary
# of issues before it's cleared along with the progress bar. Together with
# the above, we can test the full catalog output.
redefer_initialize_catalog <- function(test_env) {
  local({
    function(control, env = rlang::caller_env()) {
      initialize_catalog_(control, env)

      withr::defer(
        assign(
          "catalog_summary_test",
          tune:::tune_env$progress_env$catalog_summary,
          test_env
        ),
        envir = env,
        priority = "first"
      )

      NULL
    }
  })
}

# ------------------------------------------------------------------------------
# Objects to test grid processing

if (rlang::is_installed("splines2")) {
	rec_no_tune <-
		recipes::recipe(mpg ~ ., data = mtcars) %>%
		recipes::step_corr(all_predictors(), threshold = .1) %>%
		recipes::step_spline_natural(disp, deg_free = 5)

	rec_tune <-
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
