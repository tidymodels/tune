check_predictions <- function(split, pred, tune_df, obj) {
  assess <- rsample::assessment(split)
  n_te <- nrow(assess)
  n_pm <- nrow(tune_df)
  ind_te <- as.integer(split, data = "assessment")
  expect_true(tibble::is_tibble(pred))
  expect_equal(nrow(pred), n_te * n_pm)
  exp_nms <- c("mpg", ".pred", ".row", .get_tune_parameter_names(obj), ".config")
  expect_equal(names(pred), exp_nms)
  expect_equal(sort(unique(ind_te)), sort(unique(pred$.row)))
  TRUE
}
