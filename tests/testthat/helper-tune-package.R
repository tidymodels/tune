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
