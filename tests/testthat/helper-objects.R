context("creating test objects")

# ------------------------------------------------------------------------------

library(recipes)
library(parsnip)
library(workflows)
data("Chicago", package = "dials")

# ------------------------------------------------------------------------------

isomap_rec <-
  recipe(ridership ~ ., data = head(Chicago)) %>%
  step_date(date) %>%
  step_holiday(date) %>%
  step_rm(date, ends_with("away")) %>%
  # step_knnimpute(all_predictors(), neighbors = tune("imputation")) %>%
  step_other(all_nominal(), threshold = tune()) %>%
  step_dummy(all_nominal()) %>%
  step_normalize(all_predictors()) %>%
  step_isomap(all_predictors(), num_terms = tune(), neighbors = tune())

bare_rec <-
  recipe(ridership ~ ., data = head(Chicago))

rm_rec <-
  recipe(ridership ~ ., data = head(Chicago)) %>%
  step_rm(date, ends_with("away"))

ununtable_rec <-
  recipe(ridership ~ ., data = head(Chicago)) %>%
  step_dummy(all_nominal(), naming = tune())

# ------------------------------------------------------------------------------

bst_model <-
  boost_tree(mode = "regression") %>%
  set_engine("C5.0", rules = TRUE, noGlobalPruning = TRUE)

lm_model <-
  linear_reg() %>%
  set_engine("lm")

no_engine <- linear_reg()

weird_annotation <-
  boost_tree(mode = "regression", trees = tune("$^%&#!")) %>%
  set_engine("C5.0", rules = TRUE, noGlobalPruning = TRUE)

