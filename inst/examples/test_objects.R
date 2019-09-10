library(tidymodels)
library(workflows)
library(tune)

# ------------------------------------------------------------------------------

set.seed(455)
folds <- vfold_cv(mtcars, v = 5)

simple_rec <- recipe(mpg ~ ., data = mtcars)

spline_rec <-
  recipe(mpg ~ ., data = mtcars) %>%
  step_normalize(all_predictors()) %>%
  step_bs(disp, deg_free = tune())

lm_mod <- linear_reg() %>% set_engine("lm")

knn_mod <-
  nearest_neighbor(mode = "regression", neighbors = tune()) %>%
  set_engine("kknn")

# ------------------------------------------------------------------------------

mt_spln_lm <-
  workflow() %>%
  add_recipe(spline_rec) %>%
  add_model(lm_mod)

mt_spln_knn <-
  workflow() %>%
  add_recipe(spline_rec) %>%
  add_model(knn_mod)

mt_knn <-
  workflow() %>%
  add_recipe(simple_rec) %>%
  add_model(knn_mod)

# ------------------------------------------------------------------------------

set.seed(8825)
mt_spln_lm_grid <- tune_grid(mt_spln_lm, folds, control = grid_control(save_pred = TRUE))

set.seed(8825)
mt_spln_lm_bo <- tune_Bayes(mt_spln_lm, folds, iter = 3, control = Bayes_control(save_pred = TRUE))

# ------------------------------------------------------------------------------

set.seed(8825)
mt_spln_knn_grid <- tune_grid(mt_spln_knn, folds,
                              grid = grid_regular(param_set(mt_spln_knn)),
                              control = grid_control(save_pred = TRUE))

set.seed(8825)
mt_spln_knn_bo <- tune_Bayes(mt_spln_knn, folds, iter = 3, control = Bayes_control(save_pred = TRUE))

# ------------------------------------------------------------------------------

set.seed(8825)
mt_knn_grid <- tune_grid(mt_knn, folds, control = grid_control(save_pred = TRUE))

set.seed(8825)
mt_knn_bo <- tune_Bayes(mt_knn, folds, iter = 3, control = Bayes_control(save_pred = TRUE))

# ------------------------------------------------------------------------------

options(warn = 2, error = traceback)
set.seed(8825)
mt_spln_lm_grid_fails <- tune_grid(mt_spln_lm, folds, control = grid_control(save_pred = TRUE))


# ------------------------------------------------------------------------------




save.image(file = "tests/testthat/test_objects.RData", version = 2, compress = "xz")








