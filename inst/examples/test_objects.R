library(tidymodels)
library(tune)

# ------------------------------------------------------------------------------

set.seed(455)
folds <- vfold_cv(mtcars, v = 5)

simple_rec <- recipe(mpg ~ ., data = mtcars)

form <- mpg ~ .

spline_rec <-
  recipe(mpg ~ ., data = mtcars) %>%
  step_normalize(all_predictors()) %>%
  step_bs(disp, deg_free = tune())

lm_mod <- linear_reg() %>% set_engine("lm")

knn_mod <-
  nearest_neighbor(mode = "regression", neighbors = tune()) %>%
  set_engine("kknn")

verb <- TRUE
g_ctrl <- grid_control(verbose = verb, save_pred = TRUE)
b_ctrl <- Bayes_control(verbose = verb, save_pred = TRUE)


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
mt_spln_lm_grid <-
  tune_grid(mt_spln_lm,
            resamples = folds,
            control = g_ctrl)

set.seed(8825)
mt_spln_lm_bo <-
  tune_Bayes(
    mt_spln_lm,
    resamples = folds,
    iter = 3,
    control = b_ctrl
  )

set.seed(8825)
mt_spln_lm_grid_sep <-
  tune_grid(
    spline_rec,
    lm_mod,
    resamples = folds,
    control = g_ctrl
  )

set.seed(8825)
mt_spln_lm_bo_sep <-
  tune_Bayes(
    spline_rec,
    lm_mod,
    resamples = folds,
    iter = 3,
    control = b_ctrl
  )

# ------------------------------------------------------------------------------

set.seed(8825)
mt_spln_knn_grid <-
  tune_grid(
    mt_spln_knn,
    resamples = folds,
    grid = grid_regular(parameters(mt_spln_knn)),
    control = g_ctrl
  )

set.seed(8825)
mt_spln_knn_bo <-
  tune_Bayes(mt_spln_knn,
             resamples = folds,
             iter = 3,
             control = b_ctrl)


set.seed(8825)
mt_spln_knn_grid_sep <-
  tune_grid(
    spline_rec,
    knn_mod,
    resamples = folds,
    grid = grid_regular(parameters(mt_spln_knn)),
    control = g_ctrl
  )

set.seed(8825)
mt_spln_knn_bo_sep <-
  tune_Bayes(spline_rec,
             knn_mod,
             resamples = folds,
             iter = 3,
             control = b_ctrl)

# ------------------------------------------------------------------------------

set.seed(8825)
mt_knn_grid <- tune_grid(mt_knn, resamples = folds, control = g_ctrl)

set.seed(8825)
mt_knn_bo <-
  tune_Bayes(mt_knn,
             resamples = folds,
             iter = 3,
             control = b_ctrl)

set.seed(8825)
mt_knn_grid_sep <-
  tune_grid(simple_rec, knn_mod, resamples = folds, control = g_ctrl)

set.seed(8825)
mt_knn_bo_sep <-
  tune_Bayes(simple_rec,
             knn_mod,
             resamples = folds,
             iter = 3,
             control = b_ctrl)

set.seed(8825)
mt_knn_grid_form <-
  tune_grid(form, knn_mod, resamples = folds, control = g_ctrl)

set.seed(8825)
mt_knn_bo_form <-
  tune_Bayes(form,
             knn_mod,
             resamples = folds,
             iter = 3,
             control = b_ctrl)

# ------------------------------------------------------------------------------

options(warn = 2, error = traceback)
set.seed(8825)
mt_spln_lm_grid_fails <-
  tune_grid(mt_spln_lm,
            resamples = folds,
            control = g_ctrl)


# ------------------------------------------------------------------------------

save(
  list = grep("^mt_", ls(), value = TRUE),
  file = "tests/testthat/test_objects.RData",
  version = 2,
  compress = "xz"
)

