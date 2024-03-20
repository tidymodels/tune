library(tidymodels)
library(scales)
library(censored)
library(sessioninfo)
library(testthat)
# also will require prodlim, mboost, kknn, and kernlab

# ------------------------------------------------------------------------------
# "mt_*" test objects used in test-predictions.R, test-extract.R, and test-autoplot.R

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

knn_mod_two <-
  nearest_neighbor(mode = "regression", neighbors = tune("K"), weight_func = tune()) %>%
  set_engine("kknn")

get_coefs  <- function(x) {
  x %>%
    extract_fit_parsnip() %>%
    tidy()
}

verb <- FALSE
g_ctrl <- control_grid(verbose = verb, save_pred = TRUE, extract = get_coefs)
b_ctrl <- control_bayes(verbose = verb, save_pred = TRUE, extract = get_coefs)

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
  tune_bayes(
    mt_spln_lm,
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
    grid = grid_regular(extract_parameter_set_dials(mt_spln_knn)),
    control = g_ctrl
  )

set.seed(8825)
mt_spln_knn_bo <-
  tune_bayes(mt_spln_knn,
             resamples = folds,
             iter = 3,
             control = b_ctrl)

set.seed(8825)
mt_spln_knn_bo_sep <-
  tune_bayes(knn_mod_two,
             spline_rec,
             resamples = folds,
             iter = 3,
             control = b_ctrl)

# ------------------------------------------------------------------------------

set.seed(8825)
mt_knn_grid <- tune_grid(mt_knn, resamples = folds, control = g_ctrl)

set.seed(8825)
mt_knn_bo <-
  tune_bayes(mt_knn,
             resamples = folds,
             iter = 3,
             control = b_ctrl)

# ------------------------------------------------------------------------------

save(
  list = grep("^mt_", ls(), value = TRUE),
  file = test_path("data", "test_objects.RData"),
  version = 2,
  compress = "xz"
)

# ------------------------------------------------------------------------------
# "knn_*" test objects used in test-predictions.R, test-autoplot.R, test-GP.R
# and test-select_best.R

data(two_class_dat, package = "modeldata")
set.seed(7898)
data_folds <- vfold_cv(two_class_dat, repeats = 5)

two_class_rec <-
  recipe(Class ~ ., data = two_class_dat) %>%
  step_normalize(A, B)

knn_model <-
  nearest_neighbor(
    mode = "classification",
    neighbors = tune("K"),
    weight_func = tune(),
    dist_power = tune("exponent")
  ) %>%
  set_engine("kknn")

two_class_wflow <-
  workflow() %>%
  add_recipe(two_class_rec) %>%
  add_model(knn_model)

two_class_set <-
  extract_parameter_set_dials(two_class_wflow) %>%
  update(K = neighbors(c(1, 50))) %>%
  update(exponent = dist_power(c(1 / 10, 2)))

set.seed(2494)
two_class_grid <-
  two_class_set %>%
  grid_max_entropy(size = 10)

class_metrics <- metric_set(roc_auc, accuracy, kap, mcc)

knn_results <-
  tune_grid(
    two_class_wflow,
    resamples = data_folds,
    grid = two_class_grid,
    metrics = class_metrics
  )


knn_set <- two_class_set

knn_gp <-
  tune:::fit_gp(dat = collect_metrics(knn_results),
                pset = knn_set,
                metric = "accuracy",
                control = control_bayes()
  )

saveRDS(
  knn_results,
  file = testthat::test_path("data", "knn_results.rds"),
  version = 2,
  compress = "xz"
)

saveRDS(
  two_class_set,
  file = testthat::test_path("data", "knn_set.rds"),
  version = 2,
  compress = "xz"
)

saveRDS(
  two_class_grid,
  file = testthat::test_path("data", "knn_grid.rds"),
  version = 2,
  compress = "xz"
)

saveRDS(
  knn_set,
  file = testthat::test_path("data", "knn_set.rds"),
  version = 2,
  compress = "xz"
)

saveRDS(
  knn_gp,
  file = testthat::test_path("data", "knn_gp.rds"),
  version = 2,
  compress = "xz"
)

# ------------------------------------------------------------------------------
# "svm_*" test objects used in numerous test files

svm_model <-
  svm_poly(
    mode = "classification",
    cost = tune(),
    degree = tune("%^*#"),
    scale_factor = tune()
  ) %>%
  set_engine("kernlab")

two_class_wflow <-
  workflow() %>%
  add_recipe(two_class_rec) %>%
  add_model(svm_model)

two_class_set <-
  extract_parameter_set_dials(two_class_wflow) %>%
  update(cost = cost(c(-10, 4)))

set.seed(2494)
two_class_grid <-
  two_class_set %>%
  grid_max_entropy(size = 5)

class_only <- metric_set(accuracy, kap, mcc)

svm_results <-
  tune_grid(
    two_class_wflow,
    resamples = data_folds,
    grid = two_class_grid,
    metrics = class_only,
    control = control_grid(save_pred = TRUE)
  )

saveRDS(
  svm_results,
  file = testthat::test_path("data", "svm_results.rds"),
  version = 2,
  compress = "xz"
)

two_class_reg_grid <-
  two_class_set %>%
  grid_regular(levels = c(5, 4, 2))

svm_reg_results <-
  tune_grid(
    two_class_wflow,
    resamples = data_folds,
    grid = two_class_reg_grid,
    metrics = class_only,
    control = control_grid(save_pred = TRUE)
  )

saveRDS(
  svm_reg_results,
  file = testthat::test_path("data", "svm_reg_results.rds"),
  version = 2,
  compress = "xz"
)

# ------------------------------------------------------------------------------

set.seed(7898)
data_folds <- vfold_cv(mtcars, repeats = 2)

# ------------------------------------------------------------------------------
# "rcv_results" used in test-autoplot.R, test-select_best.R, and test-estimate.R

base_rec <-
  recipe(mpg ~ ., data = mtcars) %>%
  step_normalize(all_predictors())

disp_rec <-
  base_rec %>%
  step_bs(disp, degree = tune(), deg_free = tune()) %>%
  step_bs(wt, degree = tune("wt degree"), deg_free = tune("wt df"))

lm_model <-
  linear_reg(mode = "regression") %>%
  set_engine("lm")

cars_wflow <-
  workflow() %>%
  add_recipe(disp_rec) %>%
  add_model(lm_model)

cars_set <-
  cars_wflow %>%
  parameters %>%
  update(degree = degree_int(1:2)) %>%
  update(deg_free = deg_free(c(2, 10))) %>%
  update(`wt degree` = degree_int(1:2)) %>%
  update(`wt df` = deg_free(c(2, 10)))

set.seed(255)
cars_grid <-
  cars_set %>%
  grid_regular(levels = c(3, 2, 3, 2))


rcv_results <-
  tune_grid(
    cars_wflow,
    resamples = data_folds,
    grid = cars_grid,
    control = control_grid(verbose = FALSE, save_pred = TRUE)
  )

saveRDS(
  rcv_results,
  file = testthat::test_path("data", "rcv_results.rds"),
  version = 2,
  compress = "xz"
)


# ------------------------------------------------------------------------------
# Object classed with `resample_results` for use in vctrs/dplyr tests

set.seed(6735)

folds <- vfold_cv(mtcars, v = 3)

rec <- recipe(mpg ~ ., data = mtcars)

mod <- linear_reg() %>%
  set_engine("lm")

lm_resamples <- fit_resamples(mod, rec, folds)

lm_resamples

saveRDS(
  lm_resamples,
  file = testthat::test_path("data", "lm_resamples.rds"),
  version = 2,
  compress = "xz"
)

# ------------------------------------------------------------------------------
# Object classed with `iteration_results` for use in vctrs/dplyr tests

set.seed(7898)
folds <- vfold_cv(mtcars, v = 2)

rec <- recipe(mpg ~ ., data = mtcars) %>%
  step_normalize(all_predictors()) %>%
  step_ns(disp, deg_free = tune())

mod <- linear_reg(mode = "regression") %>%
  set_engine("lm")

wflow <- workflow() %>%
  add_recipe(rec) %>%
  add_model(mod)

set.seed(2934)
lm_bayes <- tune_bayes(wflow, folds, initial = 4, iter = 3)

saveRDS(
  lm_bayes,
  file = testthat::test_path("data", "lm_bayes.rds"),
  version = 2,
  compress = "xz"
)

# ------------------------------------------------------------------------------
# A single survival model

set.seed(1)
sim_dat <- prodlim::SimSurv(200) %>%
  mutate(event_time = Surv(time, event)) %>%
  select(event_time, X1, X2)

set.seed(2)
sim_rs <- vfold_cv(sim_dat)

time_points <- c(10, 1, 5, 15)

boost_spec <-
  boost_tree(trees = tune()) %>%
  set_mode("censored regression") %>%
  set_engine("mboost")

srv_mtr <-
  metric_set(
    brier_survival,
    roc_auc_survival,
    brier_survival_integrated,
    concordance_survival
  )

set.seed(2193)
surv_boost_tree_res <-
  boost_spec %>%
  tune_grid(
    event_time ~ X1 + X2,
    resamples = sim_rs,
    grid  = tibble(trees = c(1, 5, 10, 20, 100)),
    metrics = srv_mtr,
    eval_time = time_points
  )

saveRDS(
  surv_boost_tree_res,
  file = testthat::test_path("data", "surv_boost_tree_res.rds"),
  version = 2,
  compress = "xz"
)


# ------------------------------------------------------------------------------

sessioninfo::session_info()

if (!interactive()) {
  q("no")
}
