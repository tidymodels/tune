library(tidymodels)
library(tune)

load(url("http://bit.ly/seg-data"))

theme_set(theme_bw())

# ------------------------------------------------------------------------------

segmentationData <-
  segmentationData %>%
  dplyr::select(-Case, -Cell, -contains("Centroid"))

set.seed(8567)
tr_te_split <- initial_split(segmentationData)

seg_train <- training(tr_te_split)
seg_test  <-  testing(tr_te_split)

val_split <- mc_cv(seg_train, times = 1)
# could also be a simple modeling/validation split

# ------------------------------------------------------------------------------

seg_pre_proc <-
  recipe(Class ~ ., data = seg_train) %>%
  step_YeoJohnson(all_predictors()) %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors(), num_comp = tune()) %>%
  step_downsample(Class)

nn_mod <-
  mlp(mode = "classification", hidden_units = tune(), dropout = tune(),
      epochs = tune(), activation = tune()) %>%
  set_engine("keras", verbose = 0, validation = .1)


nn_wflow <-
  workflow() %>%
  add_model(nn_mod) %>%
  add_recipe(seg_pre_proc)

# ------------------------------------------------------------------------------

nn_set <-
  nn_wflow %>%
  parameters() %>%
  # In case you want to manually adjust the parameter specification
  update(num_comp = num_comp(c(1, 20)))

roc_set <- metric_set(roc_auc, pr_auc)

set.seed(1558)
grid <- grid_max_entropy(nn_set, size = 5)

grid_results <- tune_grid(nn_wflow, resamples = val_split, grid = grid,
                          metrics = roc_set,
                          control = control_grid(verbose = TRUE, save_pred = TRUE))

grid_results

grid_results$.metrics[[1]]

summarize(grid_results)

# functions to pick best or other rules

# ------------------------------------------------------------------------------
foo <- function(i) {
  expo_decay(i, start_val = .05, 0, slope = 1/5)
}



nn_search <- tune_bayes(nn_wflow, resamples = val_split,
                        initial = grid_results,
                        iter = 20,
                        metrics = roc_set,
                        param_info = nn_set,
                        objective = exp_improve(foo),
                        control = control_bayes(verbose = TRUE))

autoplot(nn_search, type = "performance", metric = "roc_auc")

# ------------------------------------------------------------------------------

nn_search_2 <- tune_bayes(nn_wflow, resamples = val_split,
                           initial = nn_search,
                           iter = 15,
                           metrics = roc_set,
                           param_info = nn_set,
                           control = control_bayes(verbose = TRUE))


autoplot(nn_search_2, type = "performance", metric = "roc_auc")

# ------------------------------------------------------------------------------

nn_search_2 %>%
  summarize() %>%
  dplyr::filter(.iter > 0 & .metric == "roc_auc") %>%
  select(-.iter, -.metric, -.estimator, -n, -std_err) %>%
  gather(parameter, value, -activation, -mean) %>%
  ggplot(aes(x = value, y = mean, col = activation)) +
  geom_point(alpha = .3) +
  facet_wrap(~ parameter, scales = "free_x")

nn_search_2 %>%
  summarize() %>%
  dplyr::filter(.iter > 0 & .metric == "roc_auc") %>%
  select(-.metric, -.estimator, -n, -std_err) %>%
  gather(parameter, value, -activation, -mean, -.iter) %>%
  ggplot(aes(x = .iter, y = value, col = activation, size = mean)) +
  geom_point(alpha = .3) +
  facet_wrap(~ parameter, scales = "free_y")
