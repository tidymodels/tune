library(tidymodels)
library(workflows)
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

folds <- vfold_cv(seg_train)
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
  param_set() %>%
  # In case you want to manually adjust the parameter specification
  update("num_comp", num_comp(c(1, 20)))

set.seed(1558)
grid <- grid_max_entropy(nn_set, size = 5)

grid_results <- tune_grid(nn_wflow, rs = folds, grid = grid,
                          control = grid_control(verbose = TRUE))

grid_results

grid_results$.metrics[[1]]

summarize(grid_results)

# functions to pick best or other rules

# ------------------------------------------------------------------------------
foo <- function(i) {
  expo_decay(i, start_val = .05, 0, slope = 1/5)
}


kappa_only <- metric_set(kap)

nn_search <- tune_Bayes(nn_wflow, rs = folds,
                        initial = grid_results,
                        iter = 20,
                        perf = kappa_only,
                        param_info = nn_set,
                        objective = exp_improve(foo),
                        control = Bayes_control(verbose = TRUE, uncertain = 20))

plot_perf_vs_iter(nn_search, "kap")

# ------------------------------------------------------------------------------

nn_search_2 <- tune_Bayes(nn_wflow, rs = folds,
                           initial = nn_search,
                           iter = 15,
                           perf = kappa_only,
                           param_info = nn_set,
                           control = Bayes_control(verbose = TRUE))

plot_perf_vs_iter(nn_search_2, "kap")

# ------------------------------------------------------------------------------

nn_search_2 %>% dplyr::filter(.iter > 0) %>%
  ggplot(aes(x = cost, y = rbf_sigma)) +
  geom_path(aes(x = cost, y = rbf_sigma), col = "black") +
  geom_point(aes(col = num_comp, size = mean))  +
  geom_point(data = nn_search_2, aes(col = num_comp, size = mean), pch = 1) +
  scale_x_log10() +
  scale_y_log10()

nn_search_2 %>%
  dplyr::filter(.metric == "kap") %>%
  dplyr::select(-.estimator, -.metric, -n, -std_err) %>%
  mutate(cost = log10(cost), rbf_sigma = log10(rbf_sigma)) %>%
  gather(variable, value, -mean, -.iter) %>%
  ggplot(aes(x = .iter, y = value)) +
  geom_point() +
  facet_wrap(~ variable, scales = "free_y")


nn_search_2 %>%
  dplyr::filter(.metric == "kap") %>%
  dplyr::select(-.estimator, -.metric, -n, -std_err) %>%
  mutate(cost = log10(cost), rbf_sigma = log10(rbf_sigma)) %>%
  gather(variable, value, -mean, -.iter) %>%
  ggplot(aes(x = value, y = mean)) +
  geom_point() +
  facet_wrap(~ variable, scales = "free_x")


