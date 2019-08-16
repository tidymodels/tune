library(tidymodels)
library(workflows)
library(tune)

# ------------------------------------------------------------------------------

set.seed(7898)
data_folds <- rolling_origin(Chicago, initial = 364 * 15, assess = 7 * 4, skip = 13, cumulative = FALSE)

# ------------------------------------------------------------------------------

stations <- names(Chicago)[2:21]

chi_rec <-
  recipe(ridership ~ ., data = Chicago) %>%
  step_holiday(date) %>%
  step_date(date) %>%
  step_rm(date) %>%
  step_dummy(all_nominal()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_predictors())

knn_model <-
  nearest_neighbor(
    mode = "regression",
    neighbors = tune(),
    weight_func = tune(),
    dist_power = tune()
  ) %>%
  set_engine("kknn")


chi_wflow <-
  workflow() %>%
  add_recipe(chi_rec) %>%
  add_model(knn_model)

set.seed(255)
chi_grid <-
  chi_wflow %>%
  param_set %>%
  update(id = "neighbors", neighbors(c(1, 30))) %>%
  update(id = "dist_power", dist_power(c(1/4, 2))) %>%
  grid_regular(levels = c(30, 3, 3))


reg_knn_grid <- tune_grid(chi_wflow, data_folds, chi_grid, control = grid_control(verbose = TRUE))

estimate(reg_knn_grid) %>%
  dplyr::filter(.metric == "rmse") %>%
  ggplot(aes(x = neighbors, y = mean, col = factor(weight_func))) +
  geom_path() +
  geom_point() +
  facet_wrap(~ dist_power)


estimate(reg_knn_grid) %>%
  dplyr::filter(.metric == "rmse") %>%
  arrange(mean) %>%
  slice(1)

# ------------------------------------------------------------------------------

chi_set <-
  chi_wflow %>%
  param_set %>%
  update(id = "neighbors", neighbors(c(1, 30))) %>%
  update(id = "dist_power", dist_power(c(1/10, 2)))

set.seed(255)
smol_grid <-
  chi_set %>%
  grid_random(size = 5)


smol_knn_grid <- tune_grid(chi_wflow, data_folds, smol_grid, control = grid_control(verbose = TRUE))

knn_search <-
  tune_Bayes(
    chi_wflow,
    data_folds,
    param_info = chi_set,
    initial = estimate(smol_knn_grid),
    metrics = metric_set(rmse, rsq),
    iter = 10,
    control = Bayes_control(verbose = TRUE, random_value = Inf)
  )

