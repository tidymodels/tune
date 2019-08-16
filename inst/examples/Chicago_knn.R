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


res <- tune_grid(chi_wflow, data_folds, chi_grid, control = list(verbose = TRUE))

summarizer(res) %>%
  dplyr::filter(.metric == "rmse") %>%
  ggplot(aes(x = neighbors, y = mean, col = factor(weight_func))) +
  geom_path() +
  geom_point() +
  facet_wrap(~ dist_power)


summarizer(res) %>%
  dplyr::filter(.metric == "rmse") %>%
  arrange(mean) %>%
  slice(1)

