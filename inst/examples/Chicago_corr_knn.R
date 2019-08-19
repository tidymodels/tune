library(tidymodels)
library(workflows)
library(tune)
library(kknn)

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
  step_corr(one_of(!!stations), threshold = tune())


knn_model <-
  nearest_neighbor(mode = "regression", neighbors = tune(), weight_func = tune(), dist_power = tune()) %>%
  set_engine("kknn")

chi_wflow <-
  workflow() %>%
  add_recipe(chi_rec) %>%
  add_model(knn_model)

chi_param <-
  param_set(chi_wflow) %>%
  update(id = "threshold", threshold(c(.8, .99))) %>%
  update(id = "neighbors", neighbors(c(1, 50))) %>%
  update(id = "dist_power", dist_power())


chi_grid <-
  chi_param %>%
  grid_latin_hypercube(size = 6)

res <- tune_grid(chi_wflow, data_folds, chi_grid, control = grid_control(verbose = TRUE))

estimate(res) %>%
  dplyr::filter(.metric == "rmse") %>%
  select(-n, -std_err, -.estimator, -.metric) %>%
  ggplot(aes(x = neighbors, y = mean, col = weight_func)) +
  geom_point() + geom_line() +
  facet_wrap(~threshold, scales = "free_x")

estimate(res) %>%
  dplyr::filter(.metric == "rmse") %>%
  arrange(mean) %>%
  slice(1)

test <-
  tune_Bayes(
    chi_wflow,
    data_folds,
    param_info = chi_param,
    initial = res,
    metrics = metric_set(rmse, rsq),
    iter = 10,
    control = Bayes_control(verbose = TRUE, random_value = 3)
  )
