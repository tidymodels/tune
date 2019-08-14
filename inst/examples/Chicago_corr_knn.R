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
  step_corr(one_of(!!stations), threshold = tune())


knn_model <-
  nearest_neighbor(mode = "regression", neighbors = tune(), weight_func = tune()) %>%
  set_engine("kknn")

chi_wflow <-
  workflow() %>%
  add_recipe(chi_rec) %>%
  add_model(knn_model)

res <- tune_grid(chi_wflow, data_folds, control = list(verbose = TRUE))

summarizer(res) %>%
  dplyr::filter(.metric == "rmse") %>%
  select(-n, -std_err, -.estimator, -.metric) %>%
  gather(parameter, value, -mean, -weight_func) %>%
  ggplot(aes(x = value, y = mean, col = weight_func)) +
  geom_point() +
  facet_wrap(~parameter, scales = "free_x")

summarizer(res) %>%
  dplyr::filter(.metric == "rmse") %>%
  arrange(mean) %>%
  slice(1)

