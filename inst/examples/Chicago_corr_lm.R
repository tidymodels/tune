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


lm_model <-
  linear_reg(mode = "regression") %>%
  set_engine("lm")

chi_wflow <-
  workflow() %>%
  add_recipe(chi_rec) %>%
  add_model(lm_model)

chi_grid <-
  param_set(chi_wflow) %>%
  update(id = "threshold", threshold(c(.8, .99))) %>%
  grid_regular(levels = 10)


res <- tune_grid(chi_wflow, data_folds, chi_grid, control = grid_control(verbose = TRUE))

summarize(res) %>%
  dplyr::filter(.metric == "rmse") %>%
  select(-n, -std_err, -.estimator, -.metric) %>%
  ggplot(aes(x = threshold, y = mean)) +
  geom_point() +
  geom_line()

summarize(res) %>%
  dplyr::filter(.metric == "rmse") %>%
  arrange(mean) %>%
  slice(1)

