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


svm_mod <-
  svm_rbf(mode = "regression", cost = tune(), rbf_sigma = tune(), margin = tune()) %>%
  set_engine("kernlab")

chi_wflow <-
  workflow() %>%
  add_recipe(chi_rec) %>%
  add_model(svm_mod)

cor_mat <- Chicago %>% dplyr::select(one_of(stations)) %>% cor()
cor_mat <- tibble(cor = cor_mat[upper.tri(cor_mat)])
ggplot(cor_mat, aes(x = cor)) + geom_histogram(binwidth = .01, col = "white")

chi_set <-
  param_set(chi_wflow) %>%
  update(id = "threshold", threshold(c(.8, .99))) %>%
  update(id = "cost", cost(c(-10, 3)))

chi_grid <-
  chi_set %>%
  grid_max_entropy(size = 5)


res <- tune_grid(chi_wflow, data_folds, chi_grid, control = grid_control(verbose = TRUE))

estimate(res) %>%
  dplyr::filter(.metric == "rmse") %>%
  select(-n, -std_err, -.estimator, -.metric) %>%
  ggplot(aes(x = threshold, y = mean)) +
  geom_point() +
  geom_line()

estimate(res) %>%
  dplyr::filter(.metric == "rmse") %>%
  arrange(mean) %>%
  slice(1)


svm_search <-
  tune_Bayes(
    chi_wflow,
    data_folds,
    param_info = chi_set,
    initial = res,
    metrics = metric_set(rmse, rsq),
    iter = 15,
    control = Bayes_control(verbose = TRUE, random_value = 3)
  )

svm_search <-
  tune_Bayes(
    chi_wflow,
    data_folds,
    param_info = chi_set,
    initial = res,
    metrics = metric_set(rmse, rsq),
    iter = 20,
    control = Bayes_control(verbose = TRUE)
  )
