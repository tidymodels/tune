library(tidymodels)
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
  step_corr(all_of(!!stations), threshold = tune())


svm_mod <-
  svm_rbf(mode = "regression", cost = tune(), rbf_sigma = tune(), margin = tune()) %>%
  set_engine("kernlab")

chi_wflow <-
  workflow() %>%
  add_recipe(chi_rec) %>%
  add_model(svm_mod)

cor_mat <- Chicago %>% dplyr::select(all_of(stations)) %>% cor()
cor_mat <- tibble(cor = cor_mat[upper.tri(cor_mat)])
ggplot(cor_mat, aes(x = cor)) + geom_histogram(binwidth = .01, col = "white")

chi_set <-
  parameters(chi_wflow) %>%
  update(threshold = threshold(c(.8, .99)))

chi_grid <-
  chi_set %>%
  grid_max_entropy(size = 5)


ext <- function(x) {
  tibble(num_sv = x$model@nSV)
}

res <- tune_grid(chi_wflow, resamples = data_folds, grid = chi_grid, control = control_grid(verbose = TRUE, save_pred = TRUE, extract = ext))


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

foo <- function(i) {
  expo_decay(i, start_val = .5, 0, slope = 1/10)
}

svm_search <-
  tune_bayes(
    chi_wflow,
    rs = data_folds,
    param_info = chi_set,
    initial = res,
    metrics = metric_set(rmse, rsq),
    objective = exp_improve(foo),
    iter = 50,
    control = control_bayes(verbose = TRUE, uncertain = 3)
  )

svm_search_2 <-
  tune_bayes(
    chi_wflow,
    rs = data_folds,
    param_info = chi_set,
    initial = svm_search,
    metrics = metric_set(rmse, rsq),
    iter = 20,
    control = control_bayes(verbose = TRUE)
  )
