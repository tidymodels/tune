library(tidymodels)
library(workflows)
library(tune)
library(kknn)
library(doMC)
registerDoMC(cores=20)
# load("~/Downloads/chi_corr_knn_search.RData")
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
  update(id = "threshold", threshold(c(.8, .99)))

chi_grid <-
  chi_param %>%
  grid_latin_hypercube(size = 18)

res <- tune_grid(chi_wflow, rs = data_folds, grid = chi_grid, control = grid_control(verbose = TRUE))

# summarize(res) %>%
#   dplyr::filter(.metric == "rmse") %>%
#   select(-n, -std_err, -.estimator, -.metric) %>%
#   ggplot(aes(x = neighbors, y = mean, col = weight_func)) +
#   geom_point() + geom_line() +
#   facet_wrap(~threshold, scales = "free_x")

summarize(res) %>%
  dplyr::filter(.metric == "rmse") %>%
  arrange(mean) %>%
  slice(1)

set.seed(354)
knn_search <-
  tune_Bayes(
    chi_wflow,
    rs = data_folds,
    param_info = chi_param,
    initial = res,
    perf = metric_set(rmse),
    iter = 20,
    control = Bayes_control(verbose = TRUE, uncertain = 5),
    trace = TRUE
  )

plot_perf_vs_iter(knn_search)
