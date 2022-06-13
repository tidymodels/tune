library(tidymodels)
library(tune)
library(kknn)
library(doMC)
registerDoMC(cores=20)

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


knn_model <-
  nearest_neighbor(mode = "regression", neighbors = tune(), weight_func = tune(), dist_power = tune()) %>%
  set_engine("kknn")

chi_wflow <-
  workflow() %>%
  add_recipe(chi_rec) %>%
  add_model(knn_model)

chi_param <-
  parameters(chi_wflow) %>%
  update(
    threshold = threshold(c(.8, .99)),
    dist_power = dist_power(c(1, 2)),
    neighbors = neighbors(c(1, 50)))

chi_grid <-
  chi_param %>%
  grid_latin_hypercube(size = 18)

res <- tune_grid(chi_wflow, resamples = data_folds, grid = chi_grid, control = control_grid(verbose = TRUE))

# summarize(res) %>%
#   dplyr::filter(.metric == "rmse") %>%
#   select(-n, -std_err, -.estimator, -.metric) %>%
#   ggplot(aes(x = neighbors, y = mean, col = weight_func)) +
#   geom_point() + geom_line() +
#   facet_wrap(~threshold, scales = "free_x")

show_best(res, metric = "rmse")

set.seed(354)
knn_search <-
  tune_bayes(
    chi_wflow,
    resamples = data_folds,
    param_info = chi_param,
    initial = res,
    metrics = metric_set(rmse),
    iter = 20,
    control = control_bayes(verbose = TRUE, uncertain = 5)
  )

autoplot(knn_search, type = "performance")


## -----------------------------------------------------------------------------


library(finetune)
set.seed(121)
res_anova <-
  tune_race_anova(
    chi_wflow,
    resamples = data_folds,
    grid = chi_grid,
    control = control_race(verbose_elim = TRUE, randomize = TRUE)
  )


set.seed(354)
knn_sa <-
  tune_sim_anneal(
    chi_wflow,
    resamples = data_folds,
    param_info = chi_param,
    initial = 4,
    metrics = metric_set(rmse),
    iter = 50
  )
