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
  parameters %>%
  update(neighbors = neighbors(c(1, 30))) %>%
  update(dist_power = dist_power(c(1/4, 2))) %>%
  grid_regular(levels = c(30, 3, 3))


reg_knn_grid <- tune_grid(chi_wflow, resamples = data_folds, grid = chi_grid, control = control_grid(verbose = TRUE))

summarize(reg_knn_grid) %>%
  dplyr::filter(.metric == "rmse") %>%
  mutate(RMSE = mean, `Minkowski distance parameter` = dist_power, weights = weight_func) %>%
  ggplot(aes(x = neighbors, y = RMSE, col = weights)) +
  geom_path() +
  geom_point() +
  facet_wrap(~ `Minkowski distance parameter`) +
  theme_bw() +
  xlab("# Nearest-Neighbors")


summarize(reg_knn_grid) %>%
  dplyr::filter(.metric == "rmse") %>%
  arrange(mean) %>%
  slice(1)

# ------------------------------------------------------------------------------

chi_set <-
  chi_wflow %>%
  parameters %>%
  update(neighbors = neighbors(c(1, 30))) %>%
  update(dist_power  = dist_power(c(1/10, 2)))

set.seed(255)
smol_grid <-
  chi_set %>%
  grid_random(size = 5)


smol_knn_grid <- tune_grid(chi_wflow, resamples = data_folds, grid = smol_grid, control = control_grid(verbose = TRUE))

knn_search <-
  tune_bayes(
    chi_wflow,
    resamples = data_folds,
    param_info = chi_set,
    initial = smol_knn_grid,
    metrics = metric_set(rmse, rsq),
    iter = 30,
    control = control_bayes(verbose = TRUE, uncertain = 10)
  )

ggplot(
  knn_search %>% filter(.metric == "rmse"),
  aes(x = neighbors, y = dist_power, col = weight_func, size = mean)) +
  geom_point(alpha = .4) +
  theme_bw() +
  ylab("Minkowski distance parameter") +
  xlab("# Nearest-Neighbors")


library(gganimate)

for (i in 0:max(knn_search$.iter)) {
  cumulative_data <-
    knn_search %>%
    filter(.metric == "rmse" & .iter <= i) %>%
    mutate(frame = i + 1, RMSE = mean, weights = weight_func)
  if (i == 0) {
    ani_data <- cumulative_data
  } else {
    ani_data <- bind_rows(cumulative_data, ani_data)
  }
}


p <-
  ggplot(
  ani_data,
  aes(x = neighbors, y = dist_power, col = weights, size = RMSE)) +
  geom_point(alpha = 0.4) +
  labs(title = 'Iteration: {closest_state}') +
  transition_states(frame, state_length = 1) +
  ease_aes('linear') +
  theme_bw() +
  ylab("Minkowski distance parameter") +
  xlab("# Nearest-Neighbors")

animate(p, nframes = max(knn_search$.iter), width = 520)
