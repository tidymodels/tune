library(tidymodels)
library(tune)
library(ggforce)

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


lm_model <-
  linear_reg(mode = "regression") %>%
  set_engine("lm")

chi_wflow <-
  workflow() %>%
  add_recipe(chi_rec) %>%
  add_model(lm_model)

chi_grid <-
  parameters(chi_wflow) %>%
  update(threshold = threshold(c(.8, .99))) %>%
  grid_regular(levels = 10)


ext <- function(x) {
  broom::glance(x$model)
}

res <- tune_grid(chi_wflow, resamples = data_folds, grid = chi_grid,
                 control = control_grid(verbose = TRUE, extract = ext))

res_2 <- tune_grid(chi_rec, lm_model, resamples = data_folds, grid = chi_grid,
                   control = control_grid(verbose = TRUE, extract = ext))

# unnest(unnest(res %>% select(id, .extracts), cols = .extracts), cols = .extract)

lm_stats <-
  res %>%
  select(id, .extracts) %>%
  unnest(cols = .extracts) %>%
  unnest(cols = .extracts) %>%
  select(id, threshold, adj.r.squared, sigma, AIC, BIC) %>%
  group_by(threshold) %>%
  summarize(
    adj.r.squared = mean(adj.r.squared, na.rm = TRUE),
    sigma = mean(sigma, na.rm = TRUE),
    AIC = mean(AIC, na.rm = TRUE),
    BIC = mean(BIC, na.rm = TRUE)
  )

rs_stats <-
  summarize(res) %>%
  select(threshold, .metric, mean) %>%
  pivot_wider(names_from = .metric, values_from = mean, id_cols = threshold)

all_stats <- full_join(lm_stats, rs_stats)




ggplot(all_stats, aes(x = .panel_x, y = .panel_y, colour = threshold)) +
  geom_point() +
  facet_matrix(vars(-threshold)) +
  theme_bw()


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

