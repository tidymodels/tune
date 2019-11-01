library(tidymodels)
library(tune)
library(readr)

# From https://github.com/juliasilge/supervised-ML-case-studies-course
# ------------------------------------------------------------------------------

so_train <-
  read_rds(url("https://github.com/juliasilge/supervised-ML-case-studies-course/blob/master/data/c2_training_full.rds?raw=true")) %>%
  mutate(Country = as.factor(Country)) %>%
  mutate_if(is.logical, as.numeric) %>%
  # ranger doesn't like spaces or "/" and will error with "Illegal column names in
  # formula interface. Fix column names or use alternative interface in ranger.
  rename_at(vars(dplyr::contains(" ")), ~ gsub("([[:blank:]])|([[:punct:]])", "_", .))

lr_rec <-
  recipe(Remote ~ ., data = so_train) %>%
  step_dummy(Country) %>%
  step_downsample(Remote) %>%
  step_zv(all_predictors())

lr_mod <-
  logistic_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet")

rf_rec <-
  recipe(Remote ~ ., data = so_train) %>%
  step_downsample(Remote)

rf_mod <-
  rand_forest(mode = "classification", mtry = tune(), min_n = tune(), trees = 1000) %>%
  set_engine("ranger")

set.seed(4290)
so_boots <- bootstraps(so_train)

# ------------------------------------------------------------------------------

library(doParallel)
cl <- makePSOCKcluster(parallel::detectCores(logical = FALSE))
registerDoParallel(cl)

# ------------------------------------------------------------------------------

glmn_grid <- expand.grid(penalty = 10^seq(-3, -1, length = 14),
                         mixture = (0:5)/5)

glmn_search <- tune_grid(lr_rec, lr_mod, resamples = so_boots,
                         grid = glmn_grid,
                         metrics = metric_set(accuracy, roc_auc),
                         control = control_grid(verbose = TRUE))

summarize(glmn_search) %>%
  filter(.metric == "accuracy") %>%
  ggplot(aes(x = penalty, y = mean, col = factor(mixture))) +
  geom_point() +
  geom_line() +
  scale_x_log10()


# ------------------------------------------------------------------------------

set.seed(4538)
rf_grid <-
  parameters(rf_mod) %>%
  update(mtry = mtry(c(1, 20))) %>%
  grid_latin_hypercube(size = 20)

set.seed(1809)
rf_search <- tune_grid(rf_rec, rf_mod, resamples = so_boots,
                       grid = rf_grid,
                       metrics = metric_set(accuracy, roc_auc))

summarize(rf_search) %>%
  filter(.metric == "accuracy") %>%
  select(mtry, min_n, mean) %>%
  pivot_longer(-mean, names_to = "parameter", values_to = "value") %>%
  ggplot(aes(x = value, y = mean)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_wrap( ~ parameter, scales = "free_x")


summarize(rf_search) %>%
  filter(.metric == "accuracy") %>%
  ggplot(aes(x = mtry, y = min_n, size = mean)) +
  geom_point()
