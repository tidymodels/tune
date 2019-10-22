library(tidymodels)
library(tune)
library(readr)

# From https://github.com/juliasilge/supervised-ML-case-studies-course
# ------------------------------------------------------------------------------

vote_train <- read_rds(url("https://github.com/juliasilge/supervised-ML-case-studies-course/blob/master/data/c3_training_full.rds?raw=true"))

voters_rec <-
  recipe(turnout16_2016 ~ ., data = vote_train) %>%
  step_downsample(turnout16_2016)

lr_mod <-
  logistic_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet")

rf_mod <-
  rand_forest(mode = "classification", mtry = tune(), min_n = tune(), trees = 1000) %>%
  set_engine("ranger")

set.seed(6059)
voter_folds <- vfold_cv(vote_train)

# ------------------------------------------------------------------------------

glmn_grid <- expand.grid(penalty = 10^seq(-3, 0, length = 14),
                         mixture = (0:5)/5)

glmn_search <- tune_grid(voters_rec, lr_mod, resamples = voter_folds,
                         grid = glmn_grid,
                         metrics = metric_set(accuracy, roc_auc))

summarize(glmn_search) %>%
  filter(.metric == "accuracy") %>%
  ggplot(aes(x = penalty, y = mean, col = factor(mixture))) +
  geom_point() +
  geom_line() +
  scale_x_log10()


# ------------------------------------------------------------------------------

set.seed(606)
rf_grid <-
  parameters(rf_mod) %>%
  update(mtry = mtry(c(1, 41))) %>%
  grid_latin_hypercube(size = 20)

set.seed(1354)
rf_search <- tune_grid(voters_rec, rf_mod, resamples = voter_folds,
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
