library(tidymodels)
library(workflows)
library(tune)

# ------------------------------------------------------------------------------

set.seed(7898)
data_folds <- vfold_cv(mtcars, repeats = 2)

# ------------------------------------------------------------------------------

base_rec <-
  recipe(mpg ~ ., data = mtcars) %>%
  step_normalize(all_predictors())

disp_rec <-
  base_rec %>%
  step_bs(disp, degree = tune(), deg_free = tune()) %>%
  step_bs(wt, degree = tune("wt degree"), deg_free = tune("wt df"))

lm_model <-
  linear_reg(mode = "regression") %>%
  set_engine("lm")

cars_wflow <-
  workflow() %>%
  add_recipe(disp_rec) %>%
  add_model(lm_model)

set.seed(255)
cars_grid <-
  cars_wflow %>%
  param_set %>%
  update(id = "^degree", degree_int(1:2)) %>%
  update(id = "^deg_free", deg_free(c(2, 10))) %>%
  update(id = "wt degree", degree_int(1:2)) %>%
  update(id = "wt df$", deg_free(c(2, 10))) %>%
  grid_regular(levels = c(3, 2, 3, 2))


cars_res <- tune_grid(cars_wflow, data_folds, cars_grid, control = grid_control(verbose = TRUE))

estimate(cars_res) %>%
  dplyr::filter(.metric == "rmse") %>%
  ggplot(aes(x = deg_free, y = mean, col = factor(degree))) +
  geom_path() +
  geom_point()


estimate(cars_res) %>%
  dplyr::filter(.metric == "rmse") %>%
  arrange(mean) %>%
  slice(1)

