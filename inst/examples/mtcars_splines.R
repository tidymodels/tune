library(tidymodels)
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
  step_bs(wt,
          degree = tune("wt degree"),
          deg_free = tune("wt df"))

lm_model <-
  linear_reg(mode = "regression") %>%
  set_engine("lm")

cars_wflow <-
  workflow() %>%
  add_recipe(disp_rec) %>%
  add_model(lm_model)

cars_set <-
  cars_wflow %>%
  parameters %>%
  update(degree = degree_int(1:2)) %>%
  update(deg_free = deg_free(c(2, 10))) %>%
  update(`wt degree` = degree_int(1:2)) %>%
  update(`wt df` = deg_free(c(2, 10)))

set.seed(255)
cars_grid <-
  cars_set %>%
  grid_regular(levels = c(3, 2, 3, 2))


cars_res <-
  tune_grid(
    cars_wflow,
    resamples = data_folds,
    grid = cars_grid,
    control = control_grid(verbose = TRUE, save_pred = TRUE)
  )

spline_search <-
  tune_bayes(
    cars_wflow,
    resamples = data_folds,
    param_info = cars_set,
    initial = cars_res,
    iter = 15,
    control = control_bayes(verbose = TRUE, save_pred = TRUE)
  )

more_spline_search <-
  tune_bayes(
    cars_wflow,
    resamples = data_folds,
    param_info = cars_set,
    initial = spline_search,
    iter = 10,
    control = control_bayes(verbose = TRUE, uncertain = 10)
  )

## -----------------------------------------------------------------------------


library(finetune)
set.seed(121)
res_anova <-
  tune_race_anova(
    cars_wflow,
    resamples = data_folds,
    grid = cars_grid,
    control = control_race(verbose_elim = TRUE, randomize = TRUE)
  )

set.seed(121)
res_bt <-
  tune_race_win_loss(
    cars_wflow,
    resamples = data_folds,
    grid = cars_grid,
    control = control_race(verbose_elim = TRUE, randomize = TRUE)
  )


set.seed(121)
res_sa <-
  tune_sim_anneal(
    cars_wflow,
    resamples = data_folds,
    param_info = cars_set,
    iter = 25
  )



