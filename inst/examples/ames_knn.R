library(tidymodels)
library(workflows)
library(tune)
library(AmesHousing)
library(earth) # req for muti_predict

# ------------------------------------------------------------------------------

ames <- make_ames()

# Make sure that you get the same random numbers
set.seed(4595)
data_split <- initial_split(ames, strata = "Sale_Price")

ames_train <- training(data_split)

set.seed(2453)
rs_splits <- bootstraps(ames_train, times = 20)

# ------------------------------------------------------------------------------

ames_rec <-
  recipe(Sale_Price ~ ., data = ames_train) %>%
  step_log(Sale_Price, base = 10) %>%
  step_YeoJohnson(Lot_Area, Gr_Liv_Area) %>%
  step_other(Neighborhood, threshold = .1)  %>%
  step_dummy(all_nominal()) %>%
  step_zv(all_predictors())

knn_model <-
  nearest_neighbor(
    mode = "regression", neighbors = tune(), weight_func = "triangular", dist_power = 1) %>%
  set_engine("kknn")


ames_wflow <-
  workflow() %>%
  add_recipe(ames_rec) %>%
  add_model(knn_model)


set.seed(4567367)
ames_set <-
  param_set(ames_wflow) %>%
  update(id = "neighbors", neighbors(c(1, 50)))

ames_grid <-
  ames_set %>%
  grid_max_entropy(size = 3)

initial_grid <- tune_grid(ames_wflow, rs = rs_splits, grid = ames_grid, control = grid_control(verbose = TRUE, save_pred = TRUE))

# ------------------------------------------------------------------------------


test <-
  tune_Bayes(
    ames_wflow,
    rs = rs_splits,
    param_info = ames_set,
    initial = initial_grid,
    perf = metric_set(rmse, rsq),
    iter = 15,
    control = Bayes_control(verbose = TRUE, uncertain = 3)
  )
