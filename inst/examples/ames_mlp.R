library(tidymodels)
library(tune)
library(AmesHousing)

# ------------------------------------------------------------------------------

ames <- make_ames()

# Make sure that you get the same random numbers
set.seed(4595)
data_split <- initial_split(ames, strata = "Sale_Price")

ames_train <- training(data_split)

set.seed(2453)
cv_splits <- vfold_cv(ames_train, v = 10, strata = "Sale_Price")

# ------------------------------------------------------------------------------

ames_rec <-
  recipe(Sale_Price ~ ., data = ames_train) %>%
  step_log(Sale_Price, base = 10) %>%
  step_YeoJohnson(Lot_Area, Gr_Liv_Area) %>%
  step_other(Neighborhood, threshold = tune("Nhood_other"))  %>%
  step_other(MS_SubClass , threshold = tune("SubClass_other"))  %>%
  step_dummy(all_nominal()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors(), num_comp = tune())

nn_mod <-
  mlp(mode = "regression", hidden_units = tune(), dropout = tune(),
      epochs = tune(), activation = tune()) %>%
  set_engine("keras", verbose = 0, validation = .1)

ames_wflow <-
  workflow() %>%
  add_recipe(ames_rec) %>%
  add_model(nn_mod)


set.seed(4567367)
ames_set <-
  parameters(ames_wflow) %>%
  update(Nhood_other  = threshold(c(0, .09))) %>%
  update(SubClass_other = threshold(c(0, .059))) %>%
  update(num_comp = num_comp(c(1, 20)))

set.seed(7520)
ames_grid <-
  ames_set %>%
  grid_max_entropy(size = 10)

initial_grid <- tune_grid(ames_wflow, resamples = cv_splits, grid = ames_grid, control = control_grid(verbose = TRUE))

# ------------------------------------------------------------------------------


decr_trade_off <- function(i) {
  expo_decay(i, start_val = .5, 0, slope = 1/10)
}

search_res <-
  tune_bayes(
    ames_wflow,
    resamples = cv_splits,
    param_info = ames_set,
    initial = initial_grid,,
    objective = exp_improve(decr_trade_off),
    iter = 25,
    control = control_bayes(verbose = TRUE)
  )
