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
  step_other(Neighborhood, threshold = .1)  %>%
  step_dummy(all_nominal()) %>%
  step_zv(all_predictors())

svm_model <-
  svm_rbf(
    mode = "regression", cost = tune(), rbf_sigma = tune()) %>%
  set_engine("kernlab")


ames_wflow <-
  workflow() %>%
  add_recipe(ames_rec) %>%
  add_model(svm_model)


set.seed(4567367)
ames_set <-
  parameters(ames_wflow)

ames_grid <-
  ames_set %>%
  grid_max_entropy(size = 3)

initial_grid <- tune_grid(ames_wflow, resamples = cv_splits, grid = ames_grid, control = control_grid(verbose = TRUE))

