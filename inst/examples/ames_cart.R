library(tidymodels)
library(workflows)
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
  step_YeoJohnson(Lot_Area, Gr_Liv_Area)

cart_model <-
  decision_tree(
    mode = "regression", cost_complexity = tune(), min_n = tune()) %>%
  set_engine("rpart")


ames_wflow <-
  workflow() %>%
  add_recipe(ames_rec) %>%
  add_model(cart_model)


set.seed(4567367)
initial_grid <- tune_grid(ames_wflow, cv_splits, control = grid_control(verbose = TRUE))

# ------------------------------------------------------------------------------

set.seed(463)
test <-
  tune_Bayes(
    ames_wflow,
    cv_splits,
    initial = initial_grid,
    metrics = metric_set(rmse),
    iter = 15,
    control = Bayes_control(verbose = TRUE, random_value = 10)
  )
