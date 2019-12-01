library(tidymodels)
library(tune)
library(AmesHousing)
library(workflows)

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

extr <- function(x) {
  # tibble(num_nods = sum(x$model$frame$var == "<leaf>"))
  x$model
}

num_leaves <- function(x) {
  tibble(num_leaves = sum(x$model$frame$var == "<leaf>"))
}


prm <-
  parameters(ames_wflow) %>%
  update(min_n = min_n(c(3, 10)))

set.seed(4567367)
initial_grid <-
  tune_grid(
    ames_wflow,
    resamples = cv_splits,
    param_info = prm,
    control = control_grid(verbose = TRUE, extract = num_leaves)
  )

# ------------------------------------------------------------------------------

foo <- function(i) {
  expo_decay(i, start_val = .02, 0, slope = 1/10)
}


set.seed(463)
test <-
  tune_bayes(
    ames_wflow,
    resamples = cv_splits,
    initial = initial_grid,
    metrics = metric_set(rmse),
    objective = exp_improve(foo),
    param_info = prm,
    iter = 20,
    control = control_bayes(verbose = TRUE, uncertain = 10, extract = num_leaves)
  )
