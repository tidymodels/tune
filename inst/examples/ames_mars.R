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
cv_splits <- vfold_cv(ames_train, v = 10, strata = "Sale_Price")

# ------------------------------------------------------------------------------

ames_rec <-
  recipe(Sale_Price ~ ., data = ames_train) %>%
  recipe(Sale_Price ~ ., data = ames_train) %>%
  step_log(Sale_Price, base = 10) %>%
  step_YeoJohnson(Lot_Area, Gr_Liv_Area) %>%
  step_other(Neighborhood, threshold = tune())  %>%
  step_dummy(all_nominal()) %>%
  step_zv(all_predictors())

mars_mod <-
  mars(mode = "regression", num_terms = tune(), prod_degree = tune()) %>%
  set_engine("earth")


ames_wflow <-
  workflow() %>%
  add_recipe(ames_rec) %>%
  add_model(mars_mod)


set.seed(4567367)
ames_set <-
  param_set(ames_wflow) %>%
  update(id = "threshold", threshold(c(0, .2))) %>%
  update(id = "num_terms", num_terms(c(2, 20)))

ames_grid <-
  ames_set %>%
  grid_max_entropy(size = 5)

res <- tune_grid(ames_wflow, cv_splits, ames_grid, control = grid_control(verbose = TRUE))

estimate(res) %>%
  dplyr::filter(.metric == "rmse") %>%
  ggplot(aes(x = num_terms, y = mean, col = factor(prod_degree))) +
  geom_point(cex = 1) +
  geom_path() +
  facet_wrap(~ threshold)

estimate(res) %>%
  dplyr::filter(.metric == "rmse") %>%
  arrange(mean) %>%
  slice(1)


test <-
  tune_Bayes(
    ames_wflow,
    cv_splits,
    param_info = ames_set,
    initial = res,
    metrics = metric_set(rmse, rsq),
    iter = 15,
    control = Bayes_control(verbose = TRUE, uncertain = 3)
  )
