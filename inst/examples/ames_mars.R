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
  recipe(Sale_Price ~ Bldg_Type + Neighborhood + Year_Built + Gr_Liv_Area +
           Full_Bath + Year_Sold + Lot_Area + Central_Air + Longitude + Latitude,
         data = ames_train) %>%
  step_log(Sale_Price, base = 10) %>%
  step_YeoJohnson(Lot_Area, Gr_Liv_Area) %>%
  step_other(Neighborhood, threshold = tune())  %>%
  step_dummy(all_nominal()) %>%
  step_interact(~ starts_with("Central_Air"):Year_Built)  %>%
  step_normalize(all_predictors())


mars_mod <-
  mars(mode = "regression", num_terms = tune(), prod_degree = tune()) %>%
  set_engine("earth")


ames_wflow <-
  workflow() %>%
  add_recipe(ames_rec) %>%
  add_model(mars_mod)


set.seed(4567367)
ames_grid <-
  param_set(ames_wflow) %>%
  update(id = "threshold", threshold(c(0, .2))) %>%
  update(id = "num_terms", num_terms(c(2, 40))) %>%
  grid_regular(levels = c(30, 2, 10))

res <- tune_grid(ames_wflow, cv_splits, ames_grid, control = list(verbose = TRUE))

summarizer(res) %>%
  dplyr::filter(.metric == "rmse") %>%
  ggplot(aes(x = num_terms, y = mean, col = factor(prod_degree))) +
  geom_point() +
  facet_wrap(~ threshold)

summarizer(res) %>%
  dplyr::filter(.metric == "rmse") %>%
  arrange(mean) %>%
  slice(1)

