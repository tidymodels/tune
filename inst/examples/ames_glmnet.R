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
  step_normalize(all_predictors()) %>%
  step_bs(Longitude, deg_free = tune("long df"))  %>%
  step_bs(Latitude, deg_free = tune("lat df"))


lm_mod <-
  linear_reg(penalty= tune(), mixture = tune()) %>%
  set_engine("glmnet")


ames_wflow <-
  workflow() %>%
  add_recipe(ames_rec) %>%
  add_model(lm_mod)


set.seed(4567367)
ames_grid <-
  param_set(ames_wflow) %>%
  update(id = "threshold", threshold(c(0, .2))) %>%
  update(id = "long df", deg_free(c(3, 10))) %>%
  update(id = "lat df", deg_free(c(3, 10))) %>%
  grid_max_entropy(size = 50)

res <- tune_grid(ames_wflow, cv_splits, ames_grid, control = list(verbose = TRUE))

summarizer(res) %>%
  dplyr::filter(.metric == "rmse") %>%
  select(-n, -std_err, -.estimator, -.metric) %>%
  mutate(penalty = log10(penalty)) %>%
  gather(parameter, value, -mean) %>%
  ggplot(aes(x = value, y = mean)) +
  geom_point() +
  facet_wrap(~parameter, scales = "free_x")

summarizer(res) %>%
  dplyr::filter(.metric == "rmse") %>%
  arrange(mean) %>%
  slice(1)

