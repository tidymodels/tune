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
  step_YeoJohnson(Lot_Area, Gr_Liv_Area) %>%
  step_other(Neighborhood, threshold = tune())  %>%
  step_dummy(all_nominal()) %>%
  step_interact(~ starts_with("Central_Air"):Year_Built)  %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_predictors()) %>%
  step_bs(Longitude, deg_free = tune("long df"))  %>%
  step_bs(Latitude, deg_free = tune("lat df"))


lm_mod <-
  linear_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet")


ames_wflow <-
  workflow() %>%
  add_recipe(ames_rec) %>%
  add_model(lm_mod)


set.seed(8)
ames_set <-
  param_set(ames_wflow) %>%
  update(id = "threshold", threshold(c(0, .5))) %>%
  update(id = "long df", deg_free(c(3, 15))) %>%
  update(id = "lat df", deg_free(c(3, 15)))

ames_grid <-
  ames_set %>%
  grid_max_entropy(size = 5)

ames_glmnet <- tune_grid(ames_wflow, cv_splits, ames_grid, control = grid_control(verbose = TRUE))

# estimate(ames_glmnet) %>%
#   dplyr::filter(.metric == "rmse") %>%
#   select(-n, -std_err, -.estimator, -.metric) %>%
#   mutate(penalty = log10(penalty)) %>%
#   gather(parameter, value, -mean) %>%
#   ggplot(aes(x = value, y = mean)) +
#   geom_point() +
#   facet_wrap(~parameter, scales = "free_x")
#
# estimate(ames_glmnet) %>%
#   dplyr::filter(.metric == "rmse") %>%
#   arrange(mean) %>%
#   slice(1)

test <-
  tune_Bayes(
    ames_wflow,
    cv_splits,
    param_info = ames_set,
    initial = estimate(ames_glmnet),
    metrics = metric_set(rmse, rsq),
    iter = 50,
    control = Bayes_control(verbose = TRUE, random_value = 5)
  )
