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
set.seed(2453)
big_splits <- vfold_cv(ames_train, v = 10, strata = "Sale_Price", repeats = 20)

# ------------------------------------------------------------------------------

ames_rec <-
  recipe(Sale_Price ~ ., data = ames_train) %>%
  step_log(Sale_Price, base = 10) %>%
  step_YeoJohnson(Lot_Area, Gr_Liv_Area) %>%
  step_other(Neighborhood, threshold = .1)  %>%
  step_dummy(all_nominal()) %>%
  step_zv(all_predictors())

cart_model <-
  decision_tree(
    mode = "regression", cost_complexity = tune(), min_n = 2) %>%
  set_engine("rpart")


ames_wflow <-
  workflow() %>%
  add_recipe(ames_rec) %>%
  add_model(cart_model)


set.seed(4567367)
ames_set <-
  param_set(ames_wflow) %>%
  update("cost_complexity", cost_complexity(c(-6, -1)))

ames_grid <- tibble(cost_complexity = 10^c(-4, -2, -1.5))

initial_grid <- tune_grid(ames_wflow, cv_splits, ames_grid, control = grid_control(verbose = TRUE))

# ------------------------------------------------------------------------------

whole_grid <-
  ames_set %>%
  grid_regular(levels = 100)


set.seed(45)
full_grid <- tune_grid(ames_wflow, big_splits, whole_grid, control = grid_control(verbose = TRUE))

estimate(full_grid) %>% filter(.metric == "rmse") %>%
  ggplot(aes(x = cost_complexity, y = mean)) +
  geom_path() +
  scale_x_log10() +
  geom_vline(xintercept = 10^c(-4, -2, -1.5), lty = 3)

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
    control = Bayes_control(verbose = TRUE, random_value = 3)
  )
