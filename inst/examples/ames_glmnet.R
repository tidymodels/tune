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

lm_mod <-
  linear_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet")


ames_wflow <-
  workflow() %>%
  add_formula(log(Sale_Price) ~ .) %>%
  add_model(lm_mod)

grid_df <- grid_regular(ames_wflow, levels = c(10, 3))

ames_glmnet <- tune_grid(ames_wflow, resamples = cv_splits, grid = grid_df, control = control_grid(verbose = TRUE))


# summarize(ames_glmnet) %>%
#   dplyr::filter(.metric == "rmse") %>%
#   select(-n, -std_err, -.estimator, -.metric) %>%
#   mutate(penalty = log10(penalty)) %>%
#   gather(parameter, value, -mean) %>%
#   ggplot(aes(x = value, y = mean)) +
#   geom_point() +
#   facet_wrap(~parameter, scales = "free_x")
#
# summarize(ames_glmnet) %>%
#   dplyr::filter(.metric == "rmse") %>%
#   arrange(mean) %>%
#   slice(1)

set.seed(9890)
search_res <-
  tune_bayes(
    ames_wflow,
    resamples = cv_splits,
    initial = ames_glmnet,
    iter = 50,
    control = control_bayes(verbose = TRUE)
  )


more_search_res <-
  tune_bayes(
    ames_wflow,
    resamples = cv_splits,
    # param_info = ames_set,
    initial = search_res,
    metrics = metric_set(rmse, rsq),
    iter = 50,
    control = control_bayes(verbose = TRUE, uncertain = 5)
  )

