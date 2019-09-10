library(tidymodels)
library(workflows)
library(tune)
library(textrecipes)
library(doMC)
registerDoMC(cores = 20)

# ------------------------------------------------------------------------------

data("small_fine_foods")

# ------------------------------------------------------------------------------

basics <- names(textfeatures:::count_functions)

basics <- paste0("textfeature_review_raw_", basics)
pre_proc <-
  recipe(score ~ product + review, data = training_data) %>%
  update_role(product, new_role = "id") %>%
  step_mutate(review_raw = review) %>%
  step_textfeature(review_raw) %>%
  step_tokenize(review)  %>%
  step_stopwords(review) %>%
  step_stem(review) %>%
  step_texthash(review, signed = TRUE, num_terms = tune()) %>%
  step_YeoJohnson(one_of(basics)) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_predictors())

lr_mod <-
  logistic_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet")


text_wflow <-
  workflow() %>%
  add_recipe(pre_proc) %>%
  add_model(lr_mod)

set.seed(8935)
folds <- group_vfold_cv(training_data, "product")

# ------------------------------------------------------------------------------

text_grid <- expand.grid(penalty = 10^seq(-10, 1, length = 20),
                         mixture = seq(0, 1, length = 5),
                         num_terms = 2^seq(5, 10, by = 1))


glmnet_vars <- function(x) {
  tibble(penalty = x$model$lambda, num_vars = x$model$df)
}

cls <- metric_set(roc_auc)
set.seed(1559)
text_glmnet <- tune_grid(text_wflow, folds, grid = text_grid, perf = cls,
                         control = grid_control(verbose = TRUE, extract = glmnet_vars,
                                                save_pred = TRUE))

# text_glmnet %>%
#   select(id, .extract) %>%
#   unnest() %>%
#   select(-penalty) %>%
#   unnest() %>%
#   filter(num_terms == 1024 & mixture > 0) %>%
#   mutate(group = paste(id, mixture)) %>%
#   ggplot(aes(x = penalty, y = num_vars)) +
#   geom_path(aes(group = group, col = factor(mixture)), alpha = .1) +
#   scale_x_log10()

summarize(text_glmnet) %>%
  filter(.metric == "accuracy") %>%
  ggplot(aes(x = log10(penalty), y = mixture, fill = mean)) +
  facet_wrap(~ num_terms) +
  geom_tile() +
  theme_bw()

summarize(text_glmnet) %>%
  filter(.metric == "accuracy") %>%
  ggplot(aes(x = penalty, y = mean, col = mixture, group = mixture)) +
  facet_wrap(~ num_terms) +
  geom_point() + geom_line() +
  scale_x_log10()

# ------------------------------------------------------------------------------

test_set <-
  text_wflow %>%
  param_set() %>%
  update("num_terms",  num_hash())

trade_decay <- function(iter) {
  expo_decay(iter, start_val = .1, limit_val = 0, slope = 1/10)
}


set.seed(8161)
search_res <-
  tune_Bayes(
    text_wflow,
    folds,
    initial = 5,
    iter = 20,
    perf = cls,
    objective = exp_improve(trade_decay),
    control = Bayes_control(verbose = FALSE, extract = glmnet_vars, save_pred = TRUE)
  )



