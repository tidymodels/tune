library(tidymodels)
library(tune)
library(textrecipes)
library(doMC)
registerDoMC(cores = 20)

# ------------------------------------------------------------------------------

data("small_fine_foods")

# ------------------------------------------------------------------------------

basics <- names(textfeatures:::count_functions)

binary_hash <- function(x) {
  x <- ifelse(x < 0, -1, x)
  x <- ifelse(x > 0,  1, x)
  x
}

pre_proc <-
  recipe(score ~ product + review, data = training_data) %>%
  update_role(product, new_role = "id") %>%
  step_mutate(review_raw = review) %>%
  step_textfeature(review_raw) %>%
  step_rename_at(
    starts_with("textfeature_"),
    fn = ~ gsub("textfeature_review_raw_", "", .)
  ) %>%
  step_tokenize(review)  %>%
  step_stopwords(review) %>%
  step_stem(review) %>%
  step_texthash(review, signed = TRUE, num_terms = tune()) %>%
  step_rename_at(starts_with("review_hash"), fn = ~ gsub("review_", "", .)) %>%
  step_mutate_at(starts_with("hash"), fn = binary_hash) %>%
  step_YeoJohnson(all_of(basics)) %>%
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
folds <- vfold_cv(training_data)

# ------------------------------------------------------------------------------

text_grid <- expand.grid(penalty = 10^seq(-10, 1, length = 20),
                         mixture = seq(0, 1, length = 5),
                         num_terms = 2^seq(5, 10, by = 1))


glmnet_vars <- function(x) {
  tibble(penalty = x$model$lambda, num_vars = x$model$df)
}

cls <- metric_set(roc_auc)
set.seed(1559)
text_glmnet <- tune_grid(text_wflow, resamples = folds, grid = text_grid, metrics = cls,
                         control = control_grid(verbose = TRUE, extract = glmnet_vars,
                                                save_pred = TRUE))

print(warnings())

# text_glmnet %>%
#   select(id, .extracts) %>%
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
  parameters() %>%
  update(num_terms = num_hash())

trade_decay <- function(iter) {
  expo_decay(iter, start_val = .1, limit_val = 0, slope = 1/10)
}


set.seed(8161)
search_res <-
  tune_bayes(
    text_wflow,
    folds,
    initial = 5,
    iter = 20,
    metrics = cls,
    objective = exp_improve(trade_decay),
    control = control_bayes(verbose = FALSE, extract = glmnet_vars, save_pred = TRUE)
  )

print(warnings())




