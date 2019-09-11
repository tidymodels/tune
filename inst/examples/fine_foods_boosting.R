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
  step_texthash(review, signed = TRUE) %>%
  step_rename_at(starts_with("review_hash"), fn = ~ gsub("review_", "", .)) %>%
  step_mutate_at(starts_with("hash"), fn = binary_hash) %>%
  step_YeoJohnson(one_of(basics)) %>%
  step_zv(all_predictors())

boost_mod <-
  boost_tree(mode = "classification", mtry = tune(), trees = tune(),
             min_n = tune(), learn_rate = tune(), tree_depth = tune(),
             loss_reduction = tune(), sample_size = tune()) %>%
  set_engine("xgboost")


text_wflow <-
  workflow() %>%
  add_recipe(pre_proc) %>%
  add_model(boost_mod)

text_set <-
  text_wflow %>%
  param_set() %>%
  update("mtry", mtry_long(c(0, 3))) %>%
  update("sample_size", sample_prop(0:1))


set.seed(8935)
folds <- group_vfold_cv(training_data, "product")

# ------------------------------------------------------------------------------

decr_trade_off <- function(i) {
  expo_decay(i, start_val = .5, 0, slope = 1/10)
}

text_search <-
  tune_Bayes(
    text_wflow,
    folds,
    param_info = text_set,
    objective = exp_improve(decr_trade_off),
    iter = 50,
    control = Bayes_control(verbose = TRUE)
  )


