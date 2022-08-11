library(tidymodels)
library(tune)
library(textrecipes)
library(doFuture)
registerDoFuture()
cl <- makeCluster(availableCores())
plan(cluster, workers = cl)
foreach::getDoParWorkers()

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
  step_texthash(review, signed = TRUE) %>%
  step_rename_at(starts_with("review_hash"), fn = ~ gsub("review_", "", .)) %>%
  step_mutate_at(starts_with("hash"), fn = binary_hash) %>%
  step_YeoJohnson(all_of(!!basics)) %>%
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
  parameters() %>%
  update(mtry = mtry_long(c(0, 3))) %>%
  update(sample_size = sample_prop(0:1))


set.seed(8935)
folds <- vfold_cv(training_data)

grid <- grid_latin_hypercube(text_set, size = 3)

res <- tune_grid(text_wflow, resamples = folds, grid = grid,
          control = control_grid(verbose = TRUE, pkgs = c("textrecipes", "textfeatures")))

# ------------------------------------------------------------------------------

decr_trade_off <- function(i) {
  expo_decay(i, start_val = .5, 0, slope = 1/10)
}

text_search <-
  tune_bayes(
    text_wflow,
    resamples = folds,
    param_info = text_set,
    objective = exp_improve(decr_trade_off),
    iter = 50,
    control = control_bayes(verbose = TRUE)
  )


