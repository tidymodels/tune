library(tidymodels)
library(workflows)
library(tune)
library(tidyverse)
library(stringr)
library(textrecipes)

library(doMC)
registerDoMC(cores = 20)

# ------------------------------------------------------------------------------

classes <- c("bad", "poor", "ok", "good", "great")

# Data here:
# https://snap.stanford.edu/data/web-FineFoods.html

raw <-
  read_delim(
    "finefoods.txt",
    delim = "\n",
    col_names = "text",
    col_types = cols(text = col_character())
  ) %>%
  mutate(
    text = str_remove(text, "product/"),
    text = str_remove(text, "review/"),
    prod_num  = ifelse(str_detect(text, "productId"), 1, 0),
    prod_num = cumsum(prod_num)
  ) %>%
  dplyr::filter(
    str_detect(text, "(productId:)|(text:)|(score:)")
  ) %>%
  mutate(
    field = case_when(
      str_detect(text, "productId:") ~ "product",
      str_detect(text, "text:") ~ "review",
      str_detect(text, "score:") ~ "score",
      TRUE ~ "unknown"
    ),
    text = str_replace(text, "(productId: )|(text: )|(score: )", "")
  ) %>%
  spread(field, text) %>%
  dplyr::select(-prod_num) %>%
  mutate(
    # score = as.numeric(score),
    # score = as.integer(score),
    # score = classes[score],
    # score = factor(score, levels = classes)
    score = factor(ifelse(score == "5.0", "great", "other"))
  )

# ------------------------------------------------------------------------------
# Choose _products_ to split to training and test set. There tend to be multiple
# rows per product.

prod_dist <-
  raw %>%
  group_by(product) %>%
  count() %>%
  ungroup() %>%
  arrange(desc(n))

# Take the 25 well characterized products for the training set (or more for larger
# training set)
set.seed(9565)
train_prods <-
  prod_dist %>%
  dplyr::filter(n > 100) %>%
  sample_n(25) %>%
  dplyr::select(product) %>%
  mutate(data_set = "training")

# Take another sample from the same population
set.seed(6775)
test_prods <-
  prod_dist %>%
  anti_join(train_prods %>% dplyr::select(product), by = "product") %>%
  dplyr::filter(n > 100) %>%
  sample_n(25) %>%
  dplyr::select(product) %>%
  mutate(data_set = "testing")

training_data <-
  train_prods %>%
  inner_join(raw, by = "product")

testing_data <-
  test_prods %>%
  inner_join(raw, by = "product")


# ------------------------------------------------------------------------------

basics <-
  c('n_words', 'n_uq_words', 'n_charS', 'n_uq_charS', 'n_digits', 'n_hashtags',
    'n_uq_hashtags', 'n_mentions', 'n_uq_mentions', 'n_commas', 'n_periods',
    'n_exclaims', 'n_extraspaces', 'n_caps', 'n_lowers', 'n_urls', 'n_uq_urls',
    'n_nonasciis', 'n_puncts', 'politeness', 'first_person', 'first_personp',
    'second_person', 'second_personp', 'third_person', 'to_be', 'prepositions')
basics <- paste0("textfeature_review_raw_", basics)

pre_proc <-
  recipe(score ~ product + review, data = training_data) %>%
  update_role(product, new_role = "id") %>%
  step_mutate(review_raw = review) %>%
  step_textfeature(review_raw) %>%
  step_tokenize(review)  %>%
  step_stopwords(review) %>%
  step_stem(review) %>%
  step_texthash(review, signed = TRUE) %>%
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

text_grid <- expand.grid(penalty = 10^seq(-10, 1, length = 20),
                         mixture = seq(0, 1, length = 5))

set.seed(1559)
text_glmnet <- tune_grid(text_wflow, folds, grid = text_grid,
                         control = grid_control(verbose = TRUE))

estimate(text_glmnet) %>%
  filter(.metric == "accuracy") %>%
  ggplot(aes(x = log10(penalty), y = mixture, fill = mean)) +
  geom_tile() +
  theme_bw()

estimate(text_glmnet) %>%
  filter(.metric == "accuracy") %>%
  ggplot(aes(x = penalty, y = mean, col = mixture, group = mixture))  +
  geom_point() + geom_line() +
  scale_x_log10()

