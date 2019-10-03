library(tidyverse)
library(stringr)
library(sessioninfo)
options(width = 120)

# ------------------------------------------------------------------------------

classes <- c("bad", "poor", "ok", "good", "great")

# Data here:
# https://snap.stanford.edu/data/web-FineFoods.html

raw <-
  read_delim(
    "~/Downloads/finefoods.txt",
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

# Take the 10 well characterized products for the training set (or more for larger
# training set)
set.seed(5)
train_prods <-
  prod_dist %>%
  dplyr::filter(n > 125) %>%
  sample_n(10) %>%
  dplyr::select(product)

training_data <-
  train_prods %>%
  inner_join(raw, by = "product")

training_data %>%
  group_by(product) %>%
  summarise(n = length(product), rate = mean(score == "great"))

testing_data <-
  raw %>%
  anti_join(train_prods %>% dplyr::select(product), by = "product") %>%
  group_by(product) %>%
  sample_n(1) %>%
  ungroup() %>%
  sample_n(500)

save(training_data, testing_data, file = "data/small_fine_foods.RData", version = 2, compress = "xz")

# ------------------------------------------------------------------------------

session_info()

q("no")
