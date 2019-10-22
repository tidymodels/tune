library(tidymodels)
library(tune)
library(mlbench)

data(PimaIndiansDiabetes)

# ------------------------------------------------------------------------------

set.seed(151)
pima_rs <- vfold_cv(PimaIndiansDiabetes, repeats = 3)

tree_mod <-
  decision_tree(cost_complexity = tune(), min_n = tune()) %>%
  set_mode("classification") %>%
  set_engine("rpart")

pima_wflow <-
  workflow() %>%
  add_formula(diabetes ~ .) %>%
  add_model(tree_mod)

roc_vals <- metric_set(roc_auc)

set.seed(3625)
pima_res <- tune_grid(pima_wflow, resamples = pima_rs, metrics = roc_vals)

# ------------------------------------------------------------------------------

rs_estimates <- summarize(pima_res)

ggplot(rs_estimates, aes(x = cost_complexity, y = min_n, col = mean, size = mean)) +
  geom_point() +
  scale_x_log10()

best_vals <-
  rs_estimates %>%
  arrange(desc(mean)) %>%
  slice(1:2)


