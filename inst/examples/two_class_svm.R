library(tidymodels)
library(workflows)
library(tune)

# ------------------------------------------------------------------------------

set.seed(7898)
data_folds <- vfold_cv(two_class_dat, repeats = 5)

# ------------------------------------------------------------------------------

two_class_rec <-
  recipe(Class ~ ., data = two_class_dat) %>%
  step_normalize(A, B)

svm_model <-
  svm_poly(mode = "classification", cost = tune(), degree = tune(),
           scale_factor = tune()) %>%
  set_engine("kernlab")

two_class_wflow <-
  workflow() %>%
  add_recipe(two_class_rec) %>%
  add_model(svm_model)

set.seed(552)
two_class_grid <-
  param_set(two_class_wflow) %>%
  grid_max_entropy(size = 50)

class_only <- metric_set(accuracy, kap, mcc)

res <- tune_grid(two_class_wflow, data_folds, two_class_grid, perf = class_only)
estimate(res) %>% filter(.metric == "accuracy") %>% arrange(desc(mean))

set.seed(365456)
svm_search <-
  tune_Bayes(
    two_class_wflow,
    data_folds,
    initial = res,
    perf = class_only,
    iter = 150,
    control = Bayes_control(verbose = TRUE, random_value = 5)
  )
