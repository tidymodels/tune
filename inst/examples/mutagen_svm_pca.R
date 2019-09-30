library(tidymodels)
library(workflows)
library(tune)
library(QSARdata)

data(Mutagen)

Mutagen_Dragon$Class <- Mutagen_Outcome

# ------------------------------------------------------------------------------

set.seed(7898)
data_folds <- mc_cv(Mutagen_Dragon, times = 1)

# ------------------------------------------------------------------------------

Mutagen_rec <-
  recipe(Class ~ ., data = Mutagen_Dragon) %>%
  step_nzv(all_predictors()) %>%
  step_YeoJohnson(all_predictors()) %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors(), num_comp = tune())

svm_model <-
  svm_poly(mode = "classification", cost = tune(), degree = tune(),
           scale_factor = tune()) %>%
  set_engine("kernlab")

Mutagen_wflow <-
  workflow() %>%
  add_recipe(Mutagen_rec) %>%
  add_model(svm_model)

Mutagen_param <-
  param_set(Mutagen_wflow) %>%
  update(num_comp = num_comp(c(1, 20)))

set.seed(552)
Mutagen_grid <-
  Mutagen_param %>%
  grid_max_entropy(size = 5)

class_only <- metric_set(accuracy, kap, mcc)

res <- tune_grid(Mutagen_wflow, data_folds, Mutagen_grid, perf = class_only,
                 control = grid_control(verbose = TRUE))


summarize(res) %>% filter(.metric == "accuracy") %>% arrange(desc(mean))

set.seed(3654)
svm_search <-
  tune_Bayes(
    Mutagen_wflow,
    data_folds,
    param_info = Mutagen_param,
    initial = res,
    perf = class_only,
    iter = 5,
    control = Bayes_control(verbose = TRUE, uncertain = 5)
  )

set.seed(378)
more_svm_search <-
  tune_Bayes(
    Mutagen_wflow,
    data_folds,
    param_info = Mutagen_param,
    initial = svm_search,
    perf = class_only,
    iter = 10,
    control = Bayes_control(verbose = TRUE, uncertain = 5)
  )
