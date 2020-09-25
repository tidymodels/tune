library(tidymodels)
library(tune)
library(QSARdata)

data(Mutagen)

Mutagen_Dragon$Class <- Mutagen_Outcome

# ------------------------------------------------------------------------------

set.seed(7898)
data_folds <- validation_split(Mutagen_Dragon)

# ------------------------------------------------------------------------------

Mutagen_rec <-
  recipe(Class ~ ., data = Mutagen_Dragon) %>%
  step_nzv(all_predictors()) %>%
  step_YeoJohnson(all_predictors()) %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors(), num_comp = tune())

svm_model <-
  # svm_poly(mode = "classification", cost = tune(), degree = tune(),
  #          scale_factor = tune()) %>%
  svm_rbf(mode = "classification", cost = tune(), rbf_sigma = tune()) %>%
  set_engine("kernlab")

Mutagen_wflow <-
  workflow() %>%
  add_recipe(Mutagen_rec) %>%
  add_model(svm_model)

Mutagen_param <-
  parameters(Mutagen_wflow) %>%
  update(num_comp = num_comp(c(0, 20)))

set.seed(552)
Mutagen_grid <-
  Mutagen_param %>%
  grid_max_entropy(size = 5)

class_only <- metric_set(accuracy, kap, mcc)

res <- tune_grid(Mutagen_wflow, resamples = data_folds, grid = Mutagen_grid, metrics = class_only,
                 control = control_grid(verbose = TRUE))


summarize(res) %>% filter(.metric == "accuracy") %>% arrange(desc(mean))

set.seed(3654)
svm_search <-
  tune_bayes(
    Mutagen_wflow,
    resamples = data_folds,
    param_info = Mutagen_param,
    initial = res,
    metrics = class_only,
    iter = 5,
    control = control_bayes(verbose = TRUE, uncertain = 5)
  )

set.seed(378)
more_svm_search <-
  tune_bayes(
    Mutagen_wflow,
    resamples = data_folds,
    param_info = Mutagen_param,
    initial = svm_search,
    metrics = class_only,
    iter = 10,
    control = control_bayes(verbose = TRUE, uncertain = 5)
  )


set.seed(3654)
svm_sa <-
  tune_sim_anneal(
    Mutagen_wflow,
    resamples = data_folds,
    param_info = Mutagen_param,
    # initial = res,
    metrics = class_only,
    iter = 15
  )
