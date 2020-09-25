library(tidymodels)
library(tune)
library(mlbench)

data(Ionosphere)

# ------------------------------------------------------------------------------

set.seed(151)
iono_rs <- bootstraps(Ionosphere, times = 30)

svm_mod <-
  svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
  set_mode("classification") %>%
  set_engine("kernlab")

iono_rec <-
  recipe(Class ~ ., data = Ionosphere)  %>%
  step_zv(all_predictors())%>%
  step_dummy(all_predictors(), -all_numeric())

roc_vals <- metric_set(roc_auc)

# set.seed(3625)
# iono_res_1 <- tune_grid(Class ~ . - V1 - V2, model = svm_mod, resamples = iono_rs, metrics = roc_vals)

set.seed(3625)
iono_res_2 <- tune_grid(iono_rec, model = svm_mod, resamples = iono_rs, metrics = roc_vals, control = control_grid(verbose = TRUE))

set.seed(8161)
search_res <-
  tune_bayes(
    iono_rec,
    model = svm_mod,
    resamples = iono_rs,
    metrics = roc_vals,
    initial = iono_res_2,
    iter = 15,
    control = control_bayes(verbose = TRUE)
  )


