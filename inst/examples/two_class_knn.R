library(tidymodels)
library(workflows)
library(tune)
# library(doMC)
# registerDoMC(cores=8)

# ------------------------------------------------------------------------------

set.seed(7898)
data_folds <- vfold_cv(two_class_dat, repeats = 5)

# ------------------------------------------------------------------------------

two_class_rec <-
  recipe(Class ~ ., data = two_class_dat) %>%
  step_normalize(A, B)

knn_model <-
  nearest_neighbor(
    mode = "classification",
    neighbors = tune("K"),
    weight_func = tune(),
    dist_power = tune("exponent")
  ) %>%
  set_engine("kknn")

two_class_wflow <-
  workflow() %>%
  add_recipe(two_class_rec) %>%
  add_model(knn_model)

two_class_set <-
  param_set(two_class_wflow) %>%
  update(id = "K", neighbors(c(1, 50))) %>%
  update(id = "exponent", dist_power(c(1/10, 2)))

set.seed(2494)
two_class_grid <-
  two_class_set %>%
  grid_max_entropy(size = 10)

class_only <- metric_set(accuracy, kap, mcc)

res <- tune_grid(two_class_wflow, data_folds, two_class_grid, perf = class_only)
summarize(res) %>% filter(.metric == "accuracy") %>% arrange(desc(mean))

decr_kappa <- function(i) {
  if (i < 5) {
    res <- 0
  } else {
    if (i > 20) {
      res <- 1
    } else {
      res <- ((i - 5)/(20 - 5))^2
    }
  }
  2 * res
}

set.seed(365)
svm_search <-
  tune_Bayes(
    two_class_wflow,
    data_folds,
    param_info = two_class_set,
    initial = res,
    objective = conf_bound(kappa = decr_kappa),
    perf = class_only,
    iter = 30,
    control = Bayes_control(verbose = TRUE, uncertain = 5)
  )

ggplot(svm_search %>%  filter(.metric == "accuracy")) +
  aes(x = neighbors, y = dist_power, col = weight_func, size = mean) +
  geom_point(alpha= .7)

