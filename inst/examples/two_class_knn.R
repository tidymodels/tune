library(tidymodels)
library(tune)
library("doFuture")
registerDoFuture()
plan(multicore)


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
  parameters(two_class_wflow) %>%
  update(K = neighbors(c(1, 50))) %>%
  update(exponent = dist_power(c(1/10, 2)))

set.seed(2494)
two_class_grid <-
  two_class_set %>%
  grid_max_entropy(size = 10)

class_metrics <- metric_set(roc_auc, accuracy, kap, mcc)

res <- tune_grid(two_class_wflow, rs = data_folds, grid = two_class_grid,
                 perf = class_metrics, control = grid_control(verbose = TRUE))


# all_pred <-
#   res %>%
#   select(starts_with("id"), .predictions) %>%
#   unnest() %>%
#   nest(-K, -weight_func, -exponent)


summarize(res) %>% filter(.metric == "roc_auc") %>% arrange(desc(mean))

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
    rs = data_folds,
    param_info = two_class_set,
    initial = res,
    objective = conf_bound(kappa = decr_kappa),
    perf = class_metrics,
    iter = 30,
    control = Bayes_control(verbose = TRUE, uncertain = 5)
  )

ggplot(svm_search %>%  summarize() %>% filter(.metric == "roc_auc")) +
  aes(x = K, y = exponent, col = weight_func, size = mean) +
  geom_point(alpha = .7)

