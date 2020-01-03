library(tidymodels)
library(tune)
theme_set(theme_bw())

# ------------------------------------------------------------------------------

simple_rec <- recipe(Class ~ A + B, data = two_class_dat)

spline_rec <-
  simple_rec %>%
  step_ns(A, deg_free = tune("degrees of freedom"))

knn_K <-
  nearest_neighbor(neighbors = tune()) %>%
  set_engine("kknn") %>%
  set_mode("classification")

knn_weights <-
  nearest_neighbor(weight_func = tune()) %>%
  set_engine("kknn") %>%
  set_mode("classification")

knn_two_vars <-
  nearest_neighbor(neighbors = tune(), weight_func = tune()) %>%
  set_engine("kknn") %>%
  set_mode("classification")

knn_three_vars <-
  nearest_neighbor(neighbors = tune(), weight_func = tune(), dist_power = tune()) %>%
  set_engine("kknn") %>%
  set_mode("classification")

knn_no_vars <-
  nearest_neighbor(neighbors = 3) %>%
  set_engine("kknn") %>%
  set_mode("classification")

svm_mod <-
  svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
  set_engine("kernlab") %>%
  set_mode("classification")

one_perf <- metric_set(roc_auc)
two_perf <- metric_set(roc_auc, mcc)

# ------------------------------------------------------------------------------

grid_plot <- function(rec, mod, sfd = TRUE, ...) {
  set.seed(7898)
  data_folds <- vfold_cv(two_class_dat, v = 3)
  wflow <- workflow() %>% add_model(mod) %>% add_recipe(rec)
  pset <- parameters(wflow)
  is_quant <- purrr::map_lgl(pull(pset, object), inherits, "quant_param")

  p <- nrow(pset)
  p_num <- sum( is_quant)
  p_cat <- sum(!is_quant)

  if (sfd) {
    grid <- grid_latin_hypercube(pset, size = 20)
  } else {
    grid <- grid_regular(pset, levels = rep(3, p))
  }
  note <- paste0(p_num, " quant, ", p_cat, " qual, ",
                 ifelse(sfd, "space filling", "regular grid"))

  res <- tune_grid(wflow, resamples = data_folds, grid = grid, ...)
  plot_marginals(res) + ggtitle(note)
}

# ------------------------------------------------------------------------------
# All quantitative parameters:

# One parameter
grid_plot(spline_rec, knn_no_vars, sfd = TRUE)
grid_plot(spline_rec, knn_no_vars, sfd = FALSE, metrics = one_perf)

grid_plot(simple_rec, knn_K, sfd = TRUE)
grid_plot(simple_rec, knn_K, sfd = FALSE, metrics = one_perf)

# Two parameters
grid_plot(simple_rec, svm_mod, sfd = TRUE)
grid_plot(simple_rec, svm_mod, sfd = FALSE, metrics = one_perf)

grid_plot(spline_rec, knn_K, sfd = TRUE)
grid_plot(spline_rec, knn_K, sfd = FALSE, metrics = one_perf)

# Three parameters
grid_plot(spline_rec, svm_mod, sfd = TRUE)
grid_plot(spline_rec, svm_mod, sfd = FALSE, metrics = one_perf)


# ------------------------------------------------------------------------------
# All qualitative parameters (should currently fail)

# One parameter
grid_plot(simple_rec, knn_weights, sfd = TRUE)
grid_plot(simple_rec, knn_weights, sfd = FALSE, metrics = one_perf)

# ------------------------------------------------------------------------------
# Mixed parameters (should currently fail)

# Two parameters
grid_plot(spline_rec, knn_weights, sfd = TRUE)
grid_plot(spline_rec, knn_weights, sfd = FALSE, metrics = one_perf)

# Three parameters
grid_plot(simple_rec, knn_three_vars, sfd = TRUE)
grid_plot(simple_rec, knn_three_vars, sfd = FALSE, metrics = one_perf)

grid_plot(spline_rec, knn_two_vars, sfd = TRUE)
grid_plot(spline_rec, knn_two_vars, sfd = FALSE, metrics = one_perf)

# Four parameters
grid_plot(spline_rec, knn_three_vars, sfd = TRUE)
grid_plot(spline_rec, knn_three_vars, sfd = FALSE, metrics = one_perf)

# ------------------------------------------------------------------------------

set.seed(7898)
data_folds <- vfold_cv(two_class_dat, v = 5)
search_res <- tune_bayes(spline_rec, model = knn_three_vars, resamples = data_folds, iter = 10)
search_res$splits <- lapply(search_res$splits, function(x) list())




