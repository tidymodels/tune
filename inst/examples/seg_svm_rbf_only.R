library(tidymodels)
library(tune)

load(url("http://bit.ly/seg-data"))

theme_set(theme_bw())

library(doMC)
registerDoMC(cores = 20)

# ------------------------------------------------------------------------------

segmentationData <-
  segmentationData %>%
  dplyr::select(-Case, -Cell, -contains("Centroid"))

set.seed(8567)
tr_te_split <- initial_split(segmentationData)

seg_train <- training(tr_te_split)
seg_test  <-  testing(tr_te_split)

folds <- vfold_cv(seg_train, repeats = 10)

# ------------------------------------------------------------------------------

seg_pre_proc <-
  recipe(Class ~ ., data = seg_train) %>%
  step_YeoJohnson(all_predictors()) %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors(), num_comp = tune()) %>%
  step_downsample(Class)

svm_mod <-
  svm_rbf(mode = "classification", cost = tune(), rbf_sigma = tune()) %>%
  set_engine("kernlab")

svm_wflow <-
  workflow() %>%
  add_model(svm_mod) %>%
  add_recipe(seg_pre_proc)

# ------------------------------------------------------------------------------

svm_set <-
  svm_wflow %>%
  parameters() %>%
  update(num_comp = num_comp(c(1, 20)))

grid <- tibble(cost = 10^(-2.75), num_comp = 15,
               rbf_sigma = 10^seq(-8, 0, length = 100))

grid_results <- tune_grid(svm_wflow, resamples = folds, grid = grid,
                          control = control_grid(verbose = TRUE))
summarize(grid_results)

ggplot(
  summarize(grid_results) %>% filter(.metric == "accuracy"),
  aes(x = rbf_sigma, y = mean)) +
  geom_path() +
  scale_x_log10()

# ------------------------------------------------------------------------------

sigma_set <-
  svm_set %>%
  slice(2) %>%
  update(rbf_sigma = rbf_sigma(c(-8, 0)))

sigma_grid <- tibble(rbf_sigma = 10^seq(-8, 0, length = 100))

acc_vals_0 <-
  summarize(grid_results) %>%
  slice(c(80, 125, 150))

# ------------------------------------------------------------------------------
# iteration 1


gp_data_0 <-
  tune:::encode_set(acc_vals_0 %>% select(rbf_sigma), sigma_set) %>%
  set_names("scaled_sigma") %>%
  mutate(
    mean = acc_vals_0$mean,
    rbf_sigma = acc_vals_0$rbf_sigma)

gp_grid <-
  tune:::encode_set(sigma_grid, sigma_set)  %>%
  set_names("scaled_sigma") %>%
  mutate(rbf_sigma = sigma_grid$rbf_sigma)

library(GPfit)
gp_0 <- GP_fit(X = as.matrix(gp_data_0[,1, drop = FALSE]), Y = gp_data_0$mean)
gp_fit_0 <-
  predict(gp_0, as.matrix(gp_grid[,1, drop = FALSE]))$complete_data %>%
  as_tibble() %>%
  setNames(c("scaled_sigma", "mean", "var")) %>%
  mutate(sd = sqrt(var),
         lower = mean - 1 * sd,
         upper = mean + 1 * sd,
         snr = (mean - max(gp_data_0$mean))/sd,
         prob_imp = pnorm(snr)
  ) %>%
  bind_cols(gp_grid %>% select(rbf_sigma))

ggplot(gp_fit_0, aes(x = rbf_sigma)) +
  geom_path(aes(y = mean)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .1) +
  theme_bw() +
  geom_vline(xintercept = gp_data_0$rbf_sigma, lty = 3) +
  geom_point(data = gp_data_0, aes(y = mean)) +
  scale_x_continuous(limits = range(sigma_grid$rbf_sigma), trans = "log10")

