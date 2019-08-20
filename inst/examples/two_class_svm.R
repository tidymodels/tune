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

two_class_set <-
  param_set(two_class_wflow) %>%
  update("cost", cost(c(-10, 4)))

set.seed(552)
two_class_grid <-
  two_class_set %>%
  grid_max_entropy(size = 20)

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
    iter = 30,
    control = Bayes_control(verbose = TRUE, random_value = 5)
  )


# ------------------------------------------------------------------------------

svm_model <-
  svm_poly(mode = "classification", cost = tune()) %>%
  set_engine("kernlab")

two_class_wflow <-
  workflow() %>%
  add_recipe(two_class_rec) %>%
  add_model(svm_model)


set.seed(37)
two_class_grid <-
  two_class_set %>%
  grid_random(size = 5)

class_only <- metric_set(accuracy)

grid_res <- tune_grid(two_class_wflow, data_folds, two_class_grid, perf = class_only)

ggplot(estimate(grid_res), aes(x = cost, y = mean)) +
  geom_point() +
  scale_x_log10() +
  theme_bw()

cost_grid <-
  two_class_set %>%
  grid_regular(levels = 100)

acc_vals_0 <- estimate(grid_res)

gp_data_0 <-
  tune:::encode_set(acc_vals_0 %>% select(cost), two_class_set) %>%
  set_names("scaled_cost") %>%
  mutate(
    mean = acc_vals_0$mean,
    cost = acc_vals_0$cost)

gp_grid <-
  tune:::encode_set(cost_grid, two_class_set)  %>%
  set_names("scaled_cost") %>%
  mutate(cost = cost_grid$cost)

library(GPfit)
gp_0 <- GP_fit(X = as.matrix(gp_data_0[,1, drop = FALSE]), Y = gp_data_0$mean)
gp_fit_0 <-
  predict(gp_0, as.matrix(gp_grid[,1, drop = FALSE]))$complete_data %>%
  as_tibble() %>%
  setNames(c("scaled_cost", "mean", "var")) %>%
  mutate(sd = sqrt(var),
         lower = mean - 1 * sd,
         upper = mean + 1 * sd) %>%
  bind_cols(gp_grid %>% select(cost))

ggplot(gp_fit_0, aes(x = cost, y = sd)) +
  geom_path() +
  theme_bw() +
  geom_vline(xintercept = gp_data_0$cost, lty = 3) +
  scale_x_log10()

ggplot(gp_fit_0, aes(x = cost)) +
  geom_path(aes(y = mean)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .1) +
  theme_bw() +
  geom_vline(xintercept = gp_data_0$cost, lty = 3) +
  geom_point(data = gp_data_0, aes(y = mean)) +
  scale_x_log10()


ggplot(gp_fit_0, aes(x = cost, y = lower)) +
  geom_path() +
  theme_bw() +
  geom_vline(xintercept = gp_data_0$cost, lty = 3) +
  scale_x_log10()

# ------------------------------------------------------------------------------

library(tgp)


tgp_0 <- btgp(X = as.matrix(gp_data_0[,1, drop = FALSE]), Z = gp_data_0$mean)
tgp_res_0 <- predict(tgp_0, as.matrix(gp_grid[,1, drop = FALSE]))

tgp_fit_0 <-
  gp_grid %>%
  mutate(
    mean = tgp_res_0$ZZ.mean,
    sd = sqrt(tgp_res_0$ZZ.ks2),
    lower = mean - 1 * sd,
    upper = mean + 1 * sd
  )

ggplot(tgp_fit_0, aes(x = cost, y = sd)) +
  geom_path() +
  theme_bw() +
  geom_vline(xintercept = gp_data_0$cost, lty = 3) +
  scale_x_log10()

ggplot(tgp_fit_0, aes(x = cost)) +
  geom_path(aes(y = mean)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .1) +
  theme_bw() +
  geom_vline(xintercept = gp_data_0$cost, lty = 3) +
  geom_point(data = gp_data_0, aes(y = mean)) +
  scale_x_log10()

