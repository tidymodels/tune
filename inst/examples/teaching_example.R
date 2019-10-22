library(tidymodels)
library(tune)
library(GPfit)
library(gridExtra)

load(url("http://bit.ly/seg-data"))

thm <-
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),
    legend.position = "top",
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = "transparent", colour = NA)
  )
theme_set(thm)

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

folds <- vfold_cv(seg_train, repeats = 3)

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
  update(num_comp = num_comp(c(1, 20)), rbf_sigma = rbf_sigma(c(-3, 0)))

set.seed(354)
grid <-
  grid_max_entropy(svm_set, size = 150) %>%
  mutate(cost = 10^(-2.75), num_comp = 15)

grid_results <- tune_grid(svm_wflow, resamples = folds, grid = grid,
                          control = ctrl_grid(verbose = TRUE))
estimate(grid_results)

ggplot(
  estimate(grid_results) %>% filter(.metric == "accuracy"),
  aes(x = rbf_sigma, y = mean)) +
  geom_path() +
  scale_x_log10()

# ------------------------------------------------------------------------------

sigma_set <-
  svm_set %>%
  slice(2)

acc_results <-
  estimate(grid_results) %>%
  filter(.metric == "accuracy") %>%
  arrange(rbf_sigma)

acc_vals_1 <-
  acc_results %>%
  slice(c(50, 80, 120))

ggplot(
  acc_results,
  aes(x = rbf_sigma, y = mean)) +
  geom_path() +
  scale_x_log10() +
  geom_vline(xintercept = acc_vals_1$rbf_sigma, lty = 3)

# ------------------------------------------------------------------------------

gp_iter <- function(.data, pset, objective) {
  sigma_grid <- tibble(rbf_sigma = 10^seq(-3, 0, length = 500))

  if (inherits(objective, "conf_bound")) {
    const <- objective$kappa
  } else {
    const <- qnorm(0.975)
  }

  gp_data_1 <-
    tune:::encode_set(.data %>% select(rbf_sigma), pset) %>%
    set_names("scaled_sigma") %>%
    bind_cols(.data %>% select(mean, rbf_sigma))

  gp_grid_1 <-
    tune:::encode_set(sigma_grid, pset)  %>%
    set_names("scaled_sigma") %>%
    mutate(rbf_sigma = sigma_grid$rbf_sigma)

  gp_1 <- GP_fit(X = as.matrix(gp_data_1[,1, drop = FALSE]), Y = gp_data_1$mean)
  gp_fit_1 <-
    predict(gp_1, as.matrix(gp_grid_1[,1, drop = FALSE]))$complete_data %>%
    as_tibble() %>%
    setNames(c("scaled_sigma", ".mean", "var")) %>%
    mutate(.sd = sqrt(var)) %>%
    bind_cols(gp_grid_1 %>% select(rbf_sigma))

  gp_fit_1 <-
    gp_fit_1 %>%
    bind_cols(
      predict(
        objective,
        gp_fit_1,
        maximize = TRUE,
        iter = 1,
        best = max(.data$mean)
      )
    ) %>%
    mutate(
      objective = ifelse(objective < 0, 0, objective),
      lower = .mean - const * .sd,
      upper = .mean + const * .sd
    )
  gp_fit_1
}

# ------------------------------------------------------------------------------

acc_vals <-
  acc_results %>%
  slice(c(80, 110, 140))

sigma_grid <- tibble(rbf_sigma = 10^seq(-3, 0, length = 500))


p0 <-
  ggplot(acc_vals, aes(x = rbf_sigma, y = mean)) +
  geom_point()  +
  ylab("Accuracy") +
  ggtitle("Initial Grid") +
  scale_x_continuous(limits = range(sigma_grid$rbf_sigma), trans = "log10")  +
  xlab("Parameter")

svg("~/tmp/initial.svg", height = 4)
plot(p0)
dev.off()

initial_result <- acc_vals


obj <- prob_improve(trade_off = 0)

for (iter in 1:25) {

  results <- gp_iter(acc_vals, sigma_set, obj)

  p_upper <-
    ggplot(results, aes(x = rbf_sigma)) +
    geom_path(aes(y = .mean)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .1, fill = "blue") +
    geom_point(data = acc_vals, aes(y = mean)) +
    scale_x_continuous(limits = range(results$rbf_sigma), trans = "log10") +
    ylab("Accuracy") +
    ggtitle(paste("Iteration", iter)) +
    xlab("Parameter")

  min_y <-
    acc_vals %>%
    mutate(y = min(results$objective))

  p_lower <-
    ggplot(results, aes(x = rbf_sigma)) +
    geom_path(aes(y = objective)) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    geom_point(data = min_y, aes(x = rbf_sigma, y = y)) +
    scale_x_continuous(limits = range(results$rbf_sigma), trans = "log10") +
    ylab(obj$label)  +
    xlab("Parameter")

  gA <- ggplotGrob(p_upper)
  gB <- ggplotGrob(p_lower)
  maxWidth <- grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])
  gA$widths[2:5] <- as.list(maxWidth)
  gB$widths[2:5] <- as.list(maxWidth)
  grid.arrange(gA, gB, ncol = 1)

  if (iter == 1) {
    iter_01 <- list(top = p_upper, bottom = p_lower, grid = results, dat = acc_vals, y = min_y)
  }
  if (iter == 20) {
    iter_20 <- list(top = p_upper, bottom = p_lower, grid = results, dat = acc_vals, y = min_y)
  }

  best <-
    results %>%
    arrange(desc(objective)) %>%
    slice(1) %>%
    select(best = rbf_sigma) %>%
    cbind(acc_results %>% anti_join(acc_vals %>% select(rbf_sigma), by = "rbf_sigma")) %>%
    mutate(error = abs(best - rbf_sigma)) %>%
    arrange(error) %>%
    slice(1) %>%
    select(rbf_sigma)

  acc_vals <-
    acc_vals %>%
    bind_rows(
      inner_join(acc_results, best, by = "rbf_sigma")
    )
}

# ------------------------------------------------------------------------------

acc_range <-
  c(iter_01$grid$lower, iter_01$grid$upper, iter_01$dat$mean,
    iter_20$grid$lower, iter_20$grid$upper, iter_20$dat$mean)

acc_range <- extendrange(acc_range)

p_upper_01 <-
  ggplot(iter_01$grid, aes(x = rbf_sigma)) +
  geom_path(aes(y = .mean)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .1, fill = "blue") +
  geom_point(data = iter_01$dat, aes(y = mean)) +
  scale_x_continuous(limits = range(iter_01$grid$rbf_sigma), trans = "log10") +
  ylab("Accuracy") +
  ggtitle("Iteration  1") +
  xlab("Parameter")

p_lower_01 <-
  ggplot(iter_01$grid, aes(x = rbf_sigma)) +
  geom_path(aes(y = objective)) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  geom_point(data = iter_01$y, aes(x = rbf_sigma, y = y)) +
  scale_x_continuous(limits = range(iter_01$grid$rbf_sigma), trans = "log10") +
  ylab("Expected Improvement")  +
  xlab("Parameter")

svg("~/tmp/iter_1.svg")
gA_01 <- ggplotGrob(p_upper_01)
gB_01 <- ggplotGrob(p_lower_01)
maxWidth <- grid::unit.pmax(gA_01$widths[2:5], gB_01$widths[2:5])
gA_01$widths[2:5] <- as.list(maxWidth)
gB_01$widths[2:5] <- as.list(maxWidth)
grid.arrange(gA_01, gB_01, ncol = 1)
dev.off()


p_upper_20 <-
  ggplot(iter_20$grid, aes(x = rbf_sigma)) +
  geom_path(aes(y = .mean)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .1, fill = "blue") +
  geom_point(data = iter_20$dat, aes(y = mean)) +
  scale_x_continuous(limits = range(iter_20$grid$rbf_sigma), trans = "log10") +
  ylab("Accuracy") +
  ggtitle("Iteration 20") +
  xlab("Parameter")

p_lower_20 <-
  ggplot(iter_20$grid, aes(x = rbf_sigma)) +
  geom_path(aes(y = objective)) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  geom_point(data = iter_20$y, aes(x = rbf_sigma, y = y)) +
  scale_x_continuous(limits = range(iter_20$grid$rbf_sigma), trans = "log10") +
  ylab("Expected Improvement")  +
  xlab("Parameter")

svg("~/tmp/iter_20.svg")
gA_20 <- ggplotGrob(p_upper_20)
gB_20 <- ggplotGrob(p_lower_20)
maxWidth <- grid::unit.pmax(gA_20$widths[2:5], gB_20$widths[2:5])
gA_20$widths[2:5] <- as.list(maxWidth)
gB_20$widths[2:5] <- as.list(maxWidth)
grid.arrange(gA_20, gB_20, ncol = 1)

dev.off()

