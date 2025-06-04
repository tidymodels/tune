# ------------------------------------------------------------------------------
# Expected results produced by tune 1.3.0 to compare to later implementations
# This "simple" example is for model-only results with no submodels

library(tidymodels)
library(sessioninfo)

# ------------------------------------------------------------------------------

tidymodels_prefer()
theme_set(theme_bw())
options(pillar.advice = FALSE, pillar.min_title_chars = Inf)

# ------------------------------------------------------------------------------

set.seed(1)
dat <- sim_regression(1000)
rs <- vfold_cv(dat)

# ------------------------------------------------------------------------------

mod <- nearest_neighbor(neighbors = 11, weight_func = tune()) |>
  set_mode("regression")

simple_wflow <- workflow(outcome ~ ., mod)

simple_grid <-
  tibble::tribble(
    ~weight_func,
    "rectangular",
    "triangular",
    "epanechnikov"
  )

simple_res <-
  simple_wflow |>
  tune_grid(
    resamples = rs,
    grid = simple_grid,
    control = control_grid(save_pred = TRUE)
  )

simple_metrics <- collect_metrics(simple_res, summarize = FALSE)
simple_pred <- collect_predictions(simple_res, summarize = FALSE)

# ------------------------------------------------------------------------------

save(simple_metrics, simple_pred, file = "simple_example.RData")

# ------------------------------------------------------------------------------

sessioninfo::session_info()

if (!interactive()) {
  q("no")
}
