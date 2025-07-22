# ------------------------------------------------------------------------------
# Expected results produced by tune 1.3.0 to compare to later implementations
# This example tunes a model over a single submodel parameter.

library(tidymodels)
library(sessioninfo)

# ------------------------------------------------------------------------------

tidymodels_prefer()
theme_set(theme_bw())
options(pillar.advice = FALSE, pillar.min_title_chars = Inf)

# ------------------------------------------------------------------------------

set.seed(1)
dat <- sim_classification(1000)
rs <- vfold_cv(dat)

# ------------------------------------------------------------------------------

mod <- nearest_neighbor(neighbors = tune(), weight_func = "triangular") |>
  set_mode("classification")

submodel_only_wflow <- workflow(class ~ ., mod)

submodel_only_grid <- tibble(neighbors = 3:10)

submodel_only_res <-
  submodel_only_wflow |>
  tune_grid(
    resamples = rs,
    grid = submodel_only_grid,
    control = control_grid(save_pred = TRUE)
  )

submodel_only_metrics <- collect_metrics(submodel_only_res, summarize = FALSE)
submodel_only_pred <- collect_predictions(submodel_only_res, summarize = FALSE)

# ------------------------------------------------------------------------------

save(submodel_only_metrics, submodel_only_pred, file = "submodel_only_example.RData")

# ------------------------------------------------------------------------------

sessioninfo::session_info()

if (!interactive()) {
  q("no")
}
