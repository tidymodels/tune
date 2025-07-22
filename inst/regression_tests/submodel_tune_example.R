# ------------------------------------------------------------------------------
# Expected results produced by tune 1.3.0 to compare to later implementations
# This example tunes a preprocess and a model. The model has two parameters, one
# of which is a submodel parameter. A regular design was used that is unbalanced
# by removing the first candidate.

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

rec <- recipe(outcome ~ ., data = dat) |>
  step_pca(all_numeric_predictors(), num_comp = tune())

mod <- nearest_neighbor(neighbors = tune("k"), weight_func = tune()) |>
  set_mode("regression")

submodel_wflow <- workflow(rec, mod)

# submodel_grd <-
#   submodel_wflow |>
#   extract_parameter_set_dials() |>
#   update(
#     k = neighbors(c(4, 20)),
#     num_comp = num_comp(c(2, 10))
#   ) |>
#   grid_regular(levels = c(4, 3, 2)) |>
#   slice(-1)

# fmt: skip
submodel_grid <-
  tibble::tribble(
    ~k,   ~weight_func, ~num_comp,
     9L,  "rectangular",        2L,
    14L,  "rectangular",        2L,
    20L,  "rectangular",        2L,
     4L,   "triangular",        2L,
     9L,   "triangular",        2L,
    14L,   "triangular",        2L,
    20L,   "triangular",        2L,
     4L, "epanechnikov",        2L,
     9L, "epanechnikov",        2L,
    14L, "epanechnikov",        2L,
    20L, "epanechnikov",        2L,
     4L,  "rectangular",       10L,
     9L,  "rectangular",       10L,
    14L,  "rectangular",       10L,
    20L,  "rectangular",       10L,
     4L,   "triangular",       10L,
     9L,   "triangular",       10L,
    14L,   "triangular",       10L,
    20L,   "triangular",       10L,
     4L, "epanechnikov",       10L,
     9L, "epanechnikov",       10L,
    14L, "epanechnikov",       10L,
    20L, "epanechnikov",       10L
  )

submodel_res <-
  submodel_wflow |>
  tune_grid(
    resamples = rs,
    grid = submodel_grid,
    control = control_grid(save_pred = TRUE)
  )

submodel_metrics <- collect_metrics(submodel_res, summarize = FALSE)
submodel_pred <- collect_predictions(submodel_res, summarize = FALSE)

# ------------------------------------------------------------------------------

save(submodel_metrics, submodel_pred, file = "submodel_example.RData")

# ------------------------------------------------------------------------------

sessioninfo::session_info()

if (!interactive()) {
  q("no")
}
