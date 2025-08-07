library(recipes)
library(parsnip)
library(tailor)

# skip: fmt
two_row_data <-
  tibble::tribble(
    ~A, ~B, ~Class,
    2.06, 1.63, "Class1",
    2.01, 1.03, "Class1"
  )

form <- y ~ .

rec <- recipes::recipe(Class ~ ., data = two_row_data)

rec_pca <- rec |>
  recipes::step_pca(all_numeric_predictors(), num_comp = tune())

lr_spec <- parsnip::logistic_reg()

glmn_lambda_spec <-
  parsnip::logistic_reg(penalty = tune("lambda"), mixture = tune()) |>
  parsnip::set_engine("glmnet")

# engine specific args
mlp_spec <-
  parsnip::mlp(learn_rate = tune(), epochs = tune()) |>
  parsnip::set_engine("brulee", stop_iter = tune()) |>
  parsnip::set_mode("classification")

knn_submod_spec <-
  parsnip::nearest_neighbor(neighbors = tune()) |>
  parsnip::set_mode("classification")

tlr_thresh_half <- tailor::tailor() |> tailor::adjust_probability_threshold(1 / 5)
tlr_thresh_tune <- tailor::tailor() |> tailor::adjust_probability_threshold(threshold = tune())

tbl_1r <- structure(
  list(),
  names = character(0),
  row.names = c(NA, -1L),
  class = c("tbl_df", "tbl", "data.frame")
)

add_empty_post_stage <- function(x) {
  n <- nrow(x)
  dplyr::mutate(x, post_stage = purrr::map(1:n, ~tbl_1r))
}
