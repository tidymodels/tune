#' Fine Foods example data
#'
#' @details
#' These data are from Amazon, who describe it as "This dataset consists of
#'  reviews of fine foods from amazon. The data span a period of more than 10
#'  years, including all ~500,000 reviews up to October 2012. Reviews include
#'  product and user information, ratings, and a plaintext review."
#'
#' A subset of the data are contained here and are split into a training and
#'  test set. The training set sampled 10 products and retained all of their
#'  individual reviews. Since the reviews within these products are correlated,
#'  we recommend resampling the data using a leave-one-product-out approach. The
#'  test set sampled 500 products that were not included in the training set
#'  and selected a single review at random for each.
#'
#' There is a column for the product, a column for the text of the review, and
#'  a factor column for a class variable. The outcome is whether the reviewer
#'  gave the product a 5-star rating or not.
#'
#' @name small_fine_foods
#' @aliases small_fine_foods training_data testing_data
#' @docType data
#' @return \item{training_data,testing_data}{tibbles}
#'
#' @source https://snap.stanford.edu/data/web-FineFoods.html
#'
#'
#' @keywords datasets
#' @examples
#' data(small_fine_foods)
#' str(training_data)
NULL

#' Example Analysis of Ames Housing Data
#'
#' @details
#' These objects are the results of an analysis of the Ames
#'  housing data. A K-nearest neighbors model was used with a small
#'  predictor set that included natural spline transformations of
#'  the `Longitude` and `Latitude` predictors. The code used to
#'  generate these examples was:
#' \preformatted{
#' library(tidymodels)
#' library(tune)
#' library(AmesHousing)
#'
#' # ------------------------------------------------------------------------------
#'
#' ames <- make_ames()
#'
#' set.seed(4595)
#' data_split <- initial_split(ames, strata = "Sale_Price")
#'
#' ames_train <- training(data_split)
#'
#' set.seed(2453)
#' rs_splits <- vfold_cv(ames_train, strata = "Sale_Price")
#'
#' # ------------------------------------------------------------------------------
#'
#' ames_rec <-
#'   recipe(Sale_Price ~ ., data = ames_train) %>%
#'   step_log(Sale_Price, base = 10) %>%
#'   step_YeoJohnson(Lot_Area, Gr_Liv_Area) %>%
#'   step_other(Neighborhood, threshold = .1)  %>%
#'   step_dummy(all_nominal()) %>%
#'   step_zv(all_predictors()) %>%
#'   step_ns(Longitude, deg_free = tune("lon")) %>%
#'   step_ns(Latitude, deg_free = tune("lat"))
#'
#' knn_model <-
#'   nearest_neighbor(
#'     mode = "regression",
#'     neighbors = tune("K"),
#'     weight_func = tune(),
#'     dist_power = tune()
#'   ) %>%
#'   set_engine("kknn")
#'
#' ames_wflow <-
#'   workflow() %>%
#'   add_recipe(ames_rec) %>%
#'   add_model(knn_model)
#'
#' ames_set <-
#'   parameters(ames_wflow) %>%
#'   update(K = neighbors(c(1, 50)))
#'
#' set.seed(7014)
#' ames_grid <-
#'   ames_set %>%
#'   grid_max_entropy(size = 10)
#'
#' ames_grid_search <-
#'   tune_grid(
#'     ames_wflow,
#'     resamples = rs_splits,
#'     grid = ames_grid
#'   )
#'
#' set.seed(2082)
#' ames_iter_search <-
#'   tune_bayes(
#'     ames_wflow,
#'     resamples = rs_splits,
#'     param_info = ames_set,
#'     initial = ames_grid_search,
#'     iter = 15
#'   )
#' }
#'
#' __important note__: Since the `rsample` split columns contain a reference
#' to the same data, saving them to disk can results in large object sizes when
#' the object is later used. In essance, R replaces all of those references with
#' the actual data. For this reason, we saved zero-row tibbles in their place.
#' This doesn't affect how we use these objects in examples but be advised that
#' using some `rsample` functions on them will cause issues.
#'
#' @name example_ames_knn
#' @aliases ames_wflow ames_grid_search ames_iter_search
#' @docType data
#' @return \item{ames_wflow}{A workflow object}
#'  \item{ames_grid_search,ames_iter_search}{Results of model tuning. }
#'
#'
#' @keywords datasets
#' @examples
#' ames_grid_search
#' ames_iter_search
NULL



