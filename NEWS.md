# tune 0.1.0.9000

## Breaking Changes

* `autoplot.tune_results()` not requires objects made by version 0.1.0 or higher of tune. 

* `tune` objects no longer keep the `rset` class that they have from the `resamples` argument. 

## Other Changes

* `autoplot.tune_results()` now produces a different plot when the tuning grid is a regular grid (i.e. factorial or nearly factorial in nature). If there are 5+ parameters, the standard plot is produced. Non-regular grids are plotted in the same way (although see next bullet point). See `?autoplot.tune_results` for more information.

* `autoplot.tune_results()` now transforms the parameter values for the plot. For example, if the `penalty` parameter was used for a regularized regression, the points are plotted on the log-10 scale (its default transformation). For non-regular grids, the facet labels show the transformation type (e.g. `"penalty (log-10)"` or `"cost (log-2)"`). For regular grid, the x-axis is scaled using `scale_x_continuous()`. 

* Finally, `autoplot.tune_results()` now shows the parameter _labels_ in a plot. For example, if a k-nearest neighbors model was used with `neighbors = tune()`, the parameter will be labeled as `"# Nearest Neighbors"`. When an ID was used, such as `neighbors = tune("K")`, this is used to identify the parameter. 

* In other plotting news, `coord_obs_pred()` has been included for regression models. When plotting the observed and predicted values from a model, this forces the x- and y-axis to be the same range and uses an aspect ratio of 1. 

* The outcome names are saved in an attribute called `outcomes` to objects with class `tune_results`. Also, several accessor functions (named `.get_tune_*()) were added to more easily access such attributes.  

* `conf_mat_resampled()` computes the average confusion matrix across resampling statistics for a single model.  

* `show_best()`, and the `select_*()` functions will now use the first metric in the metric set if no metric is supplied. 

* `filter_parameters()` can trim the `.metrics` column of unwanted results (as well as columns `.predictions` and `.extracts`) from `tune_*` objects. 

* In concert with `dials` > 0.0.7, tuning engine-specific arguments is possible. Many known engine-specific tuning parameters and handled automatically. 

* If a grid is given, parameters do not need to be finalized to be used in the `tune_*()` functions. 

* Added a `save_workflow` argument to `control_*` functions that will result in the
workflow object used to carry out tuning/fitting (regardless of whether a formula 
or recipe was given as input to the function) to be appended to the resulting
`tune_results` object in a `workflow` attribute. The new `.get_tune_workflow()`
function can be used to access the workflow.


# tune 0.1.0

## Breaking Changes

* The arguments to the main tuning/fitting functions (`tune_grid()`, `tune_bayes()`, etc) have been reordered to better align with parsnip's `fit()`. The first argument to all these functions is now a model specification or model workflow. The previous versions are soft-deprecated as of 0.1.0 and will be deprecated as of 0.1.2.

## Other Changes

* Added more packages to be fully loaded in the workers when run in parallel using `doParallel` (#157),  (#159), and  (#160)

* `collect_predictions()` gains two new arguments. `parameters` allows for pre-filtering of the hold-out predictions by tuning parameters values. If you are only interested in one sub-model, this makes things much faster. The other option is `summarize` and is used when the resampling method has training set rows that are predicted in multiple holdout sets.  

* `select_best()`, `select_by_one_std_err()`, and `select_by_pct_loss()` no longer have a redundant `maximize` argument (#176). Each metric set in yardstick now has a direction (maximize vs. minimize) built in.

## Bug Fixes

* `tune_bayes()` no longer errors with a recipe, which has tuning parameters, in combination with a parameter set, where the defaults contain unknown values (#168).

# tune 0.0.1

* CRAN release.

* Changed license to MIT

# tune 0.0.0.9002

* The `...` arguments of `tune_grid()` and `tune_bayes()` have been moved
  forward to force optional arguments to be named.

* New `fit_resamples()` for fitting a set of resamples that don't require any
  tuning.

* Changed `summarise.tune_results()` back to `estimate.tune_results()`

# tune 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
