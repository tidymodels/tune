# tune 0.1.0.9000

## Breaking Changes

* `tune` objects no longer keep the `rset` class that they have from the `resamples` argument. 


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
