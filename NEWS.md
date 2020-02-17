# tune 0.0.1.9000

* Added more packages to be fully loaded in the workers when run in parallel using `doParallel` (#157)[https://github.com/tidymodels/tune/issues/157],  (#159)[https://github.com/tidymodels/tune/issues/159], and 
(#160)[https://github.com/tidymodels/tune/issues/160]

* `collect_predictions()` gains two new arguments. `parameters` allows for pre-filtering of the hold-out predictions by tuning parameters values. If you are only interested in one sub-model, this makes things much faster. The other option is `summarize` and is used when the resampling method has training set rows that are predicted in multiple holdout sets.  


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
