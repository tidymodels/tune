# tune 0.0.1.9000

## Breaking Changes

* The arguments to the main tuning/fitting functions (`tune_grid()`, 
  `tune_bayes()`, etc) have been reordered to better align with parsnip's `fit()`.

## Other Changes

* Added more packages to be fully loaded in the workers when run in parallel using `doParallel` (#157)[https://github.com/tidymodels/tune/issues/157],  (#159)[https://github.com/tidymodels/tune/issues/159], and 
(#160)[https://github.com/tidymodels/tune/issues/160]

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
