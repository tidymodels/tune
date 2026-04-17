# Extract resample weights from rset or tuning objects

This function provides a consistent interface to access resample weights
regardless of whether they were added to an rset object or are stored in
`tune_results` after tuning.

## Usage

``` r
extract_resample_weights(x)
```

## Arguments

- x:

  An rset object with resample weights, or a `tune_results` object.

## Value

A numeric vector of resample weights, or NULL if no weights are present.

## Examples

``` r
if (FALSE) { # \dontrun{
library(rsample)
folds <- vfold_cv(mtcars, v = 3)
weighted_folds <- add_resample_weights(folds, c(0.2, 0.3, 0.5))
extract_resample_weights(weighted_folds)
} # }
```
