# Acquisition function for scoring parameter combinations

These functions can be used to score candidate tuning parameter
combinations as a function of their predicted mean and variation.

## Usage

``` r
prob_improve(trade_off = 0, eps = .Machine$double.eps)

exp_improve(trade_off = 0, eps = .Machine$double.eps)

conf_bound(kappa = 0.1)
```

## Arguments

- trade_off:

  A number or function that describes the trade-off between exploitation
  and exploration. Smaller values favor exploitation.

- eps:

  A small constant to avoid division by zero.

- kappa:

  A positive number (or function) that corresponds to the multiplier of
  the standard deviation in a confidence bound (e.g. 1.96 in
  normal-theory 95 percent confidence intervals). Smaller values lean
  more towards exploitation.

## Value

An object of class `prob_improve`, `exp_improve`, or `conf_bounds` along
with an extra class of `acquisition_function`.

## Details

The acquisition functions often combine the mean and variance
predictions from the Gaussian process model into an objective to be
optimized.

For this documentation, we assume that the metric in question is better
when *maximized* (e.g. accuracy, the coefficient of determination, etc).

The expected improvement of a point `x` is based on the predicted mean
and variation at that point as well as the current best value (denoted
here as `x_b`). The vignette linked below contains the formulas for this
acquisition function. When the `trade_off` parameter is greater than
zero, the acquisition function will down-play the effect of the *mean*
prediction and give more weight to the variation. This has the effect of
searching for new parameter combinations that are in areas that have yet
to be sampled.

Note that for `exp_improve()` and `prob_improve()`, the `trade_off`
value is in the units of the outcome. The functions are parameterized so
that the `trade_off` value should always be non-negative.

The confidence bound function does not take into account the current
best results in the data.

If a function is passed to `exp_improve()` or `prob_improve()`, the
function can have multiple arguments but only the first (the current
iteration number) is given to the function. In other words, the function
argument should have defaults for all but the first argument. See
[`expo_decay()`](https://tune.tidymodels.org/dev/reference/expo_decay.md)
as an example of a function.

## See also

[`tune_bayes()`](https://tune.tidymodels.org/dev/reference/tune_bayes.md),
[`expo_decay()`](https://tune.tidymodels.org/dev/reference/expo_decay.md)

## Examples

``` r
prob_improve()
#> Acquisition Function: probability of improvment
```
