# Control aspects of the last fit process

Control aspects of the last fit process

## Usage

``` r
control_last_fit(verbose = FALSE, event_level = "first", allow_par = FALSE)
```

## Arguments

- verbose:

  A logical for logging results (other than warnings and errors, which
  are always shown) as they are generated during training in a single R
  process. When using most parallel backends, this argument typically
  will not result in any logging. If using a dark IDE theme, some
  logging messages might be hard to see; try setting the
  `tidymodels.dark` option with `options(tidymodels.dark = TRUE)` to
  print lighter colors.

- event_level:

  A single string containing either `"first"` or `"second"`. This
  argument is passed on to yardstick metric functions when any type of
  class prediction is made, and specifies which level of the outcome is
  considered the "event".

- allow_par:

  A logical to allow parallel processing (if a parallel backend is
  registered).

## Details

`control_last_fit()` is a wrapper around
[`control_resamples()`](https://tune.tidymodels.org/dev/reference/control_grid.md)
and is meant to be used with
[`last_fit()`](https://tune.tidymodels.org/dev/reference/last_fit.md).
