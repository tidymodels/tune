# Quietly load package namespace

For one or more packages, load the namespace. This is used during
parallel processing since the different parallel backends handle the
package environments differently.

## Usage

``` r
load_pkgs(x, ..., infra = TRUE)
```

## Arguments

- x:

  A character vector of packages.

- infra:

  Should base tidymodels packages be loaded as well?

## Value

An invisible NULL.
