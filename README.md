
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tune <a href='https://tune.tidymodels.org'><img src='man/figures/logo.png' alt='Package hex logo. A black sticker with technicolor dials representing varying parameter values.' align="right" height="139" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/tidymodels/tune/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tidymodels/tune/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/tidymodels/tune/branch/main/graph/badge.svg)](https://app.codecov.io/gh/tidymodels/tune?branch=main)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![CRAN
status](https://www.r-pkg.org/badges/version/tune)](https://CRAN.R-project.org/package=tune)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

## Overview

The goal of tune is to facilitate hyperparameter tuning for the
tidymodels packages. It relies heavily on
[recipes](https://recipes.tidymodels.org/),
[parsnip](https://parsnip.tidymodels.org/), and
[dials](https://dials.tidymodels.org/).

## Installation

Install from CRAN:

``` r
install.packages("tune", repos = "http://cran.r-project.org") #or your local mirror
```

or you can install the current development version using:

``` r
# install.packages("pak")
pak::pak("tidymodels/tune")
```

## Examples

There are several package vignettes, as well as articles available at
[tidymodels.org](https://www.tidymodels.org/), demonstrating how to use
tune.

Good places to begin include:

- [Getting started with cell segmentation
  data](https://www.tidymodels.org/start/tuning/)
- [Getting started with Ames housing
  data](https://tune.tidymodels.org/articles/getting_started.html)

More advanced resources available are:

- [Basic grid search for an SVM
  model](https://www.tidymodels.org/learn/work/tune-svm/)
- [Iterative Bayesian optimization of a classification
  model](https://www.tidymodels.org/learn/work/bayes-opt/)
- [Advanced text mining
  example](https://tune.tidymodels.org/articles/extras/text_analysis.html)
- [Notes on optimizations and parallel
  processing](https://tune.tidymodels.org/articles/extras/optimizations.html)
- [Details on acquisition function for scoring parameter
  combinations](https://tune.tidymodels.org/articles/acquisition_functions.html)

## Contributing

This project is released with a [Contributor Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

- For questions and discussions about tidymodels packages, modeling, and
  machine learning, please [post on Posit
  Community](https://community.rstudio.com/new-topic?category_id=15&tags=tidymodels,question).

- If you think you have encountered a bug, please [submit an
  issue](https://github.com/tidymodels/tune/issues).

- Either way, learn how to create and share a
  [reprex](https://reprex.tidyverse.org/articles/articles/learn-reprex.html)
  (a minimal, reproducible example), to clearly communicate about your
  code.

- Check out further details on [contributing guidelines for tidymodels
  packages](https://www.tidymodels.org/contribute/) and [how to get
  help](https://www.tidymodels.org/help/).
