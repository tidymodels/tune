% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/param_set.workflows.R
\name{parameters.workflow}
\alias{parameters.workflow}
\alias{parameters.model_spec}
\alias{parameters.recipe}
\title{Determination of parameter sets for other objects}
\usage{
\method{parameters}{workflow}(x, ...)

\method{parameters}{model_spec}(x, ...)

\method{parameters}{recipe}(x, ...)
}
\arguments{
\item{x}{An object}

\item{...}{Not currently used.}
}
\value{
A parameter set object
}
\description{
These methods extend the generic \code{\link[dials:parameters]{dials::parameters()}} to work with more
complex objects, such as recipes, model specifications, and workflows.
}
\examples{
\donttest{
library(tibble)
library(recipes)

recipe(mpg ~ ., data = mtcars) \%>\%
  step_knnimpute(all_predictors(), neighbors = tune()) \%>\%
  step_pca(all_predictors(), num_comp = tune()) \%>\%
  dials::parameters()

 # A peak under the hood
 tibble::as_tibble(.Last.value)

recipe(mpg ~ ., data = mtcars) \%>\%
  step_ns(disp, deg_free = tune("disp df")) \%>\%
  step_ns(wt, deg_free = tune("wt df")) \%>\%
  dials::parameters()

recipe(mpg ~ ., data = mtcars) \%>\%
  step_normalize(all_predictors()) \%>\%
  dials::parameters()

library(parsnip)

boost_tree(trees = tune(), min_n = tune()) \%>\%
  set_engine("xgboost") \%>\%
  dials::parameters()

boost_tree(trees = tune(), min_n = tune()) \%>\%
  set_engine("C5.0", rules = TRUE) \%>\%
  dials::parameters()
}
}
\keyword{internal}