# Support for parallel processing in tune

tune can enable simultaneous parallel computations. Tierney (2008)
defined different classes of parallel processing techniques:

- *Implicit* is when a function uses low-level tools to perform a
  calculation that is small in scope in parallel. Examples are using
  multithreaded linear algebra libraries (e.g., BLAS) or basic R
  vectorization functions.

- *Explicit* parallelization occurs when the user requests that some
  calculations should be run by generating multiple new R
  (sub)processes. These calculations can be more complex than those for
  implicit parallel processing.

For example, some decision tree libraries can implicitly parallelize
their search for the optimal splitting routine using multiple threads.

Alternatively, if you are resampling a model *B* times, you can
explicitly create *B* new R jobs to train *B* boosted trees in parallel
and return their resampling results to the main R process (e.g.,
[`fit_resamples()`](https://tune.tidymodels.org/dev/reference/fit_resamples.md)).

There are two frameworks that can be used to explicitly parallel process
your work in tune: the
[future](https://future.futureverse.org/reference/future.html) package
and the [mirai](https://mirai.r-lib.org/reference/mirai.html) package.
Previously, you could use the foreach package, but this has been
deprecated as of version 1.2.1 of tune.

By default, no parallelism is used to process models in tune; you have
to opt-in.

### Using future

You should install the package and choose your flavor of parallelism
using the [plan](https://future.futureverse.org/reference/plan.html)
function. This allows you to specify the number of worker processes and
the specific technology to use.

For example, you can use:

       library(future)
       plan(multisession, workers = 4)

and work will be conducted simultaneously (unless there is an exception;
see the section below).

If you had previously used foreach, this would replace your existing
code that probably looked like:

       library(doBackend)
       registerDoBackend(cores = 4)

See
[`future::plan()`](https://future.futureverse.org/reference/plan.html)
for possible options other than `multisession`.

Note that tune resets the *maximum* limit of memory of global variables
(e.g., attached packages) to be greater than the default when the
package is loaded. This value can be altered using
`options(future.globals.maxSize)`.

If you want future to use mirai parallel workers, you can install and
load the future.mirai package.

### Using mirai

To set the specific for parallel processing with mirai, use the
[`mirai::daemons()`](https://mirai.r-lib.org/reference/daemons.html)
function. The first argument, `n`, determines the number of parallel
workers. Using `daemons(0)` reverts to sequential processing.

The arguments `url` and `remote` are used to set up and launch parallel
processes over the network for distributed computing. See
[`mirai::daemons()`](https://mirai.r-lib.org/reference/daemons.html)
documentation for more details.

### Reverting to sequential processing

There are a few times when you might specify that you wish to use
parallel processing, but it will revert to sequential execution:

- Many of the control functions (e.g.
  [`control_grid()`](https://tune.tidymodels.org/dev/reference/control_grid.md))
  have an argument called `allow_par`. If this is set to `FALSE`,
  parallel backends will always be ignored.

- Some packages, such as rJava and keras are not compatible with
  explicit parallelization. If any of these packages are used,
  sequential processing occurs.

- If you specify fewer than two workers, or if there is only a single
  task, the computations will occur sequentially.

### Expectations for reproducibility

We advise that you *always* run
[`set.seed()`](https://rdrr.io/r/base/Random.html) with a seed value
just prior to using a function that uses (or might use) random numbers.
Given this:

- You should expect to get the same results if you run that section of
  code repeatedly, conditional on using version 1.4.0 of tune.

- You should expect differences in results between version 1.4.0 of tune
  and previous versions.

- When using
  [`last_fit()`](https://tune.tidymodels.org/dev/reference/last_fit.md),
  you should be able to get the same results as manually using
  [`generics::fit()`](https://generics.r-lib.org/reference/fit.html) and
  [`predict()`](https://rdrr.io/r/stats/predict.html) to do the same
  work.

- When running with or without parallel processing (using any backend
  package), you should be able to achieve the same results from
  [`fit_resamples()`](https://tune.tidymodels.org/dev/reference/fit_resamples.md)
  and the various tuning functions.

Specific exceptions:

- For SVM classification models using the kernlab package, the random
  number generator is independent of R, and there is no argument to
  control it. Unfortunately, it is likely to give you different results
  from run-to-run.

- For some deep learning packages (e.g., tensorflow, keras, and torch),
  it is very difficult to achieve reproducible results. This is
  especially true when using GPUs for computations. Additionally, we
  have seen differences in computations (stochastic or non-random)
  between platforms due to the packages' use of different numerical
  tolerance constants across operating systems.

### Handling package dependencies

tune knows what packages are required to fit a workflow object.

When computations are run sequentially, an initial check is made to see
if they are installed. This triggers the packages to be loaded but not
visible in the search path.

In parallel, the required packages are fully loaded (i.e., loaded and
seen in the search path), as they were previously with foreach, in the
worker processes (but not the main R session).

## References

<https://www.tmwr.org/grid-search#parallel-processing>

Tierney, Luke. "Implicit and explicit parallel computing in R." COMPSTAT
2008: Proceedings in Computational Statistics. Physica-Verlag HD, 2008.
