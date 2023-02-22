library(tidymodels)
library(testthat)
library(finetune)

skip_if(tune:::allow_parallelism(FALSE), "Will not catalog: parallelism is enabled")
skip_if(packageVersion("finetune") < "1.0.1.9002")

# data setup ------------------------------------------------------------------
ames_narrow <- modeldata::ames[, c(72, 40:45)]
spec_dt <- parsnip::decision_tree(mode = "regression")
spec_dt_tune <- parsnip::decision_tree(
  cost_complexity = tune(), min_n = tune(), mode = "regression"
)
form <- Sale_Price ~ .
set.seed(1)
folds <- rsample::vfold_cv(ames_narrow, 10)

# set up functions to error / warn deterministically --------------------------
raise_warning <- function(x) {warning("ope! yikes.")}
raise_warning_rl <- function(x) {rlang::warn("ope! yikes. (but rlang)")}
raise_error <- function(x) {stop("AHHhH")}
raise_error_rl <- function(x) {rlang::abort("AHHhH (but rlang)")}

raise_error_once <- function() {local({
  first <- TRUE
  function(x) {
    if (first) {
      first <<- FALSE
      stop("oh no")
    }

    "hi"
  }
})}

raise_error_later <- function() {local({
  count <- 0
  function(x) {
    count <<- count + 1
    if (count > 3) {
      stop("this errors now! ha!")
    }
    "hi"
  }
})}

raise_error_numbered <- function() {local({
  count <- 0
  function(x) {
    count <<- count + 1
    stop(paste0("error number ", count))
    "hi"
  }
})}

# run tuning processes with known errors ---------------------------------------
# for each test case, ensure that each issue is allotted a unique and minimal
# number that's counted correctly in the final summary. the expected
# output will be left in comments below each tuning call.
res_fit <-
  fit_resamples(spec_dt, form, folds,
                control = control_resamples(
                  extract = function(x) {raise_warning(); raise_error()})
  )
#> → A | warning: ope! yikes.
#> → B | error: AHHhH
#> There were issues with some computations   A: x10   B: x10

res_fit <-
  fit_resamples(spec_dt, form, folds,
                control = control_resamples(
                  extract = function(x) {raise_warning_rl(); raise_error_rl()})
  )
#> → A | warning: ope! yikes. (but rlang)
#> → B | error: AHHhH (but rlang)
#> There were issues with some computations   A: x10   B: x10

res_fit <-
  fit_resamples(spec_dt, form, folds,
                control = control_resamples(extract = raise_error_later()))
#> → A | error: this errors now! ha!
#> There were issues with some computations   B: x7

once <- raise_error_once()
later <- raise_error_later()
res_fit <-
  fit_resamples(spec_dt, form, folds,
                control = control_resamples(extract = function(x) {once(); later()}))
#> → A | error: oh no
#> → B | error: this errors now! ha!
#> There were issues with some computations   A: x1   B: x6

res_fit <-
  fit_resamples(spec_dt, form, folds,
                control = control_resamples(extract = raise_error_numbered()))

#> → A | error: error number 1
#> → B | error: error number 2
#> → C | error: error number 3
#> → D | error: error number 4
#> → E | error: error number 5
#> → F | error: error number 6
#> → G | error: error number 7
#> → H | error: error number 8
#> → I | error: error number 9
#> → J | error: error number 10
#> There were issues with some computations   A: x1   B: x1   C: x1   D: x1   E: x…

set.seed(1)
res_grid <-
  tune_grid(spec_dt_tune, form, folds, grid = 5,
            control = control_grid(extract = raise_error))
#> → A | error: AHHhH
#> There were issues with some computations   A: x50

set.seed(1)
res_grid <-
  tune_bayes(spec_dt_tune, form, folds, initial = 5, iter = 5,
             control = control_bayes(extract = raise_error))
#> → A | error: AHHhH
#> There were issues with some computations   A: x100

set.seed(1)
res_grid <-
  tune_bayes(spec_dt_tune, form, folds, iter = 5,
             control = control_bayes(extract = raise_error),
             initial = res_grid)
#> → A | error: AHHhH
#> There were issues with some computations   A: x50

set.seed(1)
res_anova <-
  tune_race_anova(
    spec_dt_tune,
    form,
    resamples = folds,
    control = control_race(extract = function(x) {raise_warning(); raise_error()})
  )
#> → A | warning: ope! yikes.
#> → B | error: AHHhH
#> There were issues with some computations   A: x58   B: x58

set.seed(1)
res_sa <-
  tune_sim_anneal(
    spec_dt_tune,
    form,
    resamples = folds,
    initial = res_anova,
    metrics = metric_set(rmse),
    iter = 15,
    control = control_sim_anneal(verbose_iter = FALSE,
                                 extract = function(x) {raise_warning(); raise_error()})
  )
#> → A | warning: ope! yikes.
#> → B | error: AHHhH
#> There were issues with some computations   A: x150   B: x149
