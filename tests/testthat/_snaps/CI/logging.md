# interactive logger works (fit_resamples, warning + error)

    Code
      res_fit <- fit_resamples(parsnip::nearest_neighbor("regression", "kknn"),
      Sale_Price ~ ., rsample::vfold_cv(modeldata::ames[, c(72, 40:45)], 5), control = control_resamples(
        extract = function(x) {
          raise_warning()
          raise_error()
        }))
    Message
      > A | warning: ope! yikes.
      > B | error:   AHHhH

# interactive logger works (fit_resamples, rlang warning + error)

    Code
      res_fit <- fit_resamples(parsnip::nearest_neighbor("regression", "kknn"),
      Sale_Price ~ ., rsample::vfold_cv(modeldata::ames[, c(72, 40:45)], 5), control = control_resamples(
        extract = function(x) {
          raise_warning_rl()
          raise_error_rl()
        }))
    Message
      > A | warning: ope! yikes. (but rlang)
      > B | error:   AHHhH (but rlang)

# interactive logger works (fit_resamples, multiline)

    Code
      res_fit <- fit_resamples(parsnip::nearest_neighbor("regression", "kknn"),
      Sale_Price ~ ., rsample::vfold_cv(modeldata::ames[, c(72, 40:45)], 5), control = control_resamples(
        extract = raise_multiline_conditions))
    Message
      > A | warning: hmmm what's happening
      > B | error:   aHHHksdjvndiuf

# interactive logger works (fit_resamples, occasional error)

    Code
      res_fit <- fit_resamples(parsnip::nearest_neighbor("regression", "kknn"),
      Sale_Price ~ ., rsample::vfold_cv(modeldata::ames[, c(72, 40:45)], 5), control = control_resamples(
        extract = later))
    Message
      > A | error:   this errors now! ha!

# interactive logger works (fit_resamples, occasional error + warning)

    Code
      res_fit <- fit_resamples(parsnip::nearest_neighbor("regression", "kknn"),
      Sale_Price ~ ., rsample::vfold_cv(modeldata::ames[, c(72, 40:45)], 10),
      control = control_resamples(extract = function(x) {
        once()
        later()
      }))
    Message
      > A | error:   oh no
      > B | error:   this errors now! ha!

# interactive logger works (fit_resamples, many distinct errors)

    Code
      res_fit <- fit_resamples(parsnip::nearest_neighbor("regression", "kknn"),
      Sale_Price ~ ., rsample::vfold_cv(modeldata::ames[, c(72, 40:45)], 5), control = control_resamples(
        extract = numbered))
    Message
      > A | error:   error number 1
      > B | error:   error number 2
      > C | error:   error number 3
      > D | error:   error number 4
      > E | error:   error number 5

# interactive logger works (tune grid, error)

    Code
      res_fit <- tune_grid(parsnip::nearest_neighbor("regression", "kknn", neighbors = tune()),
      Sale_Price ~ ., rsample::vfold_cv(modeldata::ames[, c(72, 40:45)], 5), grid = 5,
      control = control_grid(extract = raise_error))
    Message
      > A | error:   AHHhH

# interactive logger works (bayesian, error)

    Code
      res_grid <- tune_bayes(parsnip::nearest_neighbor("regression", "kknn",
        neighbors = tune()), Sale_Price ~ ., rsample::vfold_cv(modeldata::ames[, c(72,
        40:45)], 5), initial = 5, iter = 5, control = control_bayes(extract = raise_error))
    Message
      > A | error:   AHHhH

