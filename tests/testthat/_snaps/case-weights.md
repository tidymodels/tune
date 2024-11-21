# gives informative default error

    Code
      .use_case_weights_with_yardstick(1)
    Condition
      Error in `.use_case_weights_with_yardstick()`:
      ! An object with <hardhat_case_weights> was expected, not a number.
      i Define a `.use_case_weights_with_yardstick()` method for this type to declare whether or not these case weights should be passed on to yardstick.
      i See `?.use_case_weights_with_yardstick()` for more information.

# `extract_case_weights()` errors if `col` doesn't exist

    Code
      extract_case_weights(mtcars, wf)
    Condition
      Error in `extract_case_weights()`:
      ! `col` must exist and be a quosure at this point.
      i This is an internal error that was detected in the tune package.
        Please report it at <https://github.com/tidymodels/tune/issues> with a reprex (<https://tidyverse.org/help/>) and the full backtrace.

# `extract_case_weights()` errors if case weights column isn't the right class

    Code
      extract_case_weights(mtcars, wf)
    Condition
      Error in `extract_case_weights()`:
      ! Case weights must be a supported case weights type, as determined by `hardhat::is_case_weights()`.

