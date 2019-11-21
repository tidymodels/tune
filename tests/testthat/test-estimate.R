context("`collect_metrics` methods")

# ------------------------------------------------------------------------------

library(dplyr)
library(tidyr)
source(test_path("../helper-objects.R"))
load(test_path("rcv_results.RData"))

# ------------------------------------------------------------------------------

compl <-
  unnest(rcv_results, .metrics) %>%
  group_by(deg_free, degree, `wt df`, `wt degree`, .metric, .estimator) %>%
  summarize(mean = mean(.estimate, na.rm = TRUE), n = sum(!is.na(.estimator)),
            std_err = sd(.estimate, na.rm = TRUE)/sqrt(n))


test_that('estimate method', {
  expect_equal(collect_metrics(rcv_results), compl)
})
