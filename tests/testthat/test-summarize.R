context("`summarize` methods")

# ------------------------------------------------------------------------------

library(dplyr)
library(tidyr)
source("../helper-objects.R")
load("rcv_results.RData")

# ------------------------------------------------------------------------------

compl <-
  unnest(rcv_results, .metrics) %>%
  group_by(deg_free, degree, `wt df`, `wt degree`, .metric, .estimator) %>%
  summarize(mean = mean(.estimate, na.rm = TRUE), n = sum(!is.na(.estimator)),
            std_err = sd(.estimate, na.rm = TRUE)/sqrt(n))


test_that('summarize method', {
  expect_equal(summarize(rcv_results), compl)
})
