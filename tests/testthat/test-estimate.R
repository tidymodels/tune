context("`collect_metrics` methods")

# ------------------------------------------------------------------------------

source(test_path("../helper-objects.R"))
rcv_results <- readRDS(test_path("rcv_results.rds"))

# ------------------------------------------------------------------------------

opt <- getOption("dplyr.summarise.inform", default = "FALSE")
options(dplyr.summarise.inform = FALSE)

compl <-
  unnest(rcv_results, .metrics) %>%
  group_by(deg_free, degree, `wt df`, `wt degree`, .config, .metric, .estimator) %>%
  summarize(mean = mean(.estimate, na.rm = TRUE), n = sum(!is.na(.estimator)),
            std_err = sd(.estimate, na.rm = TRUE)/sqrt(n)) %>%
  ungroup() %>%
  arrange(.config)

options(dplyr.summarise.inform = opt)

test_that('estimate method', {
  expect_equivalent(
    collect_metrics(rcv_results)[, names(compl)] %>% arrange(.config),
    compl
  )
})
