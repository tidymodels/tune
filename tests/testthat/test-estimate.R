rcv_results <- readRDS(test_path("data", "rcv_results.rds"))

opt <- getOption("dplyr.summarise.inform", default = "FALSE")
options(dplyr.summarise.inform = FALSE)

compl <-
  tidyr::unnest(rcv_results, .metrics) %>%
  dplyr::group_by(deg_free, degree, `wt df`, `wt degree`, .config, .metric, .estimator) %>%
  dplyr::summarise(
    mean = mean(.estimate, na.rm = TRUE),
    n = sum(!is.na(.estimator)),
    std_err = sd(.estimate, na.rm = TRUE) / sqrt(n)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(.config)

options(dplyr.summarise.inform = opt)

test_that("estimate method", {
  expect_equal(
    collect_metrics(rcv_results)[, names(compl)] %>% dplyr::arrange(.config),
    compl
  )
})
