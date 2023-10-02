rcv_results <- readRDS(test_path("data", "rcv_results.rds"))

opt <- getOption("dplyr.summarise.inform", default = "FALSE")
options(dplyr.summarise.inform = FALSE)

compl <-
  unnest(rcv_results, .metrics) %>%
  group_by(deg_free, degree, `wt df`, `wt degree`, .config, .metric, .estimator) %>%
  summarize(
    mean = mean(.estimate, na.rm = TRUE),
    n = sum(!is.na(.estimator)),
    std_err = sd(.estimate, na.rm = TRUE) / sqrt(n)
  ) %>%
  ungroup() %>%
  arrange(.config)

options(dplyr.summarise.inform = opt)

test_that("estimate method", {
  expect_equal(
    collect_metrics(rcv_results)[, names(compl)] %>% arrange(.config),
    compl
  )
})

test_that("estimate method (with apparent resample)", {
  skip_on_cran()
  skip_if_not_installed("kknn")

  library(parsnip)
  library(rsample)
  library(yardstick)

  m_set <- metric_set(rmse)

  res <-
    fit_resamples(
      nearest_neighbor("regression"),
      mpg ~ cyl + hp,
      bootstraps(mtcars, 5, apparent = TRUE),
      metrics = m_set,
      control = control_grid(save_pred = TRUE)
    )

  collected_sum <-
    collect_metrics(res) %>%
    select(mean, n, std_err)

  collected_manual <-
    res %>%
    dplyr::filter(id != "Apparent") %>%
    tidyr::unnest(.metrics) %>%
    summarize(
      mean = mean(.estimate),
      n = sum(!is.na(.estimator)),
      std_err = sd(.estimate) / sqrt(n)
    )

  expect_equal(collected_sum, collected_manual)
})
