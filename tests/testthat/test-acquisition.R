context("acquisition functions")

# ------------------------------------------------------------------------------

library(rlang)
library(dplyr)
source("../helper-objects.R")

test_res <- tibble(.mean = 1:10,  .sd = c((1:9)/10, NA_real_))

dbled <- function(x) x^2

# ------------------------------------------------------------------------------

test_that('conf_bound interface', {
  expect_error(conf_bound("a"))
  expect_error(conf_bound(function() 1))
  expect_error(predict(conf_bound(), test_res, maximize = 2, iter = 1))
})

test_that('conf_bound calculations', {
  expect_equal(
    predict(conf_bound(kappa = 1), test_res, maximize = TRUE, iter = 1),
    test_res %>% mutate(objective = .mean - 1 * .sd) %>% select(objective)
  )
  expect_equal(
    predict(conf_bound(2), test_res, maximize = TRUE, iter = 1),
    test_res %>% mutate(objective = .mean - 2 * .sd) %>% select(objective)
  )
  expect_equal(
    predict(conf_bound(dbled), test_res, maximize = TRUE, iter = 2),
    test_res %>% mutate(objective = .mean - 4 * .sd) %>% select(objective)
  )
  expect_equal(
    predict(conf_bound(kappa = 1), test_res, maximize = FALSE, iter = 1),
    test_res %>% mutate(objective = .mean + 1 * .sd) %>% select(objective)
  )
})


# ------------------------------------------------------------------------------

test_that('prob_improve interface', {
  expect_error(prob_improve("a"))
  expect_error(prob_improve(function() 1))
  expect_error(predict(prob_improve(), test_res, maximize = 2, iter = 1))
  expect_error(predict(prob_improve(), test_res, maximize = TRUE, iter = 1, best = NA))
  expect_error(predict(prob_improve(), test_res, maximize = TRUE, iter = 1, best = "WAT"))
})

test_that('prob_improve calculations', {
  expect_equal(
    predict(prob_improve(), test_res, maximize = TRUE, iter = 1, best = 15),
    test_res %>%
      mutate(objective = pnorm((.mean - 15)/.sd)) %>%
      select(objective)
  )
  expect_equal(
    predict(prob_improve(), test_res, maximize = FALSE, iter = 1, best = -2),
    test_res %>%
      mutate(objective = pnorm((-2 - .mean)/.sd)) %>%
      select(objective)
  )

  expect_equal(
    predict(prob_improve(.1), test_res, maximize = TRUE, iter = 1, best = 15),
    test_res %>%
      mutate(objective = pnorm((.mean - 15 - .1)/.sd)) %>%
      select(objective)
  )
  expect_equal(
    predict(prob_improve(.1), test_res, maximize = FALSE, iter = 1, best = -2),
    test_res %>%
      mutate(objective = pnorm((-2 + .1 - .mean)/.sd)) %>%
      select(objective)
  )

  expect_equal(
    predict(prob_improve(dbled), test_res, maximize = TRUE, iter = 2, best = 15),
    test_res %>%
      mutate(objective = pnorm((.mean - 15 - 4)/.sd)) %>%
      select(objective)
  )
  expect_equal(
    predict(prob_improve(dbled), test_res, maximize = FALSE, iter = 4, best = -2),
    test_res %>%
      mutate(objective = pnorm((-2 + 16 - .mean)/.sd)) %>%
      select(objective)
  )
})


# ------------------------------------------------------------------------------

test_that('exp_improve interface', {
  expect_error(exp_improve("a"))
  expect_error(exp_improve(function() 2))
  expect_error(predict(exp_improve(), test_res, maximize = 2, iter = 1))
  expect_error(predict(exp_improve(), test_res, maximize = TRUE, iter = 1, best = NA))
  expect_error(predict(exp_improve(), test_res, maximize = TRUE, iter = 1, best = "WAT"))
})

test_that('exp_improve calculations', {
  expect_equal(
    predict(exp_improve(), test_res, maximize = TRUE, iter = 1, best = 15),
    test_res %>%
      mutate(
        diff = .mean - 15,
        diff = ifelse(diff < 0, 0, diff),
        objective = (diff * pnorm(diff/.sd)) + (.sd * dnorm(diff/.sd))
      ) %>%
      select(objective)
  )
  expect_equal(
    predict(exp_improve(), test_res, maximize = FALSE, iter = 1, best = 15),
    test_res %>%
      mutate(
        diff = 15 - .mean,
        diff = ifelse(diff < 0, 0, diff),
        objective = (diff * pnorm(diff/.sd)) + (.sd * dnorm(diff/.sd))
      ) %>%
      select(objective)
  )

  expect_equal(
    predict(exp_improve(1), test_res, maximize = TRUE, iter = 1, best = 15),
    test_res %>%
      mutate(
        diff = .mean - 16,
        diff = ifelse(diff < 0, 0, diff),
        objective = (diff * pnorm(diff/.sd)) + (.sd * dnorm(diff/.sd))
      ) %>%
      select(objective)
  )
  expect_equal(
    predict(exp_improve(1), test_res, maximize = FALSE, iter = 1, best = 15),
    test_res %>%
      mutate(
        diff = 16 - .mean,
        diff = ifelse(diff < 0, 0, diff),
        objective = (diff * pnorm(diff/.sd)) + (.sd * dnorm(diff/.sd))
      ) %>%
      select(objective)
  )


  expect_equal(
    predict(exp_improve(dbled), test_res, maximize = TRUE, iter = 2, best = 15),
    test_res %>%
      mutate(
        diff = .mean - 19,
        diff = ifelse(diff < 0, 0, diff),
        objective = (diff * pnorm(diff/.sd)) + (.sd * dnorm(diff/.sd))
      ) %>%
      select(objective)
  )
  expect_equal(
    predict(exp_improve(dbled), test_res, maximize = FALSE, iter = 2, best = 15),
    test_res %>%
      mutate(
        diff = 19 - .mean,
        diff = ifelse(diff < 0, 0, diff),
        objective = (diff * pnorm(diff/.sd)) + (.sd * dnorm(diff/.sd))
      ) %>%
      select(objective)
  )
})

