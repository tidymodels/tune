library(workflows)
library(dplyr)
library(recipes)
library(parsnip)
library(rsample)

test_that("showing notes", {
  skip_if_not_installed("modeldata")
  data(Chicago, package = "modeldata")

  base_wflow <-
    workflow() %>%
    add_model(linear_reg())

  role_rec <-
    recipe(ridership ~ ., data = Chicago) %>%
    step_date(date, id = "step_date") %>%
    update_role(date, new_role = "date") %>%
    update_role_requirements("date", bake = FALSE)

  role_bp_wflow <-
    base_wflow %>%
    add_recipe(role_rec)

  set.seed(1)
  rs <- vfold_cv(Chicago)
  expect_snapshot(res_roles <- role_bp_wflow %>% fit_resamples(rs))
  expect_snapshot(show_notes(res_roles))

  # example with warnings

  simple_rec <-
    recipe(ridership ~ ., data = Chicago) %>%
    step_holiday(date) %>%
    step_date(date, keep_original_cols = FALSE) %>%
    step_dummy(all_nominal_predictors(), one_hot = TRUE)

  simple_wflow <-
    base_wflow %>%
    add_recipe(simple_rec)

  expect_snapshot(res_simple <- simple_wflow %>% fit_resamples(rs))
  expect_snapshot(show_notes(res_simple))

  # nothing to show

  clean_rec <-
    recipe(ridership ~ Austin + Belmont, data = Chicago)

  clean_wflow <-
    base_wflow %>%
    add_recipe(clean_rec)

  res_clean <- clean_wflow %>% fit_resamples(rs)
  expect_snapshot(show_notes(.Last.tune.result
                             ))
})
