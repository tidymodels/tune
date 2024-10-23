library(workflows)
library(dplyr)
library(recipes)
library(parsnip)
library(rsample)
library(yardstick)

test_that("showing notes", {
  skip_if_not_installed("modeldata")
  skip_if(!rankdeficient_version)

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

  skip_if(packageVersion("dplyr") < "1.1.1")
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
  expect_snapshot(show_notes(.Last.tune.result))

  # Get cli lines right
  set.seed(1)
  dat <- modeldata::sim_classification(150, intercept = 15)
  rs <- rsample::vfold_cv(dat)
  expect_snapshot(
    fit_lr <-
      parsnip::logistic_reg() %>%
      fit_resamples(class ~ ., rs)
  )
  expect_snapshot(show_notes(fit_lr))

})

test_that("notes are sorted in the correct order", {
  # set `apparent = TRUE` so that resamples aren't in alphabetical order by id
  mt_boots <- bootstraps(mtcars, 3, apparent = TRUE)

  # induce the size zero yardstick error in Bootstrap1
  mt_boots$splits[[1]]$out_id <- numeric(0)

  suppressMessages({
    mt_res <- fit_resamples(linear_reg(), mpg ~ ., mt_boots)
  })

  boots_1_loc <- which(mt_res$id == "Bootstrap1")
  boots_1_notes <- mt_res$.notes[[boots_1_loc]]
  expect_equal(nrow(boots_1_notes), 1)
})
