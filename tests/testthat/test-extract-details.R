test_that("option to extract the split - fit_resamples", {
  skip_if(R.Version()$major < "4")

  spec <- parsnip::linear_reg()
  form <- mpg ~ .
  set.seed(1)
  boots <- rsample::bootstraps(mtcars, 5)
  extract_split <- function(x, split){
    list(x = list(x), split = split$id$id)
  }
  ctrl_fit_split <- control_resamples(extract = extract_split)
  res_fit_split <- fit_resamples(spec, form, boots, control = ctrl_fit_split)

  expect_equal(res_fit_split$.extracts[[1]]$.extracts[[1]]$split, "Bootstrap1")
})
