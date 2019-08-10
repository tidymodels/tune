context("tune object")

# ------------------------------------------------------------------------------

test_that('tune creates a call', {
  expect_true(is.call(tune()))
  expect_true(is.call(tune(id = "salad")))
  expect_true(is.call(tune("steak")))

})

test_that('tune id value', {
  expect_equal(tune(), call("tune"))
  expect_equal(tune(""), call("tune"))
  expect_equal(tune(id = "salad"), call("tune", "salad"))
  expect_equal(tune("steak"), call("tune", "steak"))
})

test_that('bad tune id', {
  expect_error(tune(2))
  expect_error(tune(NA_character_), "The `id` cannot be NA.")
  expect_error(tune(NULL))
  expect_error(tune(a = "a"), 'unused argument')
})

