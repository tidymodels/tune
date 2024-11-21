test_that("check for no arguments pass to ...", {

  expect_snapshot(
    empty_ellipses(a = 5)
  )
  expect_snapshot(
    empty_ellipses(a = 5, b = 1)
  )
  expect_snapshot(
    empty_ellipses(5)
  )
  expect_snapshot(
    empty_ellipses(1, 5)
  )
  expect_snapshot(
    empty_ellipses(1, second = 2)
  )
})
