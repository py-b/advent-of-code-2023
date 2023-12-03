test_that("solve02", {

  expect_equal(
    solve02a(example_data_02()),
    8
  )

  expect_equal(
    solve02b(example_data_02()),
    2286
  )

})
