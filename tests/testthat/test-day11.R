test_that("solve11", {

  expect_equal(
    solve11a(example_data_11()),
    374
  )

  expect_equal(
    solve11b(example_data_11()),
    82000210
  )

})
