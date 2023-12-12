test_that("solve12", {

  expect_equal(
    solve12a(example_data_12()),
    21
  )

  skip("no implementation")
  expect_equal(
    solve12b(example_data_12()),
    525152
  )

})
