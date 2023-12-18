test_that("solve18", {

  expect_equal(
    solve18a(example_data_18()),
    62
  )

  skip("no implementation")
  expect_equal(
    solve18b(example_data_18()),
    952408144115
  )

})
