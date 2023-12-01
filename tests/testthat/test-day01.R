test_that("solve01", {

  expect_equal(
    solve01a(example_data_01("a")),
    142
  )

  expect_equal(
    solve01b(example_data_01("b")),
    281
  )

})
