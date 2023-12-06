test_that("solve06", {

  expect_equal(
    solve06a(example_data_06()),
    288
  )

  expect_equal(
    solve06b(example_data_06()),
    71503
  )

})
