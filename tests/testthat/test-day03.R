test_that("solve03", {

  expect_equal(
    solve03a(example_data_03()),
    4361
  )

  expect_equal(
    solve03b(example_data_03()),
    467835
  )

})
