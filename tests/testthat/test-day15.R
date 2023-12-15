test_that("solve15", {

  expect_equal(
    solve15a(example_data_15()),
    1320
  )

  expect_equal(
    solve15b(example_data_15()),
    145
  )

})
