test_that("solve10", {

  expect_equal(
    solve10a(example_data_10("a1")),
    4
  )

  expect_equal(
    solve10a(example_data_10("a2")),
    8
  )

  expect_equal(
    solve10b(example_data_10("b1")),
    4
  )

  expect_equal(
    solve10b(example_data_10("b2")),
    8
  )

  expect_equal(
    solve10b(example_data_10("b3")),
    10
  )

})
