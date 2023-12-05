test_that("solve05", {

  expect_equal(
    solve05a(example_data_05()),
    35
  )

  expect_equal(
    solve05b(example_data_05()),
    46
  )

})
